# =================================================================================
# Multi-k Automated Alpha Lattice Simulation
# Footrule vs Rho Plots
# =================================================================================

# Load required libraries
if (!require("evd")) install.packages("evd")
library(evd)
library(FielDHub)
library(BradleyTerry2)
library(ggplot2)
library(parallel)
library(doSNOW)
library(foreach)

# Define core simulation function
run_multi_k_dual_metrics <- function(t_val = 240, k_values = c(2, 3, 4, 5), r_limit = 20, seed_val = 1006, temp = 0.5, judge_method = "ranking", error_location = "rankings", maxit = 20) {
  
  # [Task 1] Set parameter ranges: r & k
  r_steps <- 2:r_limit
  task_grid <- expand.grid(k = k_values, r = r_steps)
  total_tasks <- nrow(task_grid)
  
  # Initialize Parallel Computing Environment
  num_cores <- max(1, parallel::detectCores() - 2) 
  cl <- makeCluster(num_cores)
  registerDoSNOW(cl)
  
  cat(sprintf("\n🚀 雙指標引擎啟動！已徵召 %d 顆核心 (Parallel Engine Started with %d cores).\n", num_cores, num_cores))
  cat(sprintf("模式：%s | 誤差位置：%s | 雜訊等級：%s | Max Iterations: %d\n", judge_method, error_location, temp, maxit))
  cat(sprintf("正在執行 %d 個任務 (Executing %d tasks)...\n\n", total_tasks, total_tasks))
  flush.console() 
  
  # Setup Progress Bar
  pb <- txtProgressBar(max = total_tasks, style = 3)
  progress <- function(n) {
    setTxtProgressBar(pb, n)
    flush.console() 
  }
  opts <- list(progress = progress)
  
  # Calculate Ranking Metrics: Footrule and Rho
  final_data <- foreach(i = 1:total_tasks, 
                        .combine = rbind, 
                        .packages = c("FielDHub", "BradleyTerry2", "evd"),
                        .options.snow = opts) %dopar% {
    
    current_k <- task_grid$k[i]
    current_r <- task_grid$r[i]
    
    # Generate Alpha Lattice Design
    design <- try(alpha_lattice(t = t_val, k = current_k, r = current_r, seed = seed_val), silent = TRUE)
    if (inherits(design, "try-error")) return(NULL) 
    
    fb <- design$fieldBook
    # 設定真實數值 (Set Ground Truth/True Values)
    fb$TrueValue <- (t_val + 1) - as.numeric(fb$ENTRY)
    
    block_tags <- paste(fb$REP, fb$IBLOCK, sep = "_")
    matches <- do.call(rbind, lapply(split(fb, block_tags), function(block) {
      
      # 模式 A: Ranking Error (一組項目只加一次雜訊，邏輯一致)
      u_scores <- block$TrueValue + temp * rgumbel(nrow(block), 0, 1)
      
      if (judge_method == "ranking") {
        # --- Ranking ---
        pairs_mat <- combn(block$TREATMENT, 2)
        
        if (error_location == "rankings") {
          # 抓取預先算好的效用值 (Ranking-level error)
          u1 <- u_scores[match(pairs_mat[1, ], block$TREATMENT)]
          u2 <- u_scores[match(pairs_mat[2, ], block$TREATMENT)]
        } else {
          # 模式 B: Comparison Error (每一對都重新加雜訊，可能自相矛盾)
          # 抓取真實數值後，現場加新的雜訊
          v1 <- block$TrueValue[match(pairs_mat[1, ], block$TREATMENT)]
          v2 <- block$TrueValue[match(pairs_mat[2, ], block$TREATMENT)]
          u1 <- v1 + temp * rgumbel(length(v1), 0, 1)
          u2 <- v2 + temp * rgumbel(length(v2), 0, 1)
        }
        
        winners <- ifelse(u1 > u2, pairs_mat[1, ], pairs_mat[2, ])
        losers  <- ifelse(u1 > u2, pairs_mat[2, ], pairs_mat[1, ])
        return(data.frame(Winner = winners, Loser = losers))
        
      } else if (judge_method == "bws") {
        # --- BWS ---
        # 註：BWS 通常模擬為對整組項目的一次性感知
        if (error_location == "comparisons") {
          # 如果 BWS 也選 comparison error，模擬為選 Best 與選 Worst 時有不同的雜訊感知
          u_best  <- block$TrueValue + temp * rgumbel(nrow(block), 0, 1)
          u_worst <- block$TrueValue + temp * rgumbel(nrow(block), 0, 1)
          best_idx <-  which.max(u_best)
          worst_idx <- which.min(u_worst)
        } else {
          best_idx <-  which.max(u_scores)
          worst_idx <- which.min(u_scores)
        }
        
        best_id  <- block$TREATMENT[best_idx]
        worst_id <- block$TREATMENT[worst_idx]
        
        df_best  <- data.frame(Winner = best_id, Loser = block$TREATMENT[-best_idx])
        df_worst <- data.frame(Winner = block$TREATMENT[-worst_idx], Loser = worst_id)
        
        return(rbind(df_best, df_worst))
      }
    }))
    
    # Ensure Player IDs are factors
    all_players <- unique(c(as.character(matches$Winner), as.character(matches$Loser)))
    matches$Winner <- factor(matches$Winner, levels = all_players)
    matches$Loser  <- factor(matches$Loser, levels = all_players)
    
    # Fit Bradley-Terry Model
    # maxit: Maximum number of iterations for the GLM to converge. 
    # Higher noise levels may require more iterations (e.g., 20-50).
    fit <- suppressWarnings(try(BTm(1, Winner, Loser, data = matches, control = glm.control(maxit = maxit)), silent = TRUE))
    
    if (!inherits(fit, "try-error")) {
      # 提取估計能力值 (Extract Estimated Abilities)
      abilities <- suppressWarnings(try(BTabilities(fit), silent = TRUE))
      if (!inherits(abilities, "try-error")) {
        player_ids <- as.numeric(gsub("[^0-9]", "", rownames(abilities)))
        player_abs <- abilities[, "ability"]
        res_df <- data.frame(ID = player_ids, Ability = player_abs)
        res_df <- res_df[!is.na(res_df$Ability), ]
        
        if (nrow(res_df) > 2) {
          # 計算排名 (Calculate Rankings)
          res_df$TrueRank <- rank(res_df$ID) 
          res_df$PredRank <- rank(-res_df$Ability)
          
          # 1. 計算排名指標 (Calculate Ranking Metrics: Rho and Footrule)
          rho_val <- cor(res_df$PredRank, res_df$TrueRank, method = "spearman")
          footrule_val <- sum(abs(res_df$PredRank - res_df$TrueRank))
          
          return(data.frame(k = as.factor(current_k), 
                            r = current_r, 
                            rho = rho_val, 
                            footrule = footrule_val))
        }
      }
    }
    return(NULL) 
  }
  
  # 關閉平行運算與進度條 (Close Parallel Workers and Progress Bar)
  close(pb)
  stopCluster(cl)
  gc() # [優化] 手動觸發垃圾回收 (Manually trigger Garbage Collection)
  
  return(final_data)
}

# ==========================================
# Interactive Interface
# ==========================================
if (!require("shiny")) install.packages("shiny")
library(shiny)

ui <- fluidPage(
  titlePanel("Alpha Lattice Simulation Tool"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("t_val", "Total Treatments (t):", 240),
      textInput("k_values", "Block Sizes (k) [comma separated]:", "2, 3, 4, 5"),
      numericInput("r_limit", "Max Replications (r):", 20),
      numericInput("seed_val", "Random Seed:", 1006),
      numericInput("temp", "Noise Level (Temperature):", value = 0.5, min = 0, step = 0.1),
      numericInput("maxit", "Max Model Iterations:", value = 20, min = 5, step = 5),
      selectInput("judge_method", "Judging Method:", 
                  choices = c("Ranking (Full Pairwise)" = "ranking", "BWS (Best-Worst Scaling)" = "bws")),
      selectInput("error_location", "Error Added To:", 
                  choices = c("Rankings (Item-level)" = "rankings", "Comparisons (Pair-level)" = "comparisons")),
      hr(),
      actionButton("run_sim", "🚀 Run Simulation", class = "btn-primary btn-lg", width = "100%"),
      hr(),
      h4("Download Results"),
      downloadButton("downloadRho", "Download Rho Plot", class = "btn-success"),
      br(), br(),
      downloadButton("downloadFootrule", "Download Footrule Plot", class = "btn-success")
    ),
    
    mainPanel(
      plotOutput("rhoPlot"),
      hr(),
      plotOutput("footrulePlot")
    )
  )
)

server <- function(input, output) {
  
  # Execute simulation on button click
  sim_results <- eventReactive(input$run_sim, {
    withProgress(message = 'Simulation in progress',
                 detail = 'This may take a while...', value = 0, {
                   
      # 解析 k_values 字串成向量 (Parse k_values string into a vector)
      k_vec <- as.numeric(unlist(strsplit(input$k_values, ",")))
      
      # 設定初始進度條 (Set initial progress)
      setProgress(0.3)
      
      # 執行核心模擬函數 (Execute core simulation function)
      res <- run_multi_k_dual_metrics(
        t_val = input$t_val,
        k_values = k_vec,
        r_limit = input$r_limit,
        seed_val = input$seed_val,
        temp = input$temp,
        judge_method = input$judge_method,
        error_location = input$error_location,
        maxit = input$maxit
      )
      
      setProgress(1)
      return(res)
    })
  })
  
  # Helper to create Rho Plot
  create_rho_plot <- function() {
    req(sim_results())
    ggplot(sim_results(), aes(x = r, y = rho, color = k)) +
      geom_line(linewidth = 1.2) + geom_point(size = 3) +
      labs(title = "Ranking Accuracy (Spearman's Rho)",
           subtitle = sprintf("Method: %s | Error Loc: %s | Noise: %s", input$judge_method, input$error_location, input$temp),
           x = "Replications (r)", y = "Rho (Correlation)") +
      theme_minimal(base_size = 14) + theme(legend.position = "bottom")
  }
  
  # Helper to create Footrule Plot
  create_footrule_plot <- function() {
    req(sim_results())
    ggplot(sim_results(), aes(x = r, y = footrule, color = k)) +
      geom_line(linewidth = 1.2) + geom_point(size = 3) +
      labs(title = "Ranking Error (Spearman's Footrule)",
           subtitle = sprintf("Method: %s | Error Loc: %s | Noise: %s", input$judge_method, input$error_location, input$temp),
           x = "Replications (r)", y = "Total Error Score") +
      theme_minimal(base_size = 14) + theme(legend.position = "bottom")
  }
  
  # Render Plots
  output$rhoPlot <- renderPlot({ create_rho_plot() })
  output$footrulePlot <- renderPlot({ create_footrule_plot() })
  
  # Download Handlers
  output$downloadRho <- downloadHandler(
    filename = function() { sprintf("rho_plot_%s_%s_T%s.png", input$judge_method, input$error_location, input$temp) },
    content = function(file) {
      ggsave(file, plot = create_rho_plot(), device = "png", width = 10, height = 6, dpi = 300)
    }
  )
  
  output$downloadFootrule <- downloadHandler(
    filename = function() { sprintf("footrule_plot_%s_%s_T%s.png", input$judge_method, input$error_location, input$temp) },
    content = function(file) {
      ggsave(file, plot = create_footrule_plot(), device = "png", width = 10, height = 6, dpi = 300)
    }
  )
}

# Launch Shiny App
shinyApp(ui = ui, server = server)
# test for commit 2