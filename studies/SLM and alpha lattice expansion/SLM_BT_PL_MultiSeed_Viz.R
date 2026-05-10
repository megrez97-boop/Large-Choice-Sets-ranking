# =================================================================================
# BT vs PL Alpha Lattice Simulation Engine (v5.2 - Linter SILENCED Edition)
# Author: Megrez (AI Partner) | Mandate: No-LaTeX Explanations
# =================================================================================

required_packages <- c("evd", "PlackettLuce", "FielDHub", "BradleyTerry2", "ggplot2", "tidyr", "shiny", "parallel", "iterators", "dplyr")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

library(evd)
library(FielDHub)
library(BradleyTerry2)
library(PlackettLuce)
library(ggplot2)
library(parallel)
library(tidyr)
library(shiny)
library(dplyr)

# Helper: Spearman's Footrule
calc_footrule <- function(est_params, true_indices) {
  est_ranks <- rank(-as.numeric(est_params))
  true_ranks <- rank(-as.numeric(true_indices))
  sum(abs(est_ranks - true_ranks))
}

# Core Simulation Engine
run_multi_k_batch <- function(t_val = 120, k_values = c(3, 4, 8, 10), r_limit = 20, 
                              seeds = 1:5, 
                              temp_values = c(0.5, 5.0), error_strategies = c("block"), 
                              maxit = 20, model_selection = "both", cores_free = 2,
                              shiny_progress = NULL) {
  
  task_grid <- expand.grid(k = k_values, r = 2:r_limit, temp = temp_values, strategy = error_strategies, seed_iter = seeds)
  total_tasks <- nrow(task_grid)
  
  # 將所有需要的資料「顯式打包」，避免 Linter 找不到變數
  worker_tasks <- lapply(seq_len(total_tasks), function(idx) {
    list(
      idx = idx,
      task = task_grid[idx, ],
      t_val = t_val,
      maxit = maxit,
      model_selection = model_selection
    )
  })
  
  # 【智能續跑邏輯】
  checkpoint_file <- "sim_checkpoint_temp.rds"
  if (file.exists(checkpoint_file)) {
    results_list <- readRDS(checkpoint_file)
    # 如果備份檔案的大小與目前任務總數不符（例如您改了參數），則重新開始
    if (length(results_list) != total_tasks) {
      results_list <- vector("list", total_tasks)
      cat("\n⚠️ Checkpoint size mismatch. Starting fresh.\n")
    } else {
      cat("\n🔄 Existing checkpoint detected. Resuming...\n")
    }
  } else {
    results_list <- vector("list", total_tasks)
  }
  
  # 找出還沒跑過的任務索引
  to_run_indices <- which(sapply(results_list, is.null))
  
  if (length(to_run_indices) == 0) {
    cat("\n✅ All tasks already completed! Loading results.\n")
  } else {
    cat(sprintf("\n🚀 Running %d remaining tasks out of %d...\n", length(to_run_indices), total_tasks))
    
    num_cores <- max(1, parallel::detectCores() - cores_free) 
    cl <- makeCluster(num_cores)
    
    # 這裡我們用分段方式跑完「剩下的」任務
    chunk_size <- num_cores * 2
    pending_chunks <- split(to_run_indices, ceiling(seq_along(to_run_indices) / chunk_size))
    
    for (i in seq_along(pending_chunks)) {
      curr_indices <- pending_chunks[[i]]
      
      # 執行剩餘任務
      results_list[curr_indices] <- parLapplyLB(cl, worker_tasks[curr_indices], sim_worker)
      
      # 每跑完一段就更新備份
      saveRDS(results_list, checkpoint_file)
      
      # 更新 Shiny 進度
      if (!is.null(shiny_progress)) {
        prog_val <- (total_tasks - length(to_run_indices) + i * chunk_size) / total_tasks
        prog_val <- min(prog_val, 1.0)
        shiny_progress$set(value = prog_val, 
                           detail = sprintf("Resuming: %d/%d tasks total", 
                                           total_tasks - length(to_run_indices) + length(curr_indices), total_tasks))
      }
    }
    stopCluster(cl)
  }
  
  # 全部跑完後更名存檔
  final_backup_name <- sprintf("sim_results_complete_%s.rds", format(Sys.time(), "%m%d_%H%M"))
  if (file.exists(checkpoint_file)) file.rename(checkpoint_file, final_backup_name)
  
  # 合併結果
  final_data <- do.call(rbind, results_list)
  return(final_data)
}

# Shiny UI
ui <- fluidPage(
  tags$head(tags$style(HTML(".shiny-progress { position: fixed; top: 50% !important; left: 50% !important; margin-left: -150px !important; }"))),
  titlePanel("BT vs PL v5.2: Multi-Seed & Band Visualization (Linter SILENCED)"),
  sidebarLayout(
    sidebarPanel(
      numericInput("t_val", "Total Treatments (t):", 120),
      textInput("k_values", "Block Sizes (k):", "3, 4, 8, 10"),
      numericInput("r_limit", "Max Replications (r):", 20),
      numericInput("num_seeds", "Number of Seeds:", 5),
      textInput("temp_values", "Noise Levels:", "0.5, 5.0"),
      checkboxGroupInput("strategies", "Noise Strategies:", 
                         choices = c("Global" = "global", "Block" = "block", "Pairwise" = "comparison"), 
                         selected = c("global", "block", "comparison")),
      selectInput("model_selection", "Models:", choices = c("both", "bt", "pl")),
      numericInput("cores_free", "Cores to Keep Free:", 2),
      hr(),
      actionButton("run_sim", "🚀 Run Simulation", class = "btn-primary btn-lg", width = "100%")
    ),
    mainPanel(
      tabsetPanel(id = "sim_tabs",
        tabPanel("BT Rho", plotOutput("btRhoPlot", height = "600px")),
        tabPanel("BT Footrule", plotOutput("btFootPlot", height = "600px")),
        tabPanel("PL Rho", plotOutput("plRhoPlot", height = "600px")),
        tabPanel("PL Footrule", plotOutput("plFootPlot", height = "600px")),
        tabPanel("Comp (k=3)", plotOutput("compK3Plot", height = "600px")),
        tabPanel("Comp (k=4)", plotOutput("compK4Plot", height = "600px")),
        tabPanel("Comp (k=8)", plotOutput("compK8Plot", height = "600px")),
        tabPanel("Comp (k=10)", plotOutput("compK10Plot", height = "600px"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  sim_results <- eventReactive(input$run_sim, {
    k_vec <- as.numeric(unlist(strsplit(input$k_values, ",")))
    temps <- as.numeric(unlist(strsplit(input$temp_values, ",")))
    
    progress <- shiny::Progress$new()
    progress$set(message = "Simulating Multi-Seed Alpha Lattice...", value = 0)
    on.exit(progress$close())
    
    run_multi_k_batch(
      t_val = input$t_val, k_values = k_vec, r_limit = input$r_limit, 
      seeds = 1:input$num_seeds, temp_values = temps, 
      error_strategies = input$strategies, 
      model_selection = input$model_selection, cores_free = input$cores_free,
      shiny_progress = progress
    )
  })

  render_band_plot <- function(df, col_name, model_label, metric_label) {
    p <- ggplot(df, aes(x = r, y = .data[[col_name]], color = as.factor(k), fill = as.factor(k), group = k)) +
      stat_summary(fun = mean, geom = "line", size = 1.2) +
      stat_summary(fun.data = mean_se, geom = "ribbon", alpha = 0.2, color = NA) +
      facet_grid(temp ~ strategy, labeller = label_both, scales = "free_y") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "bottom") +
      labs(title = sprintf("%s: %s (Mean ± SE Band)", model_label, metric_label),
           x = "Replications (r)", y = metric_label, color = "k", fill = "k")

    if (grepl("Rho", metric_label)) p <- p + scale_y_continuous(limits = c(NA, 1))
    p
  }

  render_comp_band_plot <- function(df, target_k) {
    df_sub <- df[df$k == target_k, ]
    req(nrow(df_sub) > 0)
    
    df_long <- tidyr::pivot_longer(df_sub, cols = c("rho_bt", "rho_pl"), names_to = "Model", values_to = "Rho")
    df_long$Model <- ifelse(df_long$Model == "rho_bt", "Bradley-Terry (BT)", "Plackett-Luce (PL)")

    ggplot(df_long, aes(x = r, y = Rho, color = Model, fill = Model, group = Model)) +
      stat_summary(fun = mean, geom = "line", size = 1.2) +
      stat_summary(fun.data = mean_se, geom = "ribbon", alpha = 0.2, color = NA) +
      facet_grid(temp ~ strategy, labeller = label_both, scales = "free_y") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "bottom") +
      scale_y_continuous(limits = c(NA, 1)) +
      labs(title = sprintf("Model Comparison (k=%s): Spearman Rho", target_k),
           subtitle = "Ribbon represents Mean ± Standard Error",
           x = "Replications (r)", y = "Spearman Rho")
  }

  output$btRhoPlot <- renderPlot({ req(sim_results()); render_band_plot(sim_results(), "rho_bt", "BT", "Spearman Rho") })
  output$btFootPlot <- renderPlot({ req(sim_results()); render_band_plot(sim_results(), "foot_bt", "BT", "Footrule Error") })
  output$plRhoPlot <- renderPlot({ req(sim_results()); render_band_plot(sim_results(), "rho_pl", "PL", "Spearman Rho") })
  output$plFootPlot <- renderPlot({ req(sim_results()); render_band_plot(sim_results(), "foot_pl", "PL", "Footrule Error") })
  
  output$compK3Plot <- renderPlot({ req(sim_results()); render_comp_band_plot(sim_results(), 3) })
  output$compK4Plot <- renderPlot({ req(sim_results()); render_comp_band_plot(sim_results(), 4) })
  output$compK8Plot <- renderPlot({ req(sim_results()); render_comp_band_plot(sim_results(), 8) })
  output$compK10Plot <- renderPlot({ req(sim_results()); render_comp_band_plot(sim_results(), 10) })
}

shinyApp(ui, server)
