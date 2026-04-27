# =================================================================================
# BT vs PL Alpha Lattice Simulation Engine (v4.1 - Full Fixed Edition)
# Author: Megrez (AI Partner)
# =================================================================================

# Load required libraries
packages <- c("evd", "PlackettLuce", "FielDHub", "BradleyTerry2", "ggplot2", "tidyr", "shiny", "parallel", "doSNOW", "foreach", "dplyr")
for (p in packages) { if (!require(p, character.only = TRUE)) install.packages(p) }

library(evd)
library(FielDHub)
library(BradleyTerry2)
library(PlackettLuce)
library(ggplot2)
library(parallel)
library(doSNOW)
library(foreach)
library(tidyr)
library(shiny)
library(dplyr)

# Core Simulation Engine
run_multi_k_batch <- function(t_val = 120, k_values = c(3, 4), r_limit = 20, seed_val = 1006, 
                              temp_values = c(0.5, 5.0), error_strategies = c("block"), 
                              maxit = 20, model_selection = "both", cores_free = 2) {
  
  # Expand task grid to include temps and strategies
  task_grid <- expand.grid(k = k_values, r = 2:r_limit, temp = temp_values, strategy = error_strategies)
  total_tasks <- nrow(task_grid)
  
  # Initialize Parallel Computing Environment
  num_cores <- max(1, parallel::detectCores() - cores_free) 
  cl <- makeCluster(num_cores)
  registerDoSNOW(cl)
  
  cat(sprintf("\n🚀 引擎啟動！保留 %d 核心，使用 %d 核心運算。\n", cores_free, num_cores))
  flush.console()
  
  # Setup Progress Bar
  pb <- txtProgressBar(max = total_tasks, style = 3)
  progress <- function(n) { setTxtProgressBar(pb, n); flush.console() }
  opts <- list(progress = progress)
  
  final_data <- foreach(i = 1:total_tasks, .combine = rbind, 
                        .packages = c("FielDHub", "BradleyTerry2", "PlackettLuce", "evd", "dplyr"),
                        .options.snow = opts) %dopar% {
    
    task <- task_grid[i, ]
    set.seed(seed_val + i)
    
    # 1. Generate Alpha Lattice Design
    design <- try(alpha_lattice(t = t_val, k = task$k, r = task$r, seed = seed_val), silent = TRUE)
    if (inherits(design, "try-error")) return(NULL) 
    
    fb <- design$fieldBook
    fb$TrueValue <- as.numeric(fb$ENTRY) # Higher ENTRY ID = Higher True Value
    
    # Global Noise Pre-calculation (Approach a)
    global_noise <- if (task$strategy == "global") {
      setNames(evd::rgumbel(t_val, 0, 1), unique(as.character(fb$ENTRY)))
    } else {
      NULL
    }
    
    block_tags <- paste(fb$REP, fb$IBLOCK, sep = "_")
    pl_rankings_list <- list()
    bt_matches <- data.frame()
    blocks <- split(fb, block_tags)
    
    # 2. Process Blocks to Generate Data
    for (j in seq_along(blocks)) {
      block <- blocks[[j]]
      
      # Determine Utility Based on Strategy
      if (task$strategy == "global") {
        u_scores <- block$TrueValue + task$temp * global_noise[as.character(block$ENTRY)]
      } else if (task$strategy == "block") {
        u_scores <- block$TrueValue + task$temp * evd::rgumbel(nrow(block), 0, 1)
      } else {
        u_scores <- block$TrueValue # Placeholder for Comparison-level
      }
      
      # --- PL Data Construction ---
      u_for_pl <- if(task$strategy == "comparison") {
        block$TrueValue + task$temp * evd::rgumbel(nrow(block), 0, 1)
      } else {
        u_scores
      }
      observed_order <- block$TREATMENT[order(-u_for_pl)]
      pl_rankings_list[[j]] <- observed_order
      
      # --- BT Data Construction ---
      pm <- combn(block$TREATMENT, 2)
      if (task$strategy == "comparison") {
        # Comparison-level noise: regenerate noise for every single pair
        u1 <- block$TrueValue[match(pm[1,], block$TREATMENT)] + task$temp * evd::rgumbel(ncol(pm), 0, 1)
        u2 <- block$TrueValue[match(pm[2,], block$TREATMENT)] + task$temp * evd::rgumbel(ncol(pm), 0, 1)
      } else {
        # Block or Global level: use the same u_scores for all pairs in this block
        u1 <- u_scores[match(pm[1,], block$TREATMENT)]
        u2 <- u_scores[match(pm[2,], block$TREATMENT)]
      }
      bt_df <- data.frame(
        Winner = ifelse(u1 > u2, pm[1,], pm[2,]),
        Loser  = ifelse(u1 > u2, pm[2,], pm[1,])
      )
      bt_matches <- rbind(bt_matches, bt_df)
    }
    
    res_row <- data.frame(k = as.factor(task$k), r = task$r, temp = task$temp, strategy = task$strategy)
    
    # 3. Model Fitting & Evaluation
    # Bradley-Terry
    if (model_selection %in% c("bt", "both")) {
      bt_matches$Winner <- factor(bt_matches$Winner, levels = unique(as.character(fb$TREATMENT)))
      bt_matches$Loser  <- factor(bt_matches$Loser, levels = unique(as.character(fb$TREATMENT)))
      fit_bt <- try(BradleyTerry2::BTm(1, Winner, Loser, data = bt_matches, control = glm.control(maxit = maxit)), silent = TRUE)
      if (!inherits(fit_bt, "try-error")) {
        abs_bt <- try(BradleyTerry2::BTabilities(fit_bt), silent = TRUE)
        if (!inherits(abs_bt, "try-error")) {
          ids <- as.numeric(gsub("[^0-9]", "", rownames(abs_bt)))
          res_bt <- data.frame(ID = ids, Ab = abs_bt[,1]) %>% dplyr::arrange(ID)
          res_row$rho_bt <- cor(res_bt$Ab, res_bt$ID, method = "spearman")
        }
      }
    }
    
    # Plackett-Luce
    if (model_selection %in% c("pl", "both")) {
      R <- PlackettLuce::as.rankings(pl_rankings_list)
      fit_pl <- try(PlackettLuce::PlackettLuce(R, maxit = c(maxit, 100)), silent = TRUE)
      if (!inherits(fit_pl, "try-error")) {
        abs_pl <- PlackettLuce::itempar(fit_pl, log = FALSE)
        res_pl <- data.frame(ID = as.numeric(names(abs_pl)), Worth = as.numeric(abs_pl)) %>% dplyr::arrange(ID)
        res_row$rho_pl <- cor(res_pl$Worth, res_pl$ID, method = "spearman")
      }
    }
    
    return(res_row)
  }
  
  close(pb)
  stopCluster(cl)
  gc()
  return(final_data)
}

# Shiny UI
ui <- fluidPage(
  titlePanel("BT vs PL v4.1: Auto-Batch Simulation & One-Click Export"),
  sidebarLayout(
    sidebarPanel(
      numericInput("t_val", "Total Treatments (t):", 120),
      textInput("k_values", "Block Sizes (k) [comma separated]:", "3, 4"),
      numericInput("r_limit", "Max Replications (r):", 20),
      textInput("temp_values", "Noise Levels (comma separated):", "0.5, 5, 10"),
      numericInput("seed_val", "Random Seed:", 1006),
      checkboxGroupInput("strategies", "Noise Strategies:", 
                         choices = c("global", "block", "comparison"), selected = c("block")),
      selectInput("model_selection", "Models to Compare:", choices = c("both", "bt", "pl")),
      numericInput("cores_free", "Cores to Keep Free:", 2, min = 1),
      hr(),
      actionButton("run_sim", "🚀 Run Batch Simulation", class = "btn-primary btn-lg", width = "100%"),
      hr(),
      actionButton("save_all", "💾 One-Click Save All Results", class = "btn-success", width = "100%")
    ),
    mainPanel(
      plotOutput("rhoPlot", height = "600px"),
      hr(),
      h4("Batch Summary Table (Top 20)"),
      tableOutput("summaryTable")
    )
  )
)

server <- function(input, output) {
  
  sim_data <- eventReactive(input$run_sim, {
    # Parse inputs
    k_vec <- as.numeric(unlist(strsplit(input$k_values, ",")))
    temps <- as.numeric(unlist(strsplit(input$temp_values, ",")))
    
    # Execute batch simulation
    run_multi_k_batch(
      t_val = input$t_val, 
      k_values = k_vec, 
      r_limit = input$r_limit, 
      seed_val = input$seed_val, 
      temp_values = temps, 
      error_strategies = input$strategies, 
      model_selection = input$model_selection,
      cores_free = input$cores_free
    )
  })
  
  output$rhoPlot <- renderPlot({
    df <- sim_data()
    req(df)
    # Reshape for ggplot
    df_long <- tidyr::pivot_longer(df, cols = starts_with("rho"), names_to = "Model", values_to = "Rho")
    
    ggplot(df_long, aes(x = r, y = Rho, color = interaction(k, Model), linetype = Model)) +
      geom_line(size = 1.2) + geom_point() +
      facet_wrap(~strategy + temp, labeller = label_both) +
      theme_minimal(base_size = 14) +
      labs(title = "Batch Ranking Accuracy Comparison", x = "Replications (r)", y = "Spearman Rho")
  })
  
  output$summaryTable <- renderTable({
    df <- sim_data()
    req(df)
    if ("rho_bt" %in% names(df) && "rho_pl" %in% names(df)) {
      df$PL_Advantage <- df$rho_pl - df$rho_bt
    }
    head(df, 20)
  })
  
  # One-Click Export Logic
  observeEvent(input$save_all, {
    df <- sim_data()
    req(df)
    
    # 1. Save Full CSV
    csv_name <- sprintf("sim_data_t%d_r%d_seed%d.csv", input$t_val, input$r_limit, input$seed_val)
    write.csv(df, csv_name, row.names = FALSE)
    
    # 2. Save Individual Plots
    combos <- df %>% dplyr::select(strategy, temp) %>% unique()
    for(i in 1:nrow(combos)) {
      s <- combos$strategy[i]
      t <- combos$temp[i]
      
      sub_df <- df %>% 
        dplyr::filter(strategy == s, temp == t) %>% 
        tidyr::pivot_longer(cols = starts_with("rho"), names_to = "Model", values_to = "Rho")
      
      p <- ggplot(sub_df, aes(x = r, y = Rho, color = interaction(k, Model), linetype = Model)) +
        geom_line(size = 1.2) + geom_point() +
        theme_minimal() +
        labs(title = sprintf("Sim: k=%s r=%d t=%d noise=%.1f model=%s strat=%s", 
                             input$k_values, input$r_limit, input$t_val, t, input$model_selection, s))
      
      # Auto-naming based on parameters
      file_name <- sprintf("sim_k%s_r%d_t%d_noise%.1f_model%s_strat%s.png", 
                           gsub(",","_",input$k_values), input$r_limit, input$t_val, t, input$model_selection, s)
      
      ggsave(file_name, p, width = 10, height = 6)
    }
    
    showNotification(sprintf("Success! Saved CSV and %d plots.", nrow(combos)), type = "message")
  })
}

shinyApp(ui, server)
