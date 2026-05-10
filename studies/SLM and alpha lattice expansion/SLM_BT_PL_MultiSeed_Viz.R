# =================================================================================
# BT vs PL Alpha Lattice Simulation Engine (v5.5 - Scope Fixed)
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

# [Global] Helper: Spearman's Footrule
calc_footrule <- function(est_params, true_indices) {
  est_ranks <- rank(-as.numeric(est_params))
  true_ranks <- rank(-as.numeric(true_indices))
  sum(abs(est_ranks - true_ranks))
}

# [Global] Worker 函數：移至全域以確保平行節點可讀取
sim_worker <- function(args) {
  # 重新加載必要包
  suppressPackageStartupMessages({
    library(FielDHub); library(BradleyTerry2); library(PlackettLuce); library(evd); library(dplyr)
  })
  
  # 解構參數
  task <- args$task
  t_val_local <- args$t_val
  maxit_local <- args$maxit
  model_sel_local <- args$model_selection
  idx_local <- args$idx
  
  current_seed <- task$seed_iter * 1000 + idx_local
  set.seed(current_seed)
  
  # 1. Generate Design
  design <- try(alpha_lattice(t = t_val_local, k = task$k, r = task$r, seed = current_seed), silent = TRUE)
  if (inherits(design, "try-error")) return(NULL) 
  
  fb <- design$fieldBook
  fb$TrueValue <- as.numeric(fb$ENTRY) 
  
  block_tags <- paste(fb$REP, fb$IBLOCK, sep = "_")
  blocks <- split(fb, block_tags)
  
  pl_rankings_matrix <- matrix(NA, nrow = length(blocks), ncol = task$k)
  bt_matches_list <- vector("list", length(blocks))
  
  global_noise <- if (task$strategy == "global") {
    setNames(evd::rgumbel(t_val_local, 0, 1), unique(as.character(fb$ENTRY)))
  } else NULL
  
  # 2. Data Generation
  for (j in seq_along(blocks)) {
    block <- blocks[[j]]
    u_scores <- if (task$strategy == "global") {
      block$TrueValue + task$temp * global_noise[as.character(block$ENTRY)]
    } else if (task$strategy == "block") {
      block$TrueValue + task$temp * evd::rgumbel(nrow(block), 0, 1)
    } else {
      block$TrueValue 
    }
    
    pl_rankings_matrix[j, ] <- as.character(block$TREATMENT[order(-u_scores)])
    
    pm <- combn(as.character(block$TREATMENT), 2)
    if (task$strategy == "comparison") {
      u1 <- block$TrueValue[match(pm[1,], block$TREATMENT)] + task$temp * evd::rgumbel(ncol(pm), 0, 1)
      u2 <- block$TrueValue[match(pm[2,], block$TREATMENT)] + task$temp * evd::rgumbel(ncol(pm), 0, 1)
    } else {
      u1 <- u_scores[match(pm[1,], block$TREATMENT)]
      u2 <- u_scores[match(pm[2,], block$TREATMENT)]
    }
    
    bt_matches_list[[j]] <- data.frame(
      Winner = ifelse(u1 > u2, pm[1,], pm[2,]),
      Loser  = ifelse(u1 > u2, pm[2,], pm[1,]),
      stringsAsFactors = FALSE
    )
  }
  
  bt_matches <- do.call(rbind, bt_matches_list)
  
  res_row <- data.frame(
    k = as.factor(task$k), r = task$r, temp = task$temp, strategy = task$strategy, seed = task$seed_iter,
    rho_bt = NA_real_, foot_bt = NA_real_, time_bt = NA_real_,
    rho_pl = NA_real_, foot_pl = NA_real_, time_pl = NA_real_
  )
  
  # 3. Model Fitting
  if (model_sel_local %in% c("bt", "both")) {
    start_t <- Sys.time()
    all_levels <- as.character(unique(fb$TREATMENT))
    bt_matches$Winner <- factor(bt_matches$Winner, levels = all_levels)
    bt_matches$Loser  <- factor(bt_matches$Loser, levels = all_levels)
    fit_bt <- try(BradleyTerry2::BTm(1, Winner, Loser, data = bt_matches, control = glm.control(maxit = maxit_local)), silent = TRUE)
    if (!inherits(fit_bt, "try-error")) {
      abs_bt <- try(BradleyTerry2::BTabilities(fit_bt), silent = TRUE)
      if (!inherits(abs_bt, "try-error")) {
        item_names <- as.numeric(gsub("[^0-9]", "", rownames(abs_bt)))
        res_row$rho_bt <- cor(abs_bt[,1], item_names, method = "spearman", use = "complete.obs")
        res_row$foot_bt <- sum(abs(rank(-abs_bt[,1]) - rank(-item_names)))
        res_row$time_bt <- as.numeric(difftime(Sys.time(), start_t, units = "secs"))
      }
    }
  }
  
  if (model_sel_local %in% c("pl", "both")) {
    start_t <- Sys.time()
    R <- if (task$strategy == "comparison") {
      try(PlackettLuce::as.rankings(bt_matches[, c("Winner", "Loser")], input = "orderings"), silent = TRUE)
    } else {
      try(PlackettLuce::as.rankings(pl_rankings_matrix, input = "orderings"), silent = TRUE)
    }
    if (!inherits(R, "try-error")) {
      fit_pl <- try(PlackettLuce::PlackettLuce(R, maxit = c(500, 100), epsilon = 1e-6), silent = TRUE)
      if (!inherits(fit_pl, "try-error")) {
        abs_pl <- PlackettLuce::itempar(fit_pl, log = FALSE)
        item_names_pl <- as.numeric(gsub("[^0-9]", "", names(abs_pl)))
        res_row$rho_pl <- cor(as.numeric(abs_pl), item_names_pl, method = "spearman", use = "complete.obs")
        res_row$foot_pl <- sum(abs(rank(-as.numeric(abs_pl)) - rank(-item_names_pl)))
        res_row$time_pl <- as.numeric(difftime(Sys.time(), start_t, units = "secs"))
      }
    }
  }
  return(res_row)
}

# Core Simulation Engine
run_multi_k_batch <- function(t_val = 120, k_values = c(3, 4, 8, 10), r_limit = 20, 
                              seeds = 1:5, 
                              temp_values = c(0.5, 5.0), error_strategies = c("block"), 
                              maxit = 20, model_selection = "both", cores_free = 2,
                              shiny_progress = NULL) {
  
  task_grid <- expand.grid(k = k_values, r = 2:r_limit, temp = temp_values, strategy = error_strategies, seed_iter = seeds)
  total_tasks <- nrow(task_grid)
  
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
    if (length(results_list) != total_tasks) {
      results_list <- vector("list", total_tasks)
      cat("\n⚠️ Checkpoint size mismatch. Starting fresh.\n")
    } else {
      cat("\n🔄 Existing checkpoint detected. Resuming...\n")
    }
  } else {
    results_list <- vector("list", total_tasks)
  }
  
  to_run_indices <- which(sapply(results_list, is.null))
  
  if (length(to_run_indices) == 0) {
    cat("\n✅ All tasks already completed! Loading results.\n")
    if (!is.null(shiny_progress)) shiny_progress$set(value = 1.0, detail = "All tasks completed!")
  } else {
    cat(sprintf("\n🚀 Running %d remaining tasks out of %d...\n", length(to_run_indices), total_tasks))
    
    # 【新增】初始定位：先讓進度條跳到上次中斷的位置
    if (!is.null(shiny_progress)) {
      init_prog <- (total_tasks - length(to_run_indices)) / total_tasks
      shiny_progress$set(value = init_prog, detail = sprintf("Resuming from %.0f%%...", init_prog * 100))
    }
    
    num_cores <- max(1, parallel::detectCores() - cores_free) 
    cl <- makeCluster(num_cores)
    
    clusterExport(cl, varlist = c("sim_worker", "calc_footrule"))
    
    chunk_size <- num_cores * 2
    pending_chunks <- split(to_run_indices, ceiling(seq_along(to_run_indices) / chunk_size))
    
    for (i in seq_along(pending_chunks)) {
      curr_indices <- pending_chunks[[i]]
      
      results_list[curr_indices] <- parLapplyLB(cl, worker_tasks[curr_indices], sim_worker)
      
      saveRDS(results_list, checkpoint_file)
      
      if (!is.null(shiny_progress)) {
        # 計算總體進度：已完成 + 本次已完成
        completed_so_far <- total_tasks - length(to_run_indices) + (i * chunk_size)
        prog_val <- min(completed_so_far / total_tasks, 1.0)
        
        shiny_progress$set(value = prog_val, 
                           detail = sprintf("Progress: %d/%d (%.0f%%)", 
                                           min(completed_so_far, total_tasks), total_tasks, prog_val * 100))
        # 給 UI 一點點時間刷新
        Sys.sleep(0.05)
      }
    }
    stopCluster(cl)
  }
  
  final_backup_name <- sprintf("sim_results_complete_%s.rds", format(Sys.time(), "%m%d_%H%M"))
  if (file.exists(checkpoint_file)) file.rename(checkpoint_file, final_backup_name)
  
  final_data <- do.call(rbind, results_list)
  return(final_data)
}

# Shiny UI
ui <- fluidPage(
  tags$head(tags$style(HTML(".shiny-progress { position: fixed; top: 50% !important; left: 50% !important; margin-left: -150px !important; }"))),
  titlePanel("BT vs PL v5.5: Multi-Seed & Band Visualization (Scope Fixed)"),
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
