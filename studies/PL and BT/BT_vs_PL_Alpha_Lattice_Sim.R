# =================================================================================
# BT vs PL Alpha Lattice Simulation Engine (v4.3 - Real-time Progress Edition)
# Author: Megrez (AI Partner) | Mandate: No-LaTeX Explanations
# =================================================================================

required_packages <- c("evd", "PlackettLuce", "FielDHub", "BradleyTerry2", "ggplot2", "tidyr", "shiny", "parallel", "doSNOW", "foreach", "dplyr")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

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
                              maxit = 20, model_selection = "both", cores_free = 2,
                              shiny_progress = NULL) {
  
  task_grid <- expand.grid(k = k_values, r = 2:r_limit, temp = temp_values, strategy = error_strategies)
  total_tasks <- nrow(task_grid)
  
  num_cores <- max(1, parallel::detectCores() - cores_free) 
  cl <- makeCluster(num_cores)
  registerDoSNOW(cl)
  
  # Progress Bar Setup
  pb <- txtProgressBar(max = total_tasks, style = 3)
  progress <- function(n) {
    setTxtProgressBar(pb, n)
    if (!is.null(shiny_progress)) {
      shiny_progress$set(value = n / total_tasks, 
                         detail = sprintf("Task %d/%d (%.0f%%)", n, total_tasks, (n/total_tasks)*100))
    }
    flush.console()
  }
  opts <- list(progress = progress)
  
  cat(sprintf("\n🚀 Engine started! Parallel tasks: %d\n", total_tasks))
  
  final_data <- foreach(i = 1:total_tasks, .combine = rbind, 
                        .packages = c("FielDHub", "BradleyTerry2", "PlackettLuce", "evd", "dplyr"),
                        .options.snow = opts) %dopar% {
    
    task <- task_grid[i, ]
    set.seed(seed_val + i)
    
    # 1. Generate Design
    design <- try(alpha_lattice(t = t_val, k = task$k, r = task$r, seed = seed_val), silent = TRUE)
    if (inherits(design, "try-error")) return(NULL) 
    
    fb <- design$fieldBook
    fb$TrueValue <- as.numeric(fb$ENTRY) 
    
    # Noise Pre-calculation
    global_noise <- if (task$strategy == "global") {
      setNames(evd::rgumbel(t_val, 0, 1), unique(as.character(fb$ENTRY)))
    } else NULL
    
    block_tags <- paste(fb$REP, fb$IBLOCK, sep = "_")
    blocks <- split(fb, block_tags)
    
    pl_rankings_matrix <- matrix(NA, nrow = length(blocks), ncol = task$k)
    bt_matches_list <- vector("list", length(blocks))
    
    # 2. Data Generation
    for (j in seq_along(blocks)) {
      block <- blocks[[j]]
      u_scores <- if (task$strategy == "global") {
        block$TrueValue + task$temp * global_noise[as.character(block$ENTRY)]
      } else if (task$strategy == "block") {
        block$TrueValue + task$temp * evd::rgumbel(nrow(block), 0, 1)
      } else block$TrueValue
      
      u_for_pl <- if(task$strategy == "comparison") {
        block$TrueValue + task$temp * evd::rgumbel(nrow(block), 0, 1)
      } else u_scores
      
      pl_rankings_matrix[j, ] <- as.character(block$TREATMENT[order(-u_for_pl)])
      
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
    res_row <- data.frame(k = as.factor(task$k), r = task$r, temp = task$temp, strategy = task$strategy)
    
    # 3. Model Fitting
    # BT Model
    if (model_selection %in% c("bt", "both")) {
      start_t <- Sys.time()
      all_levels <- as.character(unique(fb$TREATMENT))
      bt_matches$Winner <- factor(bt_matches$Winner, levels = all_levels)
      bt_matches$Loser  <- factor(bt_matches$Loser, levels = all_levels)
      fit_bt <- try(BradleyTerry2::BTm(1, Winner, Loser, data = bt_matches, control = glm.control(maxit = maxit)), silent = TRUE)
      if (!inherits(fit_bt, "try-error")) {
        abs_bt <- try(BradleyTerry2::BTabilities(fit_bt), silent = TRUE)
        if (!inherits(abs_bt, "try-error")) {
          res_row$rho_bt <- cor(abs_bt[,1], as.numeric(gsub("[^0-9]", "", rownames(abs_bt))), method = "spearman")
          res_row$time_bt <- as.numeric(difftime(Sys.time(), start_t, units = "secs"))
        }
      }
    }
    
    # PL Model
    if (model_selection %in% c("pl", "both")) {
      start_t <- Sys.time()
      R <- try(PlackettLuce::as.rankings(pl_rankings_matrix), silent = TRUE)
      if (!inherits(R, "try-error")) {
        fit_pl <- try(PlackettLuce::PlackettLuce(R, maxit = c(maxit, 100)), silent = TRUE)
        if (!inherits(fit_pl, "try-error")) {
          abs_pl <- PlackettLuce::itempar(fit_pl, log = FALSE)
          res_row$rho_pl <- cor(as.numeric(abs_pl), as.numeric(names(abs_pl)), method = "spearman")
          res_row$time_pl <- as.numeric(difftime(Sys.time(), start_t, units = "secs"))
        }
      }
    }
    return(res_row)
  }
  
  close(pb)
  stopCluster(cl)
  return(final_data)
}

# Shiny UI
ui <- fluidPage(
  tags$head(tags$style(HTML(".shiny-progress { position: fixed; top: 50% !important; left: 50% !important; margin-left: -150px !important; }"))),
  titlePanel("BT vs PL v4.3: Real-time Progress Simulation"),
  sidebarLayout(
    sidebarPanel(
      numericInput("t_val", "Total Treatments (t):", 120),
      textInput("k_values", "Block Sizes (k):", "3, 4"),
      numericInput("r_limit", "Max Replications (r):", 20),
      textInput("temp_values", "Noise Levels:", "0.5, 5.0"),
      numericInput("seed_val", "Random Seed:", 1006),
      checkboxGroupInput("strategies", "Noise Strategies:", 
                         choices = c("global", "block", "comparison"), selected = "block"),
      selectInput("model_selection", "Models:", choices = c("both", "bt", "pl")),
      numericInput("maxit", "Max Iterations:", 20),
      numericInput("cores_free", "Cores to Keep Free:", 2),
      hr(),
      actionButton("run_sim", "🚀 Run Simulation", class = "btn-primary btn-lg", width = "100%"),
      hr(),
      actionButton("save_all", "💾 Save All Results", class = "btn-success", width = "100%")
    ),
    mainPanel(
      plotOutput("rhoPlot", height = "600px"),
      hr(),
      div(style = "display: none;", 
          h4("Simulation Log Summary"),
          tableOutput("summaryTable")
      )
    )
  )
)

server <- function(input, output, session) {
  # Create a reactive value to store the plot for saving
  current_plot <- reactiveVal()

  sim_results <- eventReactive(input$run_sim, {
    k_vec <- as.numeric(unlist(strsplit(input$k_values, ",")))
    temps <- as.numeric(unlist(strsplit(input$temp_values, ",")))
    
    # Initialize Shiny Progress
    progress <- shiny::Progress$new()
    progress$set(message = "Simulating Alpha Lattice...", value = 0)
    on.exit(progress$close())
    
    run_multi_k_batch(
      t_val = input$t_val, k_values = k_vec, r_limit = input$r_limit, 
      seed_val = input$seed_val, temp_values = temps, 
      error_strategies = input$strategies, maxit = input$maxit,
      model_selection = input$model_selection, cores_free = input$cores_free,
      shiny_progress = progress
    )
  })
  
  output$rhoPlot <- renderPlot({
    df <- sim_results()
    req(df)
    
    # Transform to long format for ggplot
    df_long <- tidyr::pivot_longer(df, 
                                  cols = starts_with("rho"), 
                                  names_to = "Model", 
                                  values_to = "Rho")
    
    # Clean up Model names for legend
    df_long$Model <- ifelse(df_long$Model == "rho_bt", "Bradley-Terry (BT)", "Plackett-Luce (PL)")
    
    p <- ggplot(df_long, aes(x = r, y = Rho, color = Model, linetype = as.factor(k))) +
      geom_line(size = 1.2) + 
      geom_point(size = 2) +
      facet_grid(strategy ~ temp, labeller = label_both) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "bottom",
            panel.spacing = unit(2, "lines"),
            strip.background = element_rect(fill = "#f0f0f0", color = NA)) +
      labs(title = "Ranking Accuracy (Spearman Rho) vs Replications",
           subtitle = sprintf("Treatments (t) = %d", input$t_val),
           x = "Replications (r)", 
           y = "Spearman Rho",
           linetype = "Block Size (k)")
    
    current_plot(p) # Store for saving
    return(p)
  })
  
  output$summaryTable <- renderTable({
    df <- sim_results()
    req(df)
    if ("rho_bt" %in% names(df) && "rho_pl" %in% names(df)) df$Advantage <- df$rho_pl - df$rho_bt
    head(df, 20)
  })
  
  observeEvent(input$save_all, {
    df <- sim_results()
    p <- current_plot()
    req(df, p)
    
    base_name <- sprintf("results_t%d_r%d", input$t_val, input$r_limit)
    
    # Save CSV
    write.csv(df, paste0(base_name, ".csv"), row.names = FALSE)
    
    # Save Plot
    ggsave(paste0(base_name, ".png"), p, width = 12, height = 8, dpi = 300)
    
    showNotification(sprintf("Saved CSV and PNG: %s", base_name), type = "message")
  })
}

shinyApp(ui, server)
