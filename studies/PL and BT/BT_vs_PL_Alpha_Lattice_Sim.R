# =================================================================================
# BT vs PL Alpha Lattice Simulation Engine (v4.0 - Auto-Batch Edition)
# Author: Megrez (AI Partner)
# =================================================================================

# Load required libraries
packages <- c("evd", "PlackettLuce", "FielDHub", "BradleyTerry2", "ggplot2", "tidyr", "shiny", "parallel", "doSNOW", "foreach", "dplyr")
for (p in packages) { if (!require(p, character.only = TRUE)) install.packages(p) }

library(evd); library(FielDHub); library(BradleyTerry2); library(PlackettLuce)
library(ggplot2); library(parallel); library(doSNOW); library(foreach); library(tidyr); library(shiny); library(dplyr)

# Core Simulation Engine
run_multi_k_batch <- function(t_val = 120, k_values = c(3, 4), r_limit = 20, seed_val = 1006, 
                              temp_values = c(0.5, 5.0), error_strategies = c("block"), 
                              maxit = 20, model_selection = "both") {
  
  # Expand task grid to include temps and strategies
  task_grid <- expand.grid(k = k_values, r = 2:r_limit, temp = temp_values, strategy = error_strategies)
  total_tasks <- nrow(task_grid)
  
  num_cores <- max(1, parallel::detectCores() - 2) 
  cl <- makeCluster(num_cores); registerDoSNOW(cl)
  
  pb <- txtProgressBar(max = total_tasks, style = 3)
  progress <- function(n) { setTxtProgressBar(pb, n); flush.console() }
  opts <- list(progress = progress)
  
  final_data <- foreach(i = 1:total_tasks, .combine = rbind, 
                        .packages = c("FielDHub", "BradleyTerry2", "PlackettLuce", "evd"),
                        .options.snow = opts) %dopar% {
    
    task <- task_grid[i, ]
    set.seed(seed_val + i)
    
    design <- try(alpha_lattice(t = t_val, k = task$k, r = task$r, seed = seed_val), silent = TRUE)
    if (inherits(design, "try-error")) return(NULL) 
    
    fb <- design$fieldBook
    fb$TrueValue <- as.numeric(fb$ENTRY)
    
    # Global Noise Pre-calculation
    global_noise <- if (task$strategy == "global") setNames(rgumbel(t_val, 0, 1), unique(fb$ENTRY)) else NULL
    
    block_tags <- paste(fb$REP, fb$IBLOCK, sep = "_")
    pl_rankings <- list(); bt_matches <- data.frame(); blocks <- split(fb, block_tags)
    
    for (j in seq_along(blocks)) {
      block <- blocks[[j]]
      # Utility logic
      if (task$strategy == "global") {
        u_scores <- block$TrueValue + task$temp * global_noise[as.character(block$ENTRY)]
      } else if (task$strategy == "block") {
        u_scores <- block$TrueValue + task$temp * rgumbel(nrow(block), 0, 1)
      } else { u_scores <- block$TrueValue }
      
      # PL Data
      u_for_pl <- if(task$strategy == "comparison") block$TrueValue + task$temp * rgumbel(nrow(block), 0, 1) else u_scores
      pl_rankings[[j]] <- block$TREATMENT[order(-u_for_pl)]
      
      # BT Data
      pm <- combn(block$TREATMENT, 2)
      if (task$strategy == "comparison") {
        u1 <- block$TrueValue[match(pm[1,], block$TREATMENT)] + task$temp * rgumbel(ncol(pm), 0, 1)
        u2 <- block$TrueValue[match(pm[2,], block$TREATMENT)] + task$temp * rgumbel(ncol(pm), 0, 1)
      } else {
        u1 <- u_scores[match(pm[1,], block$TREATMENT)]; u2 <- u_scores[match(pm[2,], block$TREATMENT)]
      }
      bt_matches <- rbind(bt_matches, data.frame(Winner = ifelse(u1 > u2, pm[1,], pm[2,]), Loser = ifelse(u1 > u2, pm[2,], pm[1,])))
    }
    
    res <- data.frame(k = as.factor(task$k), r = task$r, temp = task$temp, strategy = task$strategy)
    
    # Model Fit
    if (model_selection %in% c("bt", "both")) {
      fit_bt <- try(BTm(1, factor(Winner, levels=unique(fb$TREATMENT)), factor(Loser, levels=unique(fb$TREATMENT)), 
                        data = bt_matches, control = glm.control(maxit = maxit)), silent = TRUE)
      if (!inherits(fit_bt, "try-error")) {
        ab <- try(BTabilities(fit_bt), silent = TRUE)
        if (!inherits(ab, "try-error")) {
          df_bt <- data.frame(ID = as.numeric(gsub("[^0-9]", "", rownames(ab))), Val = ab[,1]) %>% arrange(ID)
          res$rho_bt <- cor(df_bt$Val, df_bt$ID, method = "spearman")
        }
      }
    }
    if (model_selection %in% c("pl", "both")) {
      fit_pl <- try(PlackettLuce(as.rankings(pl_rankings), maxit = c(maxit, 100)), silent = TRUE)
      if (!inherits(fit_pl, "try-error")) {
        ab <- itempar(fit_pl, log = FALSE)
        df_pl <- data.frame(ID = as.numeric(names(ab)), Val = as.numeric(ab)) %>% arrange(ID)
        res$rho_pl <- cor(df_pl$Val, df_pl$ID, method = "spearman")
      }
    }
    return(res)
  }
  close(pb); stopCluster(cl); gc(); return(final_data)
}

# UI
ui <- fluidPage(
  titlePanel("BT vs PL v4.0: Auto-Batch Simulation"),
  sidebarLayout(
    sidebarPanel(
      numericInput("t_val", "Treatments (t):", 120),
      textInput("k_values", "Block Sizes (k):", "3, 4"),
      numericInput("r_limit", "Max Replications (r):", 20),
      textInput("temp_values", "Noise Levels (comma separated):", "0.5, 5, 10"),
      numericInput("seed_val", "Random Seed:", 1006),
      checkboxGroupInput("strategies", "Noise Strategies:", 
                         choices = c("global", "block", "comparison"), selected = c("block")),
      selectInput("model_selection", "Models:", choices = c("both", "bt", "pl")),
      actionButton("run_sim", "🚀 Run Batch Simulation", class = "btn-primary btn-lg", width = "100%"),
      hr(),
      actionButton("save_all", "💾 One-Click Save All Plots & CSV", class = "btn-success", width = "100%")
    ),
    mainPanel(
      plotOutput("rhoPlot", height = "600px"),
      tableOutput("summaryTable")
    )
  )
)

server <- function(input, output) {
  sim_data <- eventReactive(input$run_sim, {
    temps <- as.numeric(unlist(strsplit(input$temp_values, ",")))
    k_vec <- as.numeric(unlist(strsplit(input$k_values, ",")))
    run_multi_k_batch(t_val = input$t_val, k_values = k_vec, r_limit = input$r_limit, 
                      seed_val = input$seed_val, temp_values = temps, 
                      error_strategies = input$strategies, model_selection = input$model_selection)
  })
  
  output$rhoPlot <- renderPlot({
    req(sim_data())
    df_long <- pivot_longer(sim_data(), cols = starts_with("rho"), names_to = "Model", values_to = "Rho")
    ggplot(df_long, aes(x = r, y = Rho, color = interaction(k, Model), linetype = Model)) +
      geom_line(size = 1.2) + facet_wrap(~strategy + temp, labeller = label_both) +
      theme_minimal(base_size = 14) + labs(title = "Batch Ranking Accuracy")
  })
  
  output$summaryTable <- renderTable({ head(sim_data(), 10) })
  
  observeEvent(input$save_all, {
    df <- sim_data()
    req(df)
    # Save CSV
    csv_name <- sprintf("sim_data_t%d_r%d_seed%d.csv", input$t_val, input$r_limit, input$seed_val)
    write.csv(df, csv_name, row.names = FALSE)
    
    # Save individual plots per strategy and temp
    combos <- df %>% select(strategy, temp) %>% unique()
    for(i in 1:nrow(combos)) {
      s <- combos$strategy[i]; t <- combos$temp[i]
      sub_df <- df %>% filter(strategy == s, temp == t) %>% 
                pivot_longer(cols = starts_with("rho"), names_to = "Model", values_to = "Rho")
      
      p <- ggplot(sub_df, aes(x = r, y = Rho, color = interaction(k, Model), linetype = Model)) +
           geom_line(size = 1.2) + geom_point() + theme_minimal() +
           labs(title = sprintf("k=%s r=%d t=%d noise=%s model=%s strategy=%s", 
                               input$k_values, input$r_limit, input$t_val, t, input$model_selection, s))
      
      file_name <- sprintf("sim_k%s_r%d_t%d_noise%.1f_model%s_strat%s.png", 
                           gsub(",","_",input$k_values), input$r_limit, input$t_val, t, input$model_selection, s)
      ggsave(file_name, p, width = 10, height = 6)
    }
    showNotification("All files saved to project directory!", type = "message")
  })
}

shinyApp(ui, server)
