# =================================================================================
# Shiny LLM Evaluator: Multi-Model Alpha Lattice Pipeline
# Author: Megrez (AI Partner) | Mandate: No-LaTeX, Multi-Model, Shiny UI
# =================================================================================

library(shiny)
library(readxl)
library(jsonlite)
library(dplyr)
library(httr)
library(FielDHub)
library(ggplot2)

# --- 1. Global Helper Functions ---

# Fetch available Ollama models
get_ollama_models <- function() {
  tryCatch({
    res <- GET("http://localhost:11434/api/tags")
    if (status_code(res) == 200) {
      models <- content(res)$models
      sapply(models, function(m) m$name)
    } else {
      c("llama3.2", "gemma3", "phi4-mini") # Fallback
    }
  }, error = function(e) c("llama3.2", "gemma3", "phi4-mini"))
}

# Stateless Evaluation Logic
evaluate_block_stateless <- function(target_ids, model, mission_context, item_db, max_retries = 3) {
  
  # Prepare Payload
  selected_items <- item_db |> 
    filter(ITEM_ID %in% as.character(target_ids)) |> 
    select(id = ITEM_ID, content = ITEM_CONTENT)
  block_data <- list(block = selected_items)
  
  expected_ids <- as.character(target_ids)
  
  prompt_text <- paste0(
    "You are a professional airport service quality evaluator.\n",
    "CONTEXT:\n", mission_context, "\n\n",
    "DATA:\n", toJSON(block_data, auto_unbox = TRUE, pretty = TRUE), "\n\n",
    "TASK:\nRank ALL items from best to worst based on the context.\n\n",
    "STRICT OUTPUT REQUIREMENT:\n",
    "1. Respond ONLY with a valid JSON object.\n",
    "2. NO markdown formatting, NO preamble, NO explanations.\n",
    "3. Format exactly: {\"rankings\": [{\"item_id\": \"ID_1\", \"rank\": 1, \"reason\": \"...\"}, ...]}\n",
    "4. Include ALL IDs: [", paste(expected_ids, collapse=", "), "]."
  )

  for (attempt in 1:max_retries) {
    res <- POST(
      url = "http://localhost:11434/api/generate",
      body = list(model = model, prompt = prompt_text, stream = FALSE, options = list(temperature = 0)),
      encode = "json"
    )
    
    if (status_code(res) == 200) {
      raw_content <- content(res)$response
      
      # Robust JSON Extraction: Find first '{' and last '}'
      json_start <- regexpr("\\{", raw_content)
      json_end <- regexpr("(?s).*\\}", raw_content, perl = TRUE)
      
      clean_content <- raw_content
      if (json_start > 0 && json_end > 0) {
        clean_content <- substr(raw_content, json_start, json_end)
      }

      parsed <- tryCatch({
        fromJSON(clean_content)
      }, error = function(e) NULL)
      
      if (!is.null(parsed)) {
        # Support variations
        rank_list <- if(!is.null(parsed$rankings)) parsed$rankings else parsed$ranking
        
        if (!is.null(rank_list)) {
          received_ids <- if(!is.null(rank_list$item_id)) as.character(rank_list$item_id) else as.character(rank_list$id)
          
          if (length(received_ids) == length(expected_ids) && all(expected_ids %in% received_ids)) {
            # Standardize
            if (!is.null(rank_list$id) && is.null(rank_list$item_id)) rank_list$item_id <- rank_list$id
            return(rank_list)
          }
        }
      }
    }
    Sys.sleep(0.5)
  }
  return(NULL)
}

# --- 2. Shiny UI ---

ui <- fluidPage(
  titlePanel("LLM Alpha Lattice Evaluator (v1.0)"),
  sidebarLayout(
    sidebarPanel(
      fileInput("data_file", "Upload Excel Data", accept = c(".xlsx")),
      helpText("Default: data partition -  only 12.xlsx"),
      hr(),
      selectInput("models", "Select LLM(s)", choices = NULL, multiple = TRUE),
      helpText("Multiple selection allowed. Models will run sequentially."),
      hr(),
      uiOutput("k_ui"),
      numericInput("r_rep", "Replications (r)", 2, min = 2),
      numericInput("seed", "Random Seed", 2026),
      hr(),
      actionButton("run_eval", "🚀 Start Evaluation", class = "btn-primary btn-lg", width = "100%")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Status & Preview",
                 br(),
                 uiOutput("progress_ui"),
                 hr(),
                 tableOutput("result_preview")
        ),
        tabPanel("Full Data",
                 br(),
                 downloadButton("download_csv", "Download Merged Results (.csv)"),
                 br(), br(),
                 dataTableOutput("full_table")
        )
      )
    )
  )
)

# --- 3. Shiny Server ---

server <- function(input, output, session) {
  
  # Reactive values to store state
  rv <- reactiveValues(
    models_list = get_ollama_models(),
    item_db = NULL,
    mission_context = NULL,
    final_merged = NULL
  )
  
  # Initialize Model List
  observe({
    updateSelectInput(session, "models", choices = rv$models_list, selected = rv$models_list[1])
  })
  
  # Load Data
  observe({
    path <- if (is.null(input$data_file)) {
      "C:/Users/User/Documents/R/Thesis/studies/AI ranking test/data for thesis/data partition -  only 12.xlsx"
    } else {
      input$data_file$datapath
    }
    
    try({
      df <- read_excel(path, sheet = 1, col_names = FALSE)
      colnames(df) <- c("ITEM_ID", "ITEM_CONTENT")
      df$ITEM_ID <- as.character(df$ITEM_ID)
      rv$item_db <- df
      
      context <- colnames(read_excel(path, sheet = 2))[1]
      rv$mission_context <- context
    })
  })
  
  # Free K input (User requested freedom)
  output$k_ui <- renderUI({
    req(rv$item_db)
    t_val <- nrow(rv$item_db)
    
    tagList(
      numericInput("k_val", "Block Size (k)", value = 4, min = 2, max = t_val),
      helpText(sprintf("Current Items (t): %d. Recommendation: t should be a multiple of k.", t_val))
    )
  })
  
  # The Main Execution Loop
  observeEvent(input$run_eval, {
    req(rv$item_db, input$models, input$k_val)
    
    t_val <- nrow(rv$item_db)
    k_val <- as.numeric(input$k_val)
    r_rep <- input$r_rep
    
    # Generate Design
    design_obj <- alpha_lattice(t = t_val, k = k_val, r = r_rep, seed = input$seed)
    master_plan <- design_obj$fieldBook
    master_plan$TREATMENT <- as.character(master_plan$TREATMENT)
    
    blocks_list <- master_plan |> group_by(REP, IBLOCK) |> group_split()
    total_blocks <- length(blocks_list)
    total_steps <- total_blocks * length(input$models)
    
    all_results <- list()
    step_counter <- 0
    
    withProgress(message = 'Running LLM Evaluation...', value = 0, {
      
      for (model_name in input$models) {
        message("\nSwitching to Model: ", model_name)
        
        for (i in seq_along(blocks_list)) {
          step_counter <- step_counter + 1
          incProgress(1/total_steps, detail = sprintf("Model: %s | Block %d/%d", model_name, i, total_blocks))
          
          block_data <- blocks_list[[i]]
          res <- evaluate_block_stateless(block_data$TREATMENT, model_name, rv$mission_context, rv$item_db)
          
          if (!is.null(res)) {
            res$Model <- model_name
            res$REP <- block_data$REP[1]
            res$IBLOCK <- block_data$IBLOCK[1]
            all_results[[length(all_results) + 1]] <- res
          }
        }
      }
    })
    
    # Consolidate
    if (length(all_results) > 0) {
      rankings_df <- do.call(rbind, all_results)
      rankings_df$item_id <- as.character(rankings_df$item_id)
      
      rv$final_merged <- master_plan |> 
        inner_join(rankings_df, by = c("REP", "IBLOCK", "TREATMENT" = "item_id")) |> 
        arrange(Model, REP, IBLOCK, rank)
      
      message("Success: All models evaluated.")
    }
  })
  
  # Outputs
  output$progress_ui <- renderUI({
    if (is.null(rv$final_merged)) {
      helpText("Status: Waiting for start...")
    } else {
      helpText(sprintf("Status: Completed. Evaluated %d items across %d model(s).", 
                       nrow(rv$item_db), length(input$models)))
    }
  })
  
  output$result_preview <- renderTable({
    req(rv$final_merged)
    head(rv$final_merged, 10)
  })
  
  output$full_table <- renderDataTable({
    req(rv$final_merged)
    rv$final_merged
  })
  
  output$download_csv <- downloadHandler(
    filename = function() { paste0("LLM_Alpha_Results_", Sys.Date(), ".csv") },
    content = function(file) { write.csv(rv$final_merged, file, row.names = FALSE) }
  )
}

shinyApp(ui, server)
