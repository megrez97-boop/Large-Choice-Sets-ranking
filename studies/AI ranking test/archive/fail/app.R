library(shiny)
library(agricolae)
library(readxl)
library(dplyr)
library(httr)
library(jsonlite)
library(DT)

# --- UI Configuration ---
ui <- fluidPage(
  titlePanel("Alpha Lattice AI Ranking Dashboard (v1.41 - Fixed Join)"),
  sidebarLayout(
    sidebarPanel(
      fileInput("upload_file", "Upload Airport Suggestions (Excel)", accept = c(".xlsx")),
      numericInput("num_items_to_process", "Number of Items:", value = 48, min = 12, max = 400),
      hr(),
      checkboxGroupInput("selected_ai_models", "Select Ollama Models:", 
                         choices = c("gemma3", "llama3.2", "phi4-mini", "llama3.1"),
                         selected = c("gemma3", "llama3.2")),
      textInput("target_temps", "Temperatures:", value = "0, 0.5, 1, 10"),
      numericInput("num_replicates", "Replicates (r):", value = 40, min = 1),
      checkboxGroupInput("block_k_sizes", "Block Sizes (k):", choices = c(3, 4), selected = c(3, 4)),
      numericInput("exp_design_seed", "Design Seed:", value = 2026),
      hr(),
      actionButton("start_exp", "Run Experiment", class = "btn-primary", style = "width: 100%"),
      hr(),
      downloadButton("download_results_csv", "Download Results (CSV)")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Task Context", 
                 h4("Detected Mission Context:"),
                 div(style = "background-color: #f0f7ff; padding: 15px; border-radius: 5px; border-left: 5px solid #007bff;",
                     textOutput("display_task_context"))),
        tabPanel("Status & Log", 
                 verbatimTextOutput("execution_log")),
        tabPanel("Current Results", 
                 DTOutput("results_preview_table"))
      )
    )
  )
)

# --- Server Logic ---
server <- function(input, output, session) {
  log_storage <- reactiveVal("Ready...")
  final_results_storage <- reactiveVal(data.frame())
  current_task_context_str <- reactiveVal("Awaiting file...")
  
  append_to_log <- function(msg) {
    log_storage(paste0(log_storage(), "\n", "[", Sys.time(), "] ", msg))
  }
  
  output$execution_log <- renderText({ log_storage() })
  output$display_task_context <- renderText({ current_task_context_str() })
  
  observeEvent(input$upload_file, {
    tryCatch({
      avail_sheets <- excel_sheets(input$upload_file$datapath)
      if (length(avail_sheets) >= 2) {
        raw_data <- read_excel(input$upload_file$datapath, sheet = 2, col_names = FALSE)
        all_text <- unlist(raw_data) %>% as.character()
        valid_text <- all_text[!is.na(all_text) & nchar(all_text) > 20]
        if (length(valid_text) > 0) {
          current_task_context_str(as.character(valid_text[which.max(nchar(valid_text))]))
          append_to_log("Context loaded from Sheet 2.")
        }
      }
    }, error = function(e) { append_to_log(e$message) })
  })
  
  observeEvent(input$start_exp, {
    req(input$upload_file)
    temp_vec <- as.numeric(unlist(strsplit(input$target_temps, ",")))
    model_vec <- input$selected_ai_models
    k_vec <- as.numeric(input$block_k_sizes)
    r_val <- input$num_replicates
    
    # Load items and force ID to character
    raw_items <- read_excel(input$upload_file$datapath, sheet = 1)[1:input$num_items_to_process, ]
    colnames(raw_items)[1:2] <- c("ITEM_ID", "ITEM_CONTENT")
    raw_items$ITEM_ID <- as.character(raw_items$ITEM_ID)
    
    master_plan <- data.frame()
    set.seed(input$exp_design_seed)
    
    for (curr_k in k_vec) {
      append_to_log(paste("Designing Alpha Lattice for k =", curr_k))
      
      d_out <- design.alpha(raw_items$ITEM_ID, k = curr_k, r = r_val, seed = input$exp_design_seed)
      d_book <- d_out$book
      
      # Robust Column Renaming (v1.41)
      all_cols <- colnames(d_book)
      reserved <- c("replication", "block", "plots")
      id_col_name <- setdiff(all_cols, reserved)
      
      if(length(id_col_name) > 0) {
        colnames(d_book)[colnames(d_book) == id_col_name[1]] <- "ITEM_ID"
        d_book$ITEM_ID <- as.character(d_book$ITEM_ID)
        d_book$K_SIZE <- curr_k
        master_plan <- bind_rows(master_plan, d_book)
      } else {
        append_to_log("Error: ID column not found in design.")
      }
    }
    
    # Safe Join
    master_plan <- master_plan %>% left_join(raw_items, by = "ITEM_ID")
    
    block_list <- master_plan %>% group_by(K_SIZE, replication, block) %>% group_split()
    results_collector <- list()
    
    withProgress(message = 'Ranking Items...', value = 0, {
      for (m in model_vec) {
        for (tp in temp_vec) {
          for (bl in block_list) {
            incProgress(1/ (length(model_vec)*length(temp_vec)*length(block_list)))
            
            items_listing <- paste(paste0("[ID_", bl$ITEM_ID, "]: ", bl$ITEM_CONTENT), collapse = "\n")
            
            # Integrated English Prompt
            prompt_en <- paste0(
              "You are a professional aviation service quality evaluator.\n",
              "MISSION CONTEXT:\n", current_task_context_str(), "\n\n",
              "TASK:\n",
              "Rank ALL the airport guidance items listed below from best to worst based on the mission context.\n\n",
              "REQUIREMENTS:\n",
              "1. You MUST include EVERY item ID provided in the list below.\n",
              "2. Output ONLY the ranking string using '>' as a separator.\n",
              "3. Format Example: ID_1 > ID_2 > ID_3 > ID_4.\n",
              "4. Do NOT provide any explanations or extra text.\n\n",
              "ITEMS TO EVALUATE:\n",
              items_listing
            )
            
            res <- tryCatch({
              POST("http://localhost:11434/api/generate", 
                   body = list(model = m, prompt = prompt_en, stream = FALSE, 
                               options = list(temperature = tp, num_predict = 150)), 
                   encode = "json", timeout(60))
            }, error = function(e) return(NULL))
            
            ans <- if(!is.null(res) && status_code(res)==200) content(res)$response else "Error/Timeout"
            
            results_collector[[length(results_collector)+1]] <- data.frame(
              Model=m, Temp=tp, K=bl$K_SIZE[1], Rep=bl$replication[1], Block=bl$block[1], AI_Ranking=ans
            )
            
            if (length(results_collector) %% 10 == 0) {
              final_results_storage(bind_rows(results_collector))
            }
          }
        }
      }
    })
    
    final_results_storage(bind_rows(results_collector))
    append_to_log("--- Experiment Successfully Completed! ---")
  })
  
  output$results_preview_table <- renderDT({ datatable(final_results_storage()) })
  output$download_results_csv <- downloadHandler(
    filename = function() { paste0("AI_Results_v1.41_", Sys.Date(), ".csv") },
    content = function(file) { write.csv(final_results_storage(), file, row.names = FALSE) }
  )
}

shinyApp(ui, server)
