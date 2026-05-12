# =================================================================================
# Task 4: DEV-SLM - Data Preparation & JSON Functions
# Author: Megrez (AI Partner) | Mandate: No-LaTeX, Collaborative Breakdown
# =================================================================================

library(readxl)
library(jsonlite)
library(dplyr)
library(httr)

# --- Segment 1: Library/Database Initialization ---
# Purpose: Load the master list of items and mission context once for efficiency.

# Path to the experimental data (using the 4-item test set)
file_path <- "C:/Users/User/Documents/R/Thesis/studies/AI ranking test/data for thesis/data partition - 4 only.xlsx"

# Initialize item database (Sheet 1)
# Note: Excel sheet has no headers; manually defining column names.
item_db <- tryCatch({
  df <- read_excel(file_path, sheet = 1, col_names = FALSE)
  colnames(df) <- c("ITEM_ID", "ITEM_CONTENT")
  # Ensure IDs are treated as characters to avoid matching errors
  df$ITEM_ID <- as.character(df$ITEM_ID)
  df
}, error = function(e) {
  message("Error reading Sheet 1: ", e$message)
  return(data.frame(ITEM_ID=character(), ITEM_CONTENT=character()))
})

# Initialize mission context (Sheet 2)
# Extracting the context description from the first column header/cell
mission_context <- tryCatch({
  read_excel(file_path, sheet = 2) %>% colnames() %>% .[1]
}, error = function(e) {
  message("Error reading Sheet 2: ", e$message)
  return("Context not found.")
})

# --- Segment 2: Core Function (ID to JSON) ---
# Purpose: Convert a vector of IDs into a structured JSON object for AI evaluation.
get_block_json <- function(target_ids) {
  # 1. Filter database for the specified IDs
  # 2. Rename columns to match the required JSON keys ("id", "solution")
  selected_items <- item_db %>% 
    filter(ITEM_ID %in% as.character(target_ids)) %>%
    select(id = ITEM_ID, solution = ITEM_CONTENT)
  
  # 3. Create a named list to serve as the JSON root element
  result_list <- list(block = selected_items)
  
  # 4. Convert to formatted JSON string
  # auto_unbox = TRUE prevents single elements from being wrapped in arrays []
  json_output <- toJSON(result_list, pretty = TRUE, auto_unbox = TRUE)
  
  return(json_output)
}

# --- Segment 3: Ask SLM for Ranking (with Retry & Validation) ---
# Purpose: Send the prompt to the local Ollama instance and verify the response integrity.
ask_slm_ranking <- function(target_ids, model_name = "llama3.2", max_retries = 3) {

  # Convert IDs to character for robust comparison
  expected_ids <- as.character(target_ids)
  num_expected <- length(expected_ids)

  # 1. Generate the JSON-formatted test items
  block_json <- get_block_json(target_ids)

  # 2. Construct the system prompt with strict formatting and word count requirements
  prompt_text <- paste0(
    "You are a professional aviation service quality evaluator.\n",
    "MISSION CONTEXT:\n", mission_context, "\n\n",
    "TASK:\n",
    "Rank the following ", num_expected, " items from best to worst based on the mission context.\n",
    "DATA (JSON Format):\n", block_json, "\n\n",
    "CRITICAL REQUIREMENTS:\n",
    "1. You MUST include ALL ", num_expected, " item IDs: [", paste(expected_ids, collapse=", "), "].\n",
    "2. Limit your reason to under 100 words.\n",
    "3. Format your response EXACTLY like this:\n",
    "Ranking: [ID_X > ID_Y > ID_Z]\n",
    "Reason: [Your overall ranking logic]\n\n",
    "Wait, do NOT provide any extra text before or after the required format."
  )

  # 3. Execution Loop with Validation and Automatic Retry
  for (attempt in 1:max_retries) {
    message(sprintf("Attempt %d for block [%s]...", attempt, paste(expected_ids, collapse=",")))

    # POST request to local Ollama API
    response <- POST(
      url = "http://localhost:11434/api/generate",
      body = list(
        model = model_name,
        prompt = prompt_text,
        stream = FALSE,
        options = list(temperature = 0.7, num_predict = 400)
      ),
      encode = "json"
    )

    if (status_code(response) == 200) {
      ai_output <- content(response)$response
      
      # --- Strict Validation: Verify IDs within the 'Ranking' line only ---
      # Extract text between "Ranking:" and "Reason:"
      ranking_line <- gsub(".*Ranking: (.*)Reason:.*", "\\1", ai_output)
      
      # Use regex to find all numeric IDs in that specific line
      found_ids <- unlist(regmatches(ranking_line, gregexpr("[0-9]+", ranking_line)))
      
      # Confirm if all expected IDs are present in the ranking sequence
      is_valid <- all(expected_ids %in% found_ids)
      
      if (is_valid) {
        return(ai_output) # Success
      } else {
        message("  ⚠️ AI skipped items in the RANKING line. Retrying...")
      }
    } else {
      message("  ❌ Connection error. Retrying...")
    }
  }

  return("Error: AI repeatedly failed to include all items after max retries.")
}

# --- Quick Test ---
# Usage example:
# source("C:/Users/User/Documents/R/Thesis/studies/AI ranking test/dev_slm_functions.R")
# ai_result <- ask_slm_ranking(c(1, 2, 3, 4))
# cat(ai_result)
