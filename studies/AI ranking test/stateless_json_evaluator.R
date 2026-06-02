# =================================================================================
# Stateless LLM Evaluator: JSON Orchestrator for Alpha Lattice
# Author: Megrez (AI Partner) | Mandate: No-LaTeX, Fresh Context, JSON Output
# =================================================================================

library(readxl)
library(jsonlite)
library(dplyr)
library(httr)
library(FielDHub)

# --- 1. Settings & Path ---
base_dir <- "C:/Users/User/Documents/R/Thesis/studies/AI ranking test"
excel_path <- file.path(base_dir, "data for thesis", "data partition -  only 12.xlsx")
model_endpoint <- "http://localhost:11434/api/generate"
target_model <- "llama3.2"

# Alpha Lattice Parameters
# Note: User confirmed k=4 works with FielDHub for t=12.
k_val <- 4  
r_rep <- 2  
seed_val <- 2026

# --- 2. Data & Design Initialization ---
item_db <- read_excel(excel_path, sheet = 1, col_names = FALSE)
colnames(item_db) <- c("ITEM_ID", "ITEM_CONTENT")
item_db$ITEM_ID <- as.character(item_db$ITEM_ID)

message(sprintf("Data loaded: %d items found.", nrow(item_db)))

mission_context <- colnames(read_excel(excel_path, sheet = 2))[1]

# Generate Alpha Design using FielDHub (matches your other simulation engine)
set.seed(seed_val)
message(sprintf("Generating Alpha Design (t=%d, k=%d, r=%d)...", nrow(item_db), k_val, r_rep))

alpha_design_obj <- try(alpha_lattice(t = nrow(item_db), k = k_val, r = r_rep, seed = seed_val), silent = FALSE)

if (inherits(alpha_design_obj, "try-error")) {
  stop("Alpha design generation failed. Check parameters.")
}

master_plan <- alpha_design_obj$fieldBook

# --- 3. JSON Generation Utility ---
# (Unchanged)
get_block_json_payload <- function(target_ids) {
  selected_items <- item_db |> 
    filter(ITEM_ID %in% as.character(target_ids)) |> 
    select(id = ITEM_ID, content = ITEM_CONTENT)
  return(list(block = selected_items))
}

# --- 4. The Stateless Evaluator Function ---
# ... (rest of the function logic stays the same)
evaluate_block_stateless <- function(target_ids, model = target_model, max_retries = 3) {
  expected_ids <- as.character(target_ids)
  block_data <- get_block_json_payload(target_ids)
  
  prompt_text <- paste0(
    "You are a professional airport service quality evaluator.\n",
    "CONTEXT:\n", mission_context, "\n\n",
    "DATA (Evaluate these items):\n", toJSON(block_data, auto_unbox = TRUE, pretty = TRUE), "\n\n",
    "TASK:\n",
    "Rank ALL items from best to worst based on the context.\n\n",
    "STRICT OUTPUT REQUIREMENT:\n",
    "1. Respond ONLY with a valid JSON object.\n",
    "2. NO markdown formatting, NO ```json blocks, NO preamble, NO explanations.\n",
    "3. Format must be exactly:\n",
    "{\n",
    "  \"rankings\": [\n",
    "    { \"item_id\": \"ID_1\", \"rank\": 1, \"reason\": \"...\" },\n",
    "    { \"item_id\": \"ID_2\", \"rank\": 2, \"reason\": \"...\" }\n",
    "  ]\n",
    "}\n",
    "4. Ensure ALL item IDs are included: [", paste(expected_ids, collapse=", "), "]."
  )

  for (attempt in 1:max_retries) {
    message(sprintf("--> Requesting evaluation for block [%s] (Attempt %d)...", 
                    paste(expected_ids, collapse=","), attempt))
    
    response <- POST(
      url = model_endpoint,
      body = list(model = model, prompt = prompt_text, stream = FALSE, options = list(temperature = 0.1)),
      encode = "json"
    )
    
    if (status_code(response) == 200) {
      raw_content <- content(response)$response
      parsed_data <- tryCatch({
        clean_content <- gsub("```json|```", "", raw_content) |> trimws()
        fromJSON(clean_content)
      }, error = function(e) NULL)
      
      if (!is.null(parsed_data) && !is.null(parsed_data$rankings)) {
        received_ids <- as.character(parsed_data$rankings$item_id)
        if (length(received_ids) == length(expected_ids) && all(expected_ids %in% received_ids)) {
          message("✅ Success: Received valid detailed JSON ranking.")
          return(parsed_data)
        }
      }
    }
  }
  return(NULL)
}

# --- 5. Main Orchestration Loop ---
# Split master_plan into blocks using FielDHub column names (REP, IBLOCK)
blocks_list <- master_plan |> group_by(REP, IBLOCK) |> group_split()
results_storage <- list()

message(sprintf("Starting simulation: Total %d blocks to evaluate...", length(blocks_list)))

for (i in seq_along(blocks_list)) {
  current_block <- blocks_list[[i]]
  # FielDHub uses 'TREATMENT' for the IDs
  target_ids <- current_block$TREATMENT
  
  block_label <- paste0("Rep", current_block$REP[1], "_B", current_block$IBLOCK[1])
  message(sprintf("\n[Block %d/%d] Processing %s...", i, length(blocks_list), block_label))
  
  eval_result <- evaluate_block_stateless(target_ids)
  
  if (!is.null(eval_result)) {
    results_storage[[block_label]] <- eval_result$rankings
  } else {
    message("❌ Critical: Failed to get valid ranking for ", block_label)
  }
}

# --- 6. Results Consolidation & CSV Export ---
message("\n--- ALL BLOCKS COMPLETED ---")
message("Consolidating results with experimental design...")

# 1. Convert LLM rankings list to Data Frame
rankings_df <- do.call(rbind, lapply(names(results_storage), function(x) {
  df <- results_storage[[x]]
  # Parse Block Label (e.g., Rep1_B2) to back-link to design
  parts <- strsplit(x, "_")[[1]]
  df$REP <- as.numeric(gsub("Rep", "", parts[1]))
  df$IBLOCK <- as.numeric(gsub("B", "", parts[2]))
  return(df)
}))

# 2. Merge Design (master_plan) with AI Rankings
# FielDHub columns: REP, IBLOCK, TREATMENT
master_plan$TREATMENT <- as.character(master_plan$TREATMENT)
rankings_df$item_id <- as.character(rankings_df$item_id)

final_merged <- master_plan |> 
  inner_join(rankings_df, by = c("REP" = "REP", "IBLOCK" = "IBLOCK", "TREATMENT" = "item_id")) |> 
  arrange(REP, IBLOCK, rank)

# 3. Final File Exports
rds_file <- file.path(base_dir, "alpha_lattice_llm_results.rds")
csv_file <- file.path(base_dir, "AI_Detailed_Rankings.csv")

# Save as RDS (for R analysis)
saveRDS(list(design = master_plan, rankings = results_storage, merged = final_merged), rds_file)

# Save as CSV (for Excel viewing)
write.csv(final_merged, csv_file, row.names = FALSE)

message("--------------------------------------------------")
message("✅ SUCCESS: Pipeline Completed!")
message("Total Items Evaluated: ", nrow(item_db))
message("Total Replications: ", r_rep)
message("Final CSV Exported to: ", csv_file)
message("--------------------------------------------------")

# Preview top 10 results
print(head(final_merged, 10))
