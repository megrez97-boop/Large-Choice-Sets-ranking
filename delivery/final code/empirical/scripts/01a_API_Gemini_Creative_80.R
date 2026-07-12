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
base_dir <- getwd()
# (已由外部 Pipeline 控制工作目錄，移除 setwd)

excel_path <- file.path(base_dir, "data", "data partition-creative - 80.xlsx")
target_models <- c("gemini-3.1-flash-lite") # Switched to 3.1 Flash Lite due to 500 RPD quota

# Alpha Lattice Parameters
k_val <- 4  
r_rep <- 20  
seed_val <- 12345
run_timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

# --- 2. Data & Design Initialization ---
item_db <- read_excel(excel_path, sheet = 1, col_names = FALSE)
colnames(item_db) <- c("ITEM_ID", "ITEM_CONTENT")
item_db$ITEM_ID <- as.character(item_db$ITEM_ID)

message(sprintf("Data loaded: %d items found.", nrow(item_db)))

# Robustly read the context from Sheet 2, Cell A1
context_df <- read_excel(excel_path, sheet = 2, col_names = FALSE)
mission_context <- as.character(context_df[1, 1])

# Print a snippet so the user can verify it was loaded correctly
message("--------------------------------------------------")
message("✅ Mission Context Loaded Successfully!")
message("Preview: ", substr(mission_context, 1, 150), "...")
message("--------------------------------------------------")

# Generate Alpha Design using FielDHub
set.seed(seed_val)
message(sprintf("Generating Alpha Design (t=%d, k=%d, r=%d)...", nrow(item_db), k_val, r_rep))

alpha_design_obj <- try(alpha_lattice(t = nrow(item_db), k = k_val, r = r_rep, seed = seed_val), silent = FALSE)

if (inherits(alpha_design_obj, "try-error")) {
  stop("Alpha design generation failed. Check parameters.")
}

master_plan <- alpha_design_obj$fieldBook

# CRITICAL FIX: alpha_lattice generates TREATMENTS as 1, 2, ..., t or sometimes "T1", "T2". 
# We extract the pure numeric index and map it back to the true ITEM_ID from the Excel database.
numeric_indices <- as.numeric(gsub("[^0-9]", "", as.character(master_plan$TREATMENT)))
master_plan$TREATMENT <- item_db$ITEM_ID[numeric_indices]

# --- 3. JSON Generation Utility ---
get_block_json_payload <- function(target_ids) {
  selected_items <- item_db |> 
    filter(ITEM_ID %in% as.character(target_ids)) |> 
    select(id = ITEM_ID, content = ITEM_CONTENT) |>
    sample_n(n()) # <--- CRITICAL: Keep shuffling to address bias
  
  return(list(block = selected_items))
}

# --- 4. The Stateless Evaluator Function ---
evaluate_block_stateless <- function(target_ids, model = target_models[1], block_label = "Unknown", max_retries = 15) {
  expected_ids <- as.character(target_ids)
  
  # Extract REP and IBLOCK from block_label for deterministic seeding
  parts <- strsplit(gsub("Rep|B", "", block_label), "_")[[1]]
  rep_val <- as.numeric(parts[1])
  iblock_val <- as.numeric(parts[2])
  block_seed <- seed_val + rep_val * 10000 + iblock_val
  
  # Set deterministic seed for this specific block to guarantee reproducibility
  set.seed(block_seed)
  
  # Prepare block items with shuffled presentation order
  block_data_raw <- item_db |> 
    filter(ITEM_ID %in% expected_ids) |> 
    select(id = ITEM_ID, content = ITEM_CONTENT) |>
    sample_n(n())
  
  presentation_order_original <- as.character(block_data_raw$id)
  
  # OBFUSCATION: Map numeric IDs to Option_A, Option_B, etc. to prevent numeric bias
  obfuscated_ids <- paste0("Option_", LETTERS[1:nrow(block_data_raw)])
  id_map <- setNames(presentation_order_original, obfuscated_ids)
  block_data_raw$id <- obfuscated_ids
  
  # Gemini JSON Schema Definition
  json_schema <- list(
    type = "object",
    properties = list(
      rankings = list(
        type = "array",
        items = list(
          type = "object",
          properties = list(
            item_id = list(type = "string"),
            reason = list(type = "string"),
            rank = list(type = "integer")
          ),
          required = c("item_id", "reason", "rank")
        )
      )
    ),
    required = c("rankings")
  )
  
  current_block_data <- block_data_raw
  gemini_api_key <- Sys.getenv("GEMINI_API_KEY")
  
  for (attempt in 1:max_retries) {
    message("--> Requesting evaluation for block [", paste(target_ids, collapse=","), "] (Attempt ", attempt, ")...")
    
    if (attempt > 1) {
      # Ensure retries are also deterministic
      set.seed(block_seed + attempt)
      current_block_data <- current_block_data |> sample_n(n())
    }
    
    prompt_text <- paste0(
      "You are a professional airport service quality evaluator.\n",
      "CONTEXT:\n", mission_context, "\n\n",
      "DATA (Evaluate these items):\n", toJSON(list(block = current_block_data), auto_unbox = TRUE, pretty = TRUE), "\n\n",
      "TASK:\n",
      sprintf("1. Rank ALL %d items from best to worst based on the context.\n", nrow(current_block_data)),
      "2. For EACH item, provide a CONCISE 'reason' (maximum 20 words) BEFORE ranking it. You MUST point out one specific strength or weakness compared to the others to justify its rank.\n\n",
      sprintf("3. CRITICAL: You MUST rank EXACTLY %d items. Ensure ALL these IDs are included: [%s]. DO NOT MISS ANY.", 
              nrow(current_block_data), paste(obfuscated_ids, collapse=", "))
    )
    
    gemini_url <- paste0("https://generativelanguage.googleapis.com/v1beta/models/", model, ":generateContent?key=", gemini_api_key)
    
    response <- tryCatch({ POST(
      url = gemini_url,
      content_type_json(),
      timeout(60), # Safety against hanging requests
      encode = "json",
      body = list(
        contents = list(
          list(parts = list(list(text = prompt_text)))
        ),
        generationConfig = list(
          temperature = 0,
          responseMimeType = "application/json",
          responseSchema = json_schema
        )
      )
    )}, error = function(e) {
      message("  ⚠️ Network/Timeout Error: ", e$message)
      return(NULL)
    })
    
    if (!is.null(response) && status_code(response) == 200) {
      parsed_response <- content(response, as = "parsed", type = "application/json")
      raw_content <- parsed_response$candidates[[1]]$content$parts[[1]]$text
      
      parsed_data <- tryCatch({
        fromJSON(raw_content)
      }, error = function(e) {
        message("  ⚠️ Parsing failed. Raw: ", substr(raw_content, 1, 50))
        NULL
      })
      
      if (!is.null(parsed_data)) {
        rankings_list <- parsed_data$rankings
        if (!is.null(rankings_list)) {
          received_ids_raw <- rankings_list$item_id
          if (!is.null(received_ids_raw)) {
            received_ids_upper <- toupper(as.character(received_ids_raw))
            norm_received <- gsub(".*([A-Z]).*", "\\1", received_ids_upper)
            norm_expected <- LETTERS[1:length(expected_ids)]
            
            if (length(norm_received) == length(norm_expected) && all(norm_expected %in% norm_received)) {
              message("✅ Success: Received valid detailed JSON ranking.")
              
              # --- NEW: Save the RAW JSON to a file ---
              json_dir <- file.path(base_dir, "results", "raw_jsons")
              if (!dir.exists(json_dir)) dir.create(json_dir, recursive = TRUE)
              json_filename <- file.path(json_dir, paste0(model, "_", run_timestamp, "_", block_label, ".json"))
              writeLines(raw_content, json_filename)
              
              # Save ID Map for debugging
              map_filename <- file.path(json_dir, paste0(model, "_", run_timestamp, "_", block_label, "_map.json"))
              writeLines(toJSON(as.list(id_map), auto_unbox = TRUE, pretty = TRUE), map_filename)
              
              mapped_ids <- sapply(norm_received, function(letter) {
                id_map[paste0("Option_", letter)]
              })
              
              actual_presented_options <- as.character(current_block_data$id)
              received_options <- paste0("Option_", norm_received)
              rankings_list$presentation_pos <- match(received_options, actual_presented_options)
              
              rankings_list$item_id <- mapped_ids
              parsed_data$rankings <- rankings_list
              
              return(parsed_data)
            } else {
              message("  ⚠️ ID Mismatch. Expected [", paste(norm_expected, collapse=","), "], Got [", paste(norm_received, collapse=","), "]")
            }
          } else {
            message("  ⚠️ JSON Missing ID fields (id or item_id).")
          }
        } else {
          message("  ⚠️ JSON Missing 'rankings' list.")
        }
      }
    } else {
      if (is.null(response)) {
        message("  ⚠️ API Error: No response received (Timeout/Network). Retrying...")
        Sys.sleep(10)
      } else {
        err_detail <- ""
        try({
          err_json <- content(response, as = "parsed", type = "application/json")
          if (!is.null(err_json$error$message)) err_detail <- paste0(" | Details: ", err_json$error$message)
        }, silent = TRUE)
        message("  ⚠️ API Error: Status ", status_code(response), err_detail, ". Retrying...")
        if (status_code(response) == 429) {
          backoff_time <- min(2 ^ attempt, 60) # Exponential backoff max 60s
          message("  ⏳ Rate limit hit (429). Backing off for ", backoff_time, " seconds...")
          Sys.sleep(backoff_time)
        } else {
          Sys.sleep(2)
        }
      }
    }
  }
  return(NULL)
}

# --- 5. Multi-Model Orchestration Loop ---
blocks_list <- master_plan |> group_by(REP, IBLOCK) |> group_split()
all_models_final_results <- list()

results_dir <- file.path(base_dir, "results")
if (!dir.exists(results_dir)) dir.create(results_dir, recursive = TRUE)

checkpoint_file <- file.path(results_dir, "checkpoint_Creative_80.csv")
if (file.exists(checkpoint_file)) {
  checkpoint_data <- read.csv(checkpoint_file, colClasses = c(item_id = "character"), stringsAsFactors = FALSE)
  message(sprintf("Loaded checkpoint with %d rows.", nrow(checkpoint_data)))
} else {
  checkpoint_data <- data.frame()
}

message(sprintf("Starting simulation: Total %d blocks to evaluate per model...", length(blocks_list)))

for (current_model in target_models) {
  message(sprintf("\n=================================================="))
  message(sprintf("🚀 STARTING EVALUATION FOR MODEL: %s", current_model))
  message(sprintf("=================================================="))
  
  results_storage <- list()
  
  for (i in seq_along(blocks_list)) {
    current_block <- blocks_list[[i]]
    target_ids <- current_block$TREATMENT
    
    rep_val <- current_block$REP[1]
    iblock_val <- current_block$IBLOCK[1]
    block_label <- paste0("Rep", rep_val, "_B", iblock_val)
    
    # --- CHECKPOINT CHECK ---
    if (nrow(checkpoint_data) > 0) {
      existing_records <- checkpoint_data |> 
        filter(MODEL == current_model, REP == rep_val, IBLOCK == iblock_val)
      if (nrow(existing_records) > 0) {
        message(sprintf("\n[%s] [Block %d/%d] %s already completed in checkpoint. Skipping API.", current_model, i, length(blocks_list), block_label))
        results_storage[[block_label]] <- existing_records |> select(-MODEL, -REP, -IBLOCK)
        next
      }
    }
    
    message(sprintf("\n[%s] [Block %d/%d] Processing %s...", current_model, i, length(blocks_list), block_label))
    
    eval_result <- evaluate_block_stateless(target_ids, model = current_model, block_label = block_label)
    
    if (!is.null(eval_result)) {
      results_storage[[block_label]] <- eval_result$rankings
      
      # --- SAVE TO CHECKPOINT ---
      chk_df <- eval_result$rankings
      chk_df$REP <- rep_val
      chk_df$IBLOCK <- iblock_val
      chk_df$MODEL <- current_model
      
      write.table(chk_df, file = checkpoint_file, append = file.exists(checkpoint_file), 
                  sep = ",", row.names = FALSE, col.names = !file.exists(checkpoint_file))
      
      Sys.sleep(4) # Base delay between requests to prevent 429 limit
    } else {
      # CRITICAL FIX: Fail fast. Do not silently continue and create incomplete CSV data.
      stop(sprintf("❌ Critical: Failed to get valid ranking for %s after maximum retries. Stopping execution to preserve API Quota and Data Integrity. You can restart the script to resume from the last successful block.", block_label))
    }
  }
  
  message(sprintf("\n--- COMPLETED MODEL: %s ---", current_model))
  
  if (length(results_storage) > 0) {
    rankings_df <- do.call(rbind, lapply(names(results_storage), function(x) {
      df <- results_storage[[x]]
      parts <- strsplit(x, "_")[[1]]
      df$REP <- as.numeric(gsub("Rep", "", parts[1]))
      df$IBLOCK <- as.numeric(gsub("B", "", parts[2]))
      df$MODEL <- current_model
      return(df)
    }))
    
    master_plan_tmp <- master_plan
    master_plan_tmp$TREATMENT <- as.character(master_plan_tmp$TREATMENT)
    rankings_df$item_id <- as.character(rankings_df$item_id)
    
    final_merged <- master_plan_tmp |> 
      inner_join(rankings_df, by = c("REP" = "REP", "IBLOCK" = "IBLOCK", "TREATMENT" = "item_id")) |> 
      arrange(REP, IBLOCK, rank)
      
    all_models_final_results[[current_model]] <- final_merged
  }
}

# --- 6. Results Consolidation & CSV Export ---
message("\nConsolidating multi-model results...")

if (length(all_models_final_results) > 0) {
  combined_final_df <- do.call(rbind, all_models_final_results)
  
  # Rearrange columns so MODEL is visible easily
  col_order <- c("MODEL", setdiff(names(combined_final_df), "MODEL"))
  combined_final_df <- combined_final_df[, col_order]
  
  results_dir <- file.path(base_dir, "results")
  if (!dir.exists(results_dir)) dir.create(results_dir)
  
  csv_file <- file.path(results_dir, paste0("AI_Creative_80_Rankings_", run_timestamp, ".csv"))
  
  write.csv(combined_final_df, csv_file, row.names = FALSE)
  
  message("--------------------------------------------------")
  message("✅ SUCCESS: Multi-Model Pipeline Completed!")
  message("Models Evaluated: ", paste(names(all_models_final_results), collapse=", "))
  message("Final CSV Exported to: ", csv_file)
  message("--------------------------------------------------")
  
  print(head(combined_final_df, 10))
} else {
  message("❌ Critical Error: No models produced valid results.")
}


