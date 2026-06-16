# =================================================================================
# Gemini Neutral Evaluator: Alpha Lattice with Bias Control
# Author: Megrez (AI Partner) | Mandate: No-LaTeX, Shuffling, ID Obfuscation, Schema
# =================================================================================

library(readxl)
library(jsonlite)
library(dplyr)
library(httr)
library(FielDHub)

# --- 1. Settings & Path ---
base_dir <- "C:/Users/User/Documents/R/Thesis/studies/AI ranking test"
excel_path <- file.path(base_dir, "data for thesis", "data partition -  only 12.xlsx")

# --- IMPORTANT: USER MUST PROVIDE GEMINI_API_KEY ---
# For now, I'll use a placeholder. In a real run, use Sys.setenv(GEMINI_API_KEY = "your_key")
api_key <- Sys.getenv("GEMINI_API_KEY")
gemini_url <- paste0("https://generativelanguage.googleapis.com/v1beta/models/gemini-1.5-flash:generateContent?key=", api_key)

# Alpha Lattice Parameters
k_val <- 4  
r_rep <- 2  
seed_val <- 2026

# --- 2. Data & Design Initialization ---
item_db <- read_excel(excel_path, sheet = 1, col_names = FALSE)
colnames(item_db) <- c("ITEM_ID", "ITEM_CONTENT")
item_db$ITEM_ID <- as.character(item_db$ITEM_ID)

mission_context <- colnames(read_excel(excel_path, sheet = 2))[1]

# Generate Alpha Design
set.seed(seed_val)
alpha_design_obj <- alpha_lattice(t = nrow(item_db), k = k_val, r = r_rep, seed = seed_val)
master_plan <- alpha_design_obj$fieldBook

# --- 3. Neutral Utility Functions ---

# Function to generate a random 2-letter suffix for ID obfuscation
generate_obfuscated_id <- function(original_id) {
  # We use a mapping table to ensure we can reverse it later
  # Format: "Sug_[Random2]"
  suffix <- paste0(sample(LETTERS, 2, replace = TRUE), collapse = "")
  return(paste0("Sug_", suffix))
}

# --- 4. The Neutral Gemini Evaluator ---
evaluate_block_gemini_neutral <- function(target_ids) {
  # 1. Prepare Data with Shuffling and Obfuscation
  expected_ids <- as.character(target_ids)
  
  # Create a local mapping for this block
  mapping_df <- data.frame(
    original_id = expected_ids,
    obf_id = sapply(expected_ids, function(x) paste0("Sug_", paste0(sample(LETTERS, 2), collapse=""))),
    stringsAsFactors = FALSE
  )
  
  # Get content, Shuffle, and Apply Obfuscated IDs
  block_items <- item_db |> 
    filter(ITEM_ID %in% expected_ids) |> 
    left_join(mapping_df, by = c("ITEM_ID" = "original_id")) |>
    select(id = obf_id, content = ITEM_CONTENT) |>
    sample_n(n()) # <--- CRITICAL: Intra-block Shuffling
  
  # Store the presentation order to check for position bias later
  presentation_order <- block_items$id
  
  # 2. Define JSON Schema for Gemini
  response_schema <- list(
    type = "object",
    properties = list(
      rankings = list(
        type = "array",
        items = list(
          type = "object",
          properties = list(
            item_id = list(type = "string"),
            rank = list(type = "number"),
            reason = list(type = "string")
          ),
          required = list("item_id", "rank", "reason")
        )
      )
    ),
    required = list("rankings")
  )

  prompt_text <- paste0(
    "You are a professional airport service quality evaluator.\n",
    "CONTEXT:\n", mission_context, "\n\n",
    "DATA (Evaluate these items):\n", toJSON(list(items = block_items), auto_unbox = TRUE, pretty = TRUE), "\n\n",
    "TASK:\n",
    "Rank ALL items from best to worst based on the context.\n",
    "STRICTLY follow the provided JSON schema."
  )

  # 3. Call Gemini API
  payload <- list(
    contents = list(list(parts = list(list(text = prompt_text)))),
    generationConfig = list(
      response_mime_type = "application/json",
      response_schema = response_schema,
      temperature = 0
    )
  )

  res <- POST(
    url = gemini_url,
    body = payload,
    encode = "json"
  )
  
  if (status_code(res) == 200) {
    raw_res <- content(res)
    # Gemini returns JSON inside the first candidate's first part's text
    json_str <- raw_res$candidates[[1]]$content$parts[[1]]$text
    parsed <- fromJSON(json_str)$rankings
    
    # Reverse Obfuscation and add Metadata
    parsed <- parsed |>
      left_join(mapping_df, by = c("item_id" = "obf_id")) |>
      mutate(
        original_id = original_id,
        presentation_pos = match(item_id, presentation_order) # Tracking position bias
      ) |>
      select(original_id, rank, reason, presentation_pos)
    
    return(parsed)
  } else {
    message("❌ API Error: ", status_code(res))
    return(NULL)
  }
}

# --- 5. Execution Loop (Small Scale Test) ---
blocks_list <- master_plan |> group_by(REP, IBLOCK) |> group_split()
results_storage <- list()

message("Starting Neutral Evaluation Test with Gemini API...")

# Testing first 3 blocks as a pilot
for (i in 1:min(3, length(blocks_list))) {
  current_block <- blocks_list[[i]]
  target_ids <- current_block$TREATMENT
  
  block_label <- paste0("Rep", current_block$REP[1], "_B", current_block$IBLOCK[1])
  message(sprintf("\n[Block %d] Processing %s...", i, block_label))
  
  res <- evaluate_block_gemini_neutral(target_ids)
  
  if (!is.null(res)) {
    res$block_id <- block_label
    results_storage[[i]] <- res
    print(res |> select(original_id, rank, presentation_pos))
  }
}

final_df <- bind_rows(results_storage)
write.csv(final_df, "Gemini_Neutral_Test_Results.csv", row.names = FALSE)
message("\n✅ Pilot test completed. Results saved to Gemini_Neutral_Test_Results.csv")
