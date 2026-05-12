library(agricolae)
library(readxl)
library(dplyr)

# --- 1. Path & Settings ---
base_dir <- "C:/Users/User/Documents/R/Thesis/studies/AI ranking test"
file_path <- file.path(base_dir, "data for thesis", "data partition.xlsx")
output_dir <- base_dir
design_seed <- 2026
n_items <- 48

# --- FULL EXPERIMENT SETTINGS ---
r_reps <- 2     # Full 40 replicates as per thesis plan
k_value <- 4      # Focus on K=4
test_mode <- FALSE # Turning off test mode for full execution
# ---------------------------------

message("--- STARTING FULL GEMINI EVALUATION v1.45 (K=4, r=40) ---")

# --- 2. Extract Task Context ---
task_context_df <- read_excel(file_path, sheet = 2, col_names = FALSE)
all_text <- unlist(task_context_df) %>% as.character()
mission_context <- all_text[which.max(nchar(all_text))]

raw_items <- read_excel(file_path, sheet = 1)[1:n_items, ]
colnames(raw_items)[1:2] <- c("ITEM_ID", "ITEM_CONTENT")
raw_items$ITEM_ID <- as.character(raw_items$ITEM_ID)

# --- 3. Generate Design ---
set.seed(design_seed)
message(sprintf("Generating Alpha Design for K=%d, r=%d...", k_value, r_reps))
d_out <- design.alpha(raw_items$ITEM_ID, k = k_value, r = r_reps, seed = design_seed)
d_book <- d_out$book

# Efficiency Factor info (for your records)
message("Efficiency Factor (E): ", d_out$statistics$efficiency)

# Column Renaming
id_col_name <- setdiff(colnames(d_book), c("replication", "block", "plots"))
colnames(d_book)[colnames(d_book) == id_col_name[1]] <- "ITEM_ID"
d_book$ITEM_ID <- as.character(d_book$ITEM_ID)
d_book$K_SIZE <- k_value
master_plan <- d_book %>% left_join(raw_items, by = "ITEM_ID")

# --- 4. Gemini Evaluation ---
results_file <- file.path(output_dir, "gemini_pro_results_k4_full.rds")

# Load existing if any to allow resumption
gemini_results <- if(file.exists(results_file)) readRDS(results_file) else list()

blocks <- master_plan %>% group_by(replication, block) %>% group_split()
message(sprintf("Total blocks to process: %d", length(blocks)))

for (i in 1:length(blocks)) {
  bl <- blocks[[i]]
  block_id <- paste("K", k_value, "R", bl$replication[1], "B", bl$block[1], sep="_")
  
  if (block_id %in% names(gemini_results)) next
  
  message(sprintf("[%d/%d] Processing Block %s...", i, length(blocks), block_id))
  
  items_text <- paste(paste0("[ID_", bl$ITEM_ID, "]: ", bl$ITEM_CONTENT), collapse = "\n")
  
  prompt <- paste0(
    "[STRICT INSTRUCTION: DO NOT PROVIDE ANY PROGRESS SUMMARY. DO NOT GREET. DO NOT EXPLAIN.]\n",
    "You are a professional airport service quality evaluator.\n",
    "MISSION CONTEXT: ", mission_context, "\n\n",
    "TASK:\nRank ALL the airport guidance items listed below from best to worst.\n\n",
    "REQUIREMENTS:\n",
    "1. You MUST include EVERY item ID: ", paste(bl$ITEM_ID, collapse=", "), "\n",
    "2. Output ONLY the ranking as 'ID_1 > ID_2 > ID_3 > ID_4'.\n\n",
    "ITEMS:\n", items_text
  )
  
  temp_f <- tempfile(fileext = ".txt")
  writeLines(prompt, temp_f, useBytes = TRUE)
  cmd <- sprintf("gemini --prompt \"$(Get-Content -Raw '%s')\"", temp_f)
  
  response_raw <- tryCatch({
    shell(cmd, intern = TRUE, translate = TRUE)
  }, error = function(e) return("Error"))
  
  ranking_line <- grep(">", response_raw, value = TRUE)
  final_ans <- if(length(ranking_line) > 0) ranking_line[length(ranking_line)] else "Error"
  
  gemini_results[[block_id]] <- trimws(final_ans)
  
  # Checkpoint every 10 blocks
  if (i %% 10 == 0) {
    saveRDS(gemini_results, results_file)
    message("--> Checkpoint saved.")
  }
  
  if(file.exists(temp_f)) file.remove(temp_f)
}

saveRDS(gemini_results, results_file)
message("--- FINAL SUCCESS: All 480 blocks processed ---")
