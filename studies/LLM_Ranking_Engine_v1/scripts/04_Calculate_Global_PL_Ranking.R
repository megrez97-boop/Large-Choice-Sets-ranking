# =================================================================================
# 04. Calculate Final Global Ranking using Plackett-Luce Model
# Author: Megrez (AI Partner) | Mandate: No-LaTeX Explanations
# =================================================================================

library(dplyr)
library(PlackettLuce)

# --- 1. Settings & Path ---
base_dir <- "C:/Users/User/Documents/R/Thesis/studies/LLM_Ranking_Engine_v1"
setwd(base_dir)

# Define the target result file (You can change this to any valid result CSV)
results_file <- file.path("results", "AI_Detailed_Rankings_MultiModel_20260616_082316.csv")

if (!file.exists(results_file)) {
  stop("Result file not found! Please check the file path.")
}

message("Loading data from: ", results_file)
df <- read.csv(results_file, stringsAsFactors = FALSE)

# Clean up empty rows if any
df <- df |> filter(!is.na(rank) & !is.na(TREATMENT))

# --- 2. Build the Ordering Matrix for Plackett-Luce ---
# Group by Replicate and Block
blocks <- df |> group_by(REP, IBLOCK) |> group_split()

message(sprintf("Found %d blocks for PL aggregation.", length(blocks)))

# Determine the block size (k)
k_val <- max(sapply(blocks, nrow))

# Create a matrix where each row represents a block
# and columns contain the TREATMENT IDs ordered from 1st to last place
pl_matrix <- matrix(NA, nrow = length(blocks), ncol = k_val)

for (i in seq_along(blocks)) {
  block <- blocks[[i]]
  # Sort by rank (ascending: 1 is best, 4 is worst)
  ordered_items <- block$TREATMENT[order(block$rank)]
  
  # Fill the row (pad with NA if block size is smaller than k_val)
  pl_matrix[i, 1:length(ordered_items)] <- as.character(ordered_items)
}

message("Building Plackett-Luce Rankings Object...")

# Create rankings object (input = "orderings" means sequence of items from best to worst)
R_obj <- PlackettLuce::as.rankings(pl_matrix, input = "orderings")

# --- 3. Fit the Plackett-Luce Model ---
message("Fitting the Plackett-Luce Model...")
fit_pl <- try(PlackettLuce::PlackettLuce(R_obj), silent = TRUE)

if (inherits(fit_pl, "try-error")) {
  stop("Failed to fit Plackett-Luce model. Check if the network is fully connected.")
}

# Extract Item Parameters (Worth/Abilities)
# The higher the value, the better the global rank
item_worth <- PlackettLuce::itempar(fit_pl, log = FALSE)

# Convert to a neat data frame
final_ranking_df <- data.frame(
  ITEM_ID = names(item_worth),
  PL_Worth = as.numeric(item_worth)
) |> 
  arrange(desc(PL_Worth)) |> 
  mutate(Global_Rank = row_number())

message("\n==================================================")
message("🏆 FINAL GLOBAL RANKING (PLACKETT-LUCE) 🏆")
message("==================================================")
print(final_ranking_df)

# --- 4. Export Results ---
timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
output_file <- file.path("results", paste0("Global_PL_Ranking_", timestamp, ".csv"))

write.csv(final_ranking_df, output_file, row.names = FALSE)
message("\n✅ Final global ranking exported to: ", output_file)
