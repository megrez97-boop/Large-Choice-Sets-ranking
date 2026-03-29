# Alpha Lattice Simulation (t=20, k=2, r=2)
# 1 is the best, 2 is 2nd, ..., 20 is the last.
#=================================================================================
library(FielDHub)
library(BradleyTerry2)

# 1. Generate design with k=2
sim_design <- alpha_lattice(
  t = 20, 
  k = 2, 
  r = 2, 
  l = 1, 
  plotNumber = 101, 
  locationNames = "SimSite", 
  seed = 3080
)

# 2. Extract Field Book
fb <- sim_design$fieldBook
fb
# 3. Assign True Value (1 gets 20, 20 gets 1)
# for fb$TREATMENT, it just shows us the treatment's index
fb$TrueValue <- 21 - as.numeric(fb$ENTRY)

# 4. Loop through all blocks and return the results
results_list <- list()

# we use unique() on the REP and IBLOCK columns in "fb" to filter out a list of combinations for the next step
unique_blocks <- unique(fb[, c("REP", "IBLOCK")])

# Build a loop that repeats from the 1st row to the last row of 'unique_blocks', extracting IDs for the next filtering step.
for(i in 1:nrow(unique_blocks)) {
  rep_id <- unique_blocks[i, "REP"]
  blk_id <- unique_blocks[i, "IBLOCK"]
  
  # Filter the 'fb' dataset to extract all rows where REP equals rep_id and IBLOCK equals blk_id.
  block_data <- fb[fb$REP == rep_id & fb$IBLOCK == blk_id, ]

  # Identify the winner (the treatment with the highest TrueValue)
  winner <- block_data$TREATMENT[which.max(block_data$TrueValue)]
  
  # Identify the loser (the treatment with the lowest TrueValue)
  loser <- block_data$TREATMENT[which.min(block_data$TrueValue)]
  
  # Create a 1-row dataframe to record the current match result
  match_result <- data.frame(
    Match_ID = i,
    Winner = winner,
    Loser = loser
  )
  
  # Store this match result into the i-th position of our list
  results_list[[i]] <- match_result
}

final_results_df <- do.call(rbind, results_list)

# now the BT part
# to avoid a treatment never appear in the winner part and BT model cant recognize
all_players <- unique(c(as.character(final_results_df$Winner), as.character(final_results_df$Loser)))

# Convert 'Winner' and 'Loser' columns to factors 
# to avoid BT model treat the treatment's index as numerical number
final_results_df$Winner <- factor(final_results_df$Winner, levels = all_players)
final_results_df$Loser <- factor(final_results_df$Loser, levels = all_players)

# fit into BT model and save the result
bt_model <- BTm(1, Winner, Loser, data = final_results_df)
bt_abilities <- BTabilities(bt_model)

# print the result
cat("\n=== Final Simulated Ranking ===\n")
print(sort(bt_abilities[, "ability"], decreasing = TRUE))

