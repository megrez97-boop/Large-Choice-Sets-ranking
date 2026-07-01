library(BradleyTerry2)
library(FielDHub)

#Assuming we have a suggestion list (sl)
design <- alpha_lattice(
  t = 20,
  k = 2,
  r = 2,
  seed = 123
)

# Parameter settings:
# t = nrow(sl) # remember to remove the title or column name
# k = 2        # or maybe 3
# r = 2        # tbd (to be determined)
# data = sl    # need to confirm how FielDHub reads the data

# Extracting the results:
fb <- design$fieldBook
final_fb <- fb[, c("REP", "IBLOCK", "ENTRY")]
fb$TrueValue <- max(as.numeric(fb$ENTRY)) + 1 - as.numeric(fb$ENTRY)
#build a place to save data for later
results_list <- list()
# Extract a distinct list of block combinations
unique_blocks <- unique(fb[, c("REP", "IBLOCK")])
# Build a loop that repeats from the 1st row to the last row of 'unique_blocks', extracting IDs for the next filtering step.
for(i in 1:nrow(unique_blocks)) {
  rep_id <- unique_blocks[i, "REP"]
  blk_id <- unique_blocks[i, "IBLOCK"]
  
  # Filter the 'fb' dataset to extract all rows where REP equals rep_id and IBLOCK equals blk_id.
  block_data <- fb[fb$REP == rep_id & fb$IBLOCK == blk_id, ]

  # Identify the winner (the treatment with the highest TrueValue)
  winner <- block_data$ENTRY[which.max(block_data$TrueValue)]
  
  # Identify the loser (the treatment with the lowest TrueValue)
  loser <- block_data$ENTRY[which.min(block_data$TrueValue)]
  
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

# Convert 'Winner' and 'Loser' columns to factors 
# This prevents the BT model from treating the treatment IDs as numeric values
final_results_df$Winner <- as.factor(final_results_df$Winner)
final_results_df$Loser <- as.factor(final_results_df$Loser)

# View the first few rows to confirm it looks correct
head(final_results_df)


#gemini said better user dplyr， but I havent have a look

# now the BT part
# to avoid a treatment never appear in the winner part and BT model cant recognize
all_players <- unique(c(as.character(final_results_df$Winner), as.character(final_results_df$Loser)))
# to avoid BT model treat the treatment's index as numerical number
final_results_df$Winner <- factor(final_results_df$Winner, levels = all_players)
final_results_df$Loser <- factor(final_results_df$Loser, levels = all_players)

# fit into BT model and save the result
bt_model <- BTm(1, Winner, Loser, data = final_results_df)
bt_abilities <- BTabilities(bt_model)

#print the result
sort(bt_abilities[, "ability"], decreasing = TRUE)