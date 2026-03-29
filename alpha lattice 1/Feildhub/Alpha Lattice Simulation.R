# Alpha Lattice Simulation (t=20, k=2, r=2)
# 1 is the best, 2 is 2nd, ..., 20 is the last.
#=================================================================================
library(FielDHub)
# Generate design with k=2
sim_design <- alpha_lattice(
  t = 20, 
  k = 2, 
  r = 2, 
  l = 1, 
  plotNumber = 101, 
  locationNames = "SimSite", 
  seed = 123
)

# Extract Field Book
fb <- sim_design$fieldBook

# Assign True Value (1 gets 20, 20 gets 1)
fb$TrueValue <- 21 - as.numeric(fb$ENTRY)

# when we look at ""fb""
#   ID LOCATION PLOT REP IBLOCK UNIT ENTRY TREATMENT TrueValue
#1   1  SIMSITE  101   1      1    1    15      G-15         6
#2   2  SIMSITE  102   1      1    2    18      G-18         3
#3   3  SIMSITE  103   1      2    1    16      G-16         5
#4   4  SIMSITE  104   1      2    2     6       G-6        15
#                              .
#                              .
#                              .
#37 37  SIMSITE  217   2      9    1    17      G-17         4
#38 38  SIMSITE  218   2      9    2    16      G-16         5
#39 39  SIMSITE  219   2     10    1    14      G-14         7
#40 40  SIMSITE  220   2     10    2    12      G-12         9

# Initialize an empty list to store the results of each match
results_list <- list()
#we use unique() on the REP and IBLOCK columns in ""fb"" to filter out a list of combinations for the next step
unique_blocks <- unique(fb[, c("REP", "IBLOCK")])



# when we look at ""unique_blocks"", we have
#   REP IBLOCK
#1    1      1
#3    1      2
#5    1      3
#7    1      4
#9    1      5
#11   1      6
#        .
#        .
#        .
#33   2      7
#35   2      8
#37   2      9
#39   2     10

for(i in 1:nrow(unique_blocks)) {
  rep_id <- unique_blocks[i, "REP"]
  blk_id <- unique_blocks[i, "IBLOCK"]
  
  # Filter the dataset to extract the competing treatments in the current block
  block_data <- fb[fb$REP == rep_id & fb$IBLOCK == blk_id, ]
  
  # Identify the winner (highest TrueValue) and loser (lowest TrueValue) silently
  winner <- block_data$ENTRY[which.max(block_data$TrueValue)]
  loser <- block_data$ENTRY[which.min(block_data$TrueValue)]
  
  # Create a 1-row dataframe for the current match and store it in the list
  results_list[[i]] <- data.frame(
    Match_ID = i,
    REP = rep_id,        # Include REP and IBLOCK for easier tracking later
    IBLOCK = blk_id,
    Winner = winner,
    Loser = loser
  )
}

# After the loop, combine all the 1-row dataframes into one complete dataframe
final_results_df <- do.call(rbind, results_list)

# View the first few rows of the final dataframe
head(final_results_df)