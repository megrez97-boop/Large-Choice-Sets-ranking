# install.packages("FielDHub")

library(FielDHub)

# Example 1: Generates a resolvable IBD of characteristics (t,k,r) = (12,4,2).
# 1-resolvable IBDs
ibd1 <- incomplete_blocks(t = 12,
                          k = 4,
                          r = 2,
                          seed = 1984)
ibd1$infoDesign
head(ibd1$fieldBook)

# Example 2: Generates a balanced resolvable IBD of characteristics (t,k,r) = (15,3,7).
# In this case, we show how to use the option data.
treatments <- paste("TX-", 1:15, sep = "")
ENTRY <- 1:15
treatment_list <- data.frame(list(ENTRY = ENTRY, TREATMENT = treatments))
head(treatment_list)
ibd2 <- incomplete_blocks(t = 15,
                          k = 3,
                          r = 7,
                          seed = 1985,
                          data = treatment_list)
ibd2$infoDesign
head(ibd2$fieldBook)

ibd <- incomplete_blocks(t = 250, k=2, r=2, seed=1234)

ibd <- incomplete_blocks(t = 252, k=3, r=2, seed=1234)

ibd <- incomplete_blocks(t = 256, k=4, r=2, seed=1234)

#=================================================================================
treatments <- paste("G-", 1:25, sep = "")
#> treatments
#[1] "G-1"  "G-2"  "G-3"  "G-4"  "G-5"  "G-6"  "G-7"  "G-8"  "G-9"  "G-10" "G-11" "G-12" "G-13" "G-14" "G-15" "G-16" "G-17" "G-18" "G-19" "G-20" "G-21"
#[22] "G-22" "G-23" "G-24" "G-25"

ENTRY <- 1:25
treatment_list <- data.frame(list(ENTRY = ENTRY, TREATMENT = treatments))
# treatment_list
#     ENTRY TREATMENT
# 1      1       G-1
# 2      2       G-2
# 3      3       G-3
# 4      4       G-4

alphalattice2 <- alpha_lattice(t = 25,
                               k = 5,
                               r = 3,
                               l = 1,
                               plotNumber = 1001,
                               locationNames = "A",
                               seed = 1945,
                               data = treatment_list)
alphalattice2$infoDesign
head(alphalattice2$fieldBook, 100)


#alpha design---------------------------------------------------------------
alphalattice1 <- alpha_lattice(
t = 180,
k = 3,
r = 3,
l = 1,
plotNumber = 101,
locationNames = "group",
seed = 100
)
alphalattice1$infoDesign
head(alphalattice1$fieldBook, 10)
#------------------------------------------------------------------------------
# Simple Alpha Lattice Example (t=20, k=4, r=2)
# m = 5 (blocks per replication), g = 20 (total treatments/genotypes)
simple_alpha <- alpha_lattice(
  t = 20, 
  k = 4, 
  r = 2, 
  l = 1, 
  plotNumber = 101, 
  locationNames = "Site1", 
  seed = 42
)

# Show design information
simple_alpha$infoDesign

# Show the first few rows of the field book
head(simple_alpha$fieldBook, 10)

#=================================================================================
# Alpha Lattice Simulation (t=20, k=2, r=2)
# 1 is the best, 2 is 2nd, ..., 20 is the last.
#=================================================================================

# 1. Generate design with k=2
sim_design <- alpha_lattice(
  t = 20, 
  k = 2, 
  r = 2, 
  l = 1, 
  plotNumber = 101, 
  locationNames = "SimSite", 
  seed = 123
)

# 2. Extract Field Book
fb <- sim_design$fieldBook

# Check column names to avoid "undefined columns" error
# print(colnames(fb)) 

# 3. Assign True Value (1 gets 20, 20 gets 1)
fb$TrueValue <- 21 - as.numeric(fb$ENTRY)

# 4. Loop through all blocks and return the results
cat("\n--- Alpha Lattice Simulation Results (by Block) ---\n")

# Corrected column name to REPLICATE
unique_blocks <- unique(fb[, c("REPLICATE", "BLOCK")])

for(i in 1:nrow(unique_blocks)) {
  rep_id <- unique_blocks$REPLICATE[i]
  blk_id <- unique_blocks$BLOCK[i]
  
  # Filter data for this block
  block_data <- fb[fb$REPLICATE == rep_id & fb$BLOCK == blk_id, ]

  
  cat(paste0("\nReplication: ", rep_id, " | Block: ", blk_id, "\n"))
  cat("Treatments (ENTRY):", paste(block_data$ENTRY, collapse = ", "), "\n")
  cat("Simulated Performance Score:", paste(block_data$TrueValue, collapse = ", "), "\n")
  
  # Identify the local winner in the block
  winner <- block_data$ENTRY[which.max(block_data$TrueValue)]
  cat("Local Winner in Block:", winner, "\n")
}


