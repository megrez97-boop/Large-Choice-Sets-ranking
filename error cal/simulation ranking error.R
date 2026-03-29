# =================================================================================
# Tool: Multi-k Automated Alpha Lattice Simulation and Error Analysis
# =================================================================================

library(FielDHub)
library(BradleyTerry2)
library(ggplot2)

# Notice the new parameter 'k_values' which accepts a vector (e.g., c(2, 4, 5))
run_multi_k_analysis <- function(t_val = 20, k_values = c(2, 4, 5), r_limit = 50, seed_val = 123) {
  
  final_data <- data.frame()
  cat("Start Simulation for multiple k values...\n")
  
  # --- OUTER LOOP: Iterate through each Block Size (k) ---
  for (current_k in k_values) {
    cat(sprintf("Simulating for k = %d...\n", current_k))
    
    # --- INNER LOOP: Iterate through Replications (r) ---
    for (current_r in 2:r_limit) {
      
      # [Step A & B] Generate Design & Assign True Value
      # We use try() here just in case a specific t and k combination is mathematically invalid
      design <- try(alpha_lattice(t = t_val, k = current_k, r = current_r, seed = seed_val), silent = TRUE)
      if (inherits(design, "try-error")) next # Skip to next loop if design fails
      
      fb <- design$fieldBook
      fb$TrueValue <- (t_val + 1) - as.numeric(fb$ENTRY)
      
      # [Step C] Extract match outcomes
      block_tags <- paste(fb$REP, fb$IBLOCK, sep = "_")
    
    matches <- do.call(rbind, lapply(split(fb, block_tags), function(block) {
      
      # Generate all possible pairs (combinations of 2) within this block
      # If k=2, this creates 1 pair. If k=4, this creates 6 pairs.
      pairs <- combn(block$TREATMENT, 2, simplify = FALSE)
      
      # For each pair, determine who is the winner based on TrueValue
      block_matches <- do.call(rbind, lapply(pairs, function(p) {
        
        # Get the TrueValue for the two treatments in this pair
        val1 <- block$TrueValue[block$TREATMENT == p[1]]
        val2 <- block$TrueValue[block$TREATMENT == p[2]]
        
        # The one with the higher TrueValue wins
        if (val1 > val2) {
          data.frame(Winner = p[1], Loser = p[2])
        } else {
          data.frame(Winner = p[2], Loser = p[1])
        }
      }))
      
      return(block_matches)
    }))
      
      # [Step D] Execute BT Model
      all_players <- unique(c(as.character(matches$Winner), as.character(matches$Loser)))
      matches$Winner <- factor(matches$Winner, levels = all_players)
      matches$Loser  <- factor(matches$Loser, levels = all_players)
      
      fit <- try(BTm(1, Winner, Loser, data = matches), silent = TRUE)
      
      # [Step E] SAFELY Calculate Error (Dynamic Ranking)
      if (!inherits(fit, "try-error")) {
        abilities <- try(BTabilities(fit), silent = TRUE)
        if (!inherits(abilities, "try-error")) {
          
          player_ids <- as.numeric(gsub("[^0-9]", "", rownames(abilities)))
          player_abs <- abilities[, "ability"]
          res_df <- data.frame(ID = player_ids, Ability = player_abs)
          res_df <- res_df[!is.na(res_df$Ability), ]
          
          if (nrow(res_df) > 2) {
            res_df$TrueRank <- rank(res_df$ID) 
            res_df$PredRank <- rank(-res_df$Ability)
            error_score <- sum(abs(res_df$PredRank - res_df$TrueRank))
            
            # Record the result, including which 'k' this belongs to (as a factor for grouping)
            final_data <- rbind(final_data, data.frame(k = as.factor(current_k), r = current_r, error = error_score))
          }
        }
      }
    }
  }
  
  if (nrow(final_data) == 0) {
    stop("Error: All models failed. Please check your t and k values, or increase r_limit.")
  }
  
  cat("Simulation Complete! Plotting...\n")
  
  # Visualization: Now plotting multiple lines differentiated by color
  p <- ggplot(final_data, aes(x = r, y = error, color = k)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2, alpha = 0.7) +
    labs(title = paste("Simulation Analysis (t =", t_val, ")"),
         subtitle = "Comparing Error Convergence Across Different Block Sizes (k)",
         x = "Number of Replications (r)", 
         y = "Total Rank Error Score",
         color = "Block Size (k)") + # Legend title
    theme_minimal()
  
  return(list(data = final_data, plot = p))
}

# =================================================================================
# Execute the Final Multi-k Function
# =================================================================================

# Run the simulation for k = 2, 4, 5
result <- run_multi_k_analysis(t_val = 240, k_values = 3, r_limit = 20, seed_val = 1006)

# Display the plot
print(result$plot)