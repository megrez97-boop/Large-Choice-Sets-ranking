# --- RDS to CSV Converter (v1.1 - Bulletproof Path) ---

# 1. Define the absolute path to ensure it works regardless of working directory
target_dir <- "C:/Users/User/Documents/R/Thesis/studies/AI ranking test"
rds_path <- file.path(target_dir, "gemini_pro_results_k4_full.rds")
csv_output <- file.path(target_dir, "Gemini_Ranking_Results_Readable.csv")

message("Checking for file at: ", rds_path)

# 2. Check and Convert
if (file.exists(rds_path)) {
  message("Reading binary RDS file...")
  res_list <- readRDS(rds_path)
  
  if (length(res_list) > 0) {
    # Convert list to data frame
    res_df <- data.frame(
      Block_ID = names(res_list),
      Ranking = unlist(res_list),
      stringsAsFactors = FALSE
    )
    
    # Export to CSV
    write.csv(res_df, csv_output, row.names = FALSE)
    
    message("------------------------------------------")
    message("✅ SUCCESS: Results converted to CSV!")
    message("Total records found: ", nrow(res_df))
    message("File Location: ", csv_output)
    message("------------------------------------------")
  } else {
    message("⚠️ Warning: The RDS file was found but it is empty.")
  }
} else {
  message("❌ ERROR: Could not find 'gemini_pro_results.rds'.")
  message("Looking in: ", target_dir)
  message("\nPossible solutions:")
  message("1. Run the 'gemini_evaluator.R' script first to generate data.")
  message("2. In RStudio, go to Session -> Set Working Directory -> To Source File Location.")
}
