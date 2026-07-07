if (rstudioapi::isAvailable()) {
  try(setwd(dirname(rstudioapi::getActiveDocumentContext()$path)), silent = TRUE)
}

find_project_root <- function() {
  curr <- getwd()
  for (i in 1:5) {
    if (dir.exists(file.path(curr, "studies"))) {
      return(curr)
    }
    curr <- dirname(curr)
  }
  return(getwd())
}
proj_root <- find_project_root()

df <- read.csv(file.path(proj_root, "studies/LLM_Ranking_Engine_v1/results/AI_Detailed_Rankings_MultiModel_20260623_031154.csv"))
max_r <- max(df$REP)
global_rankings <- list()
for (r in 1:max_r) {
  df_r <- df[df$REP <= r, ]
  avg_ranks <- aggregate(rank ~ TREATMENT, data = df_r, FUN = mean)
  sorted <- avg_ranks$TREATMENT[order(avg_ranks$rank, decreasing = FALSE)]
  global_rankings[[r]] <- as.character(sorted)
}
output_rds <- file.path(proj_root, "studies/replication_convergence/gemini_global_rankings_r20.rds")
saveRDS(global_rankings, output_rds)
cat("Data aggregated and saved to", output_rds, "\n")
