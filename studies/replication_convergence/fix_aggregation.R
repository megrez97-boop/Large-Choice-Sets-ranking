df <- read.csv("C:/Users/User/Documents/R/Thesis/studies/LLM_Ranking_Engine_v1/results/AI_Detailed_Rankings_MultiModel_20260623_031154.csv")
max_r <- max(df$REP)
global_rankings <- list()
for (r in 1:max_r) {
  df_r <- df[df$REP <= r, ]
  avg_ranks <- aggregate(rank ~ TREATMENT, data = df_r, FUN = mean)
  sorted <- avg_ranks$TREATMENT[order(avg_ranks$rank, decreasing = FALSE)]
  global_rankings[[r]] <- as.character(sorted)
}
output_rds <- "C:/Users/User/Documents/R/Thesis/studies/replication_convergence/gemini_global_rankings_r20.rds"
saveRDS(global_rankings, output_rds)
cat("Data aggregated and saved to", output_rds, "\n")
