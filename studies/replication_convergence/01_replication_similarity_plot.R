# Script: 01_replication_similarity_plot.R
# Purpose: Visualizing the convergence of Global Ranking across replications
# Metrics: RBO (Rank-Biased Overlap) vs. Traditional Kendall's Tau

library(ggplot2)
library(wdm)

# 自動偵測工作目錄並切換
if (rstudioapi::isAvailable()) {
  try(setwd(dirname(rstudioapi::getActiveDocumentContext()$path)), silent = TRUE)
}

# 尋找專案根目錄 (向上搜尋包含 studies 且包含 sigir2024-rbo 或 delivery 的資料夾)
find_project_root <- function() {
  curr <- getwd()
  for (i in 1:5) {
    if (dir.exists(file.path(curr, "studies")) && 
        (dir.exists(file.path(curr, "sigir2024-rbo")) || dir.exists(file.path(curr, "delivery")))) {
      return(curr)
    }
    curr <- dirname(curr)
  }
  return(getwd())
}
proj_root <- find_project_root()

# 載入我們 6/20 整合的 RBO 腳本 (Corsi & Urbano 2024 實作版)
# 優先載入 sigir2024-rbo，其次載入 delivery 的備份，最後載入 library 中的檔案
rbo_path <- file.path(proj_root, "sigir2024-rbo/rbo/R/rbo.R")
if (!file.exists(rbo_path)) {
  rbo_path <- file.path(proj_root, "delivery/final code/modules/rbo/rbo.R")
}
if (!file.exists(rbo_path)) {
  rbo_path <- file.path(proj_root, "library/Related paper/RBO and kendalls tau/sigir2024-rbo-main/rbo/R/rbo.R")
}

if(file.exists(rbo_path)) {
  source(rbo_path)
  message("✅ 成功載入 RBO 核心演算法: ", rbo_path)
} else {
  stop("無法找到 RBO 腳本。請確認專案完整性。")
}

# --- 1. 資料讀取 (真實 LLM 數據) ---
rds_path <- file.path(proj_root, "studies/replication_convergence/gemini_global_rankings_t20_r20.rds")
if (!file.exists(rds_path)) {
  stop("找不到真實數據 RDS 檔案！請確認 Gemini 評估程式已經執行完畢。")
}

global_rankings <- readRDS(rds_path)
max_r <- length(global_rankings)
items <- global_rankings[[max_r]] # 用最後一次收斂的 items 作為基準

# --- 2. 計算 MEASURE ---
results <- data.frame(
  Replication = integer(),
  Metric = character(),
  Similarity = numeric()
)

# 比較 GlobalRanking(r) vs. GlobalRanking(r-1)
for (r in 2:max_r) {
  rank_curr <- global_rankings[[r]]
  rank_prev <- global_rankings[[r-1]]
  
  # =====================================================================
  # Algorithm 1: Tie-Aware RBO-a (via sigir2024-rbo)
  # =====================================================================
  rbo_score <- tryCatch({
    # [Step 1: Input Parsing & Parameters]
    # Pass raw string arrays to rbo(). Defaults to ties="a" (tie-aware expectation).
    val <- rbo(rank_curr, rank_prev, p = 0.9)
    
    # --- Internal Source Code Execution Logic (Under the hood) ---
    # [Step 2: Scan for Tie Groups] 
    # `flatten(x, y)` scans the list, identifies tied items, and assigns top & bottom rank boundaries.
    
    # [Step 3: Calculate Overlap Matrix]
    # `cc.a(S, L)` creates the contribution matrix. Tied items crossing an evaluation depth
    # are assigned fractional overlaps (e.g., 0.5) to simulate all random permutations.
    
    # [Step 4: Geometric Weighting & Extrapolation]
    # Overlaps are multiplied by geometric weights (p^d). The algorithm then uses the 
    # `sec3.ext` infinite geometric series to extrapolate unobserved tail overlap.
    # -------------------------------------------------------------
    
    # [Step 5: Extract Final Point Estimate]
    # The function returns an array [min, max, ext, res]. Since our item universe is closed,
    # we explicitly extract "ext" as the most mathematically sound point estimate.
    as.numeric(val["ext"])
  }, error = function(e) { NA })
  
  # =====================================================================
  # Algorithm 2: Kendall's Tau-b (via wdm package)
  # =====================================================================
  # [Step 1: Read Rankings & Input]
  # Convert string items into pure numeric rank arrays for the wdm package.
  num_curr <- match(items, rank_curr)
  num_prev <- match(items, rank_prev)
  
  # [Step 2 & 3: Pairwise Concordance & Tie Adjustment]
  # We pass the numeric arrays to wdm(). The data is instantly handed off to a 
  # pre-compiled C++ (Rcpp) backend. 
  # - The C++ engine generates all pairwise matchups in O(N log N) time.
  # - It tallies Concordant (+1) and Discordant (-1) pairs.
  # - CRITICAL: Any pairs sharing the exact same rank (Ties) are flagged as unresolved 
  #   and explicitly discarded from the denominator to prevent score inflation.
  tau_score <- wdm(num_curr, num_prev, method = "kendall")
  
  results <- rbind(results, data.frame(Replication = r, Metric = "RBO (p=0.9)", Similarity = rbo_score))
  results <- rbind(results, data.frame(Replication = r, Metric = "Traditional Tau", Similarity = tau_score))
}

results <- na.omit(results)

# --- 3. DATAVIZ 視覺化 ---
p <- ggplot(results, aes(x = Replication, y = Similarity, color = Metric, group = Metric)) +
  geom_line(linewidth = 1.2, alpha = 0.8) +
  geom_point(size = 3) +
  theme_minimal() +
  labs(
    title = "Convergence of Global Ranking Over Replications (Real Gemini Data)",
    subtitle = "Similarity between GlobalRanking(r) and GlobalRanking(r-1)",
    x = "Replication (r)",
    y = "Similarity / Correlation Coefficient",
    color = "Evaluation Metric"
  ) +
  scale_x_continuous(breaks = 2:max_r) +
  scale_y_continuous(limits = c(0, 1)) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    legend.position = "bottom",
    text = element_text(size = 12)
  )

# 儲存圖表
output_path <- file.path(proj_root, "studies/replication_convergence/Replication_Convergence_Plot_t20.png")
ggsave(output_path, plot = p, width = 8, height = 6, dpi = 300)

cat("繪圖完成！圖表已儲存至:", output_path, "\n")
