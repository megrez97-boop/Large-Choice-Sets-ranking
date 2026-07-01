# =================================================================================
# Integrated Ranking Evaluation Pipeline
# Components: Gemini API + Plackett-Luce Global Ranking + Rank-Biased Overlap (RBO)
# =================================================================================

library(dplyr)
library(PlackettLuce)

# --- 1. Paths & Initialization ---
# 作為最終交付版本，改用「相對路徑」以確保跨電腦的相容性 (Portability)。
# 只要整個 Thesis 資料夾結構不變，打包傳給任何人（如教授）都能直接執行。
# (執行前請確認 RStudio 的工作目錄已設定為此腳本所在的 final code 資料夾)

rbo_path <- "modules/rbo/rbo.R"
gemini_script <- "scripts/01_API_Gemini_Evaluator.R"
results_dir <- "results"

# 載入 RBO 核心演算法
if(file.exists(rbo_path)) {
  source(rbo_path)
  message("✅ 成功載入 RBO 核心演算法")
} else {
  warning("找不到 rbo.R 腳本")
}

# --- 2. 執行 Gemini API 評分引擎 ---
# 取消下方的註解即可執行 API 評測 (注意：會耗用 API Quota 並花費等待時間)
# source(gemini_script)

# --- 3. 讀取評分結果並執行 Plackett-Luce 全局聚合 ---
# 這裡自動抓取最新一次的 API 評分結果 CSV
csv_files <- list.files(results_dir, pattern = "AI_Detailed_Rankings_MultiModel_.*\\.csv", full.names = TRUE)

if(length(csv_files) > 0) {
  latest_csv <- max(csv_files)
  message("📊 讀取最新 AI 評分資料: ", basename(latest_csv))
  df <- read.csv(latest_csv, stringsAsFactors = FALSE)
  
  # 清理空值
  df <- df |> filter(!is.na(rank) & !is.na(TREATMENT))
  blocks <- df |> group_by(REP, IBLOCK) |> group_split()
  
  k_val <- max(sapply(blocks, nrow))
  pl_matrix <- matrix(NA, nrow = length(blocks), ncol = k_val)
  
  for (i in seq_along(blocks)) {
    block <- blocks[[i]]
    # 根據 AI 給的 rank 從最好到最差排序
    ordered_items <- block$TREATMENT[order(block$rank)]
    pl_matrix[i, 1:length(ordered_items)] <- as.character(ordered_items)
  }
  
  message("🧩 擬合 Plackett-Luce 模型中...")
  R_obj <- PlackettLuce::as.rankings(pl_matrix, input = "orderings")
  fit_pl <- try(PlackettLuce::PlackettLuce(R_obj), silent = TRUE)
  
  if (!inherits(fit_pl, "try-error")) {
    item_worth <- PlackettLuce::itempar(fit_pl, log = FALSE)
    final_ranking_df <- data.frame(
      ITEM_ID = names(item_worth),
      PL_Worth = as.numeric(item_worth)
    ) |> 
      arrange(desc(PL_Worth)) |> 
      mutate(Global_Rank = row_number())
    
    message("\n🏆 AI 全局排名 (前 5 名):")
    print(head(final_ranking_df, 5))
    
    # --- 4. 執行 Rank-Biased Overlap (RBO) 評估 ---
    # 假設我們有一個 Ground Truth (真實排名) 的列表
    # 這裡以 ID 的數字順序作為模擬的 Ground Truth (請依實際比較基準進行替換)
    ground_truth_list <- as.character(1:nrow(final_ranking_df)) 
    ai_predicted_list <- as.character(final_ranking_df$ITEM_ID)
    
    message("\n📐 計算 Rank-Biased Overlap (RBO)...")
    # p = 0.9 表示 top-heavy 的權重參數 (越接近 1 越看重全局，越接近 0 越只看榜首)
    rbo_res <- try(rbo(ai_predicted_list, ground_truth_list, p = 0.9), silent = TRUE)
    
    if(!inherits(rbo_res, "try-error")) {
      message("✅ RBO (p=0.9) 相似度得分: ", round(rbo_res$ext, 4))
    } else {
      message("⚠️ RBO 計算失敗，請確認 rbo() 函數的參數定義。")
    }
    
  } else {
    message("⚠️ Plackett-Luce 模型擬合失敗 (可能是資料稀疏導致網路未完全連結)")
  }
} else {
  message("⚠️ 找不到評分資料，請先取消註解並執行 Gemini API 腳本！")
}
