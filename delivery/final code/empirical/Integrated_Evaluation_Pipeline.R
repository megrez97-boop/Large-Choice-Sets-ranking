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
      message("✅ RBO (p=0.9) 相似度得分: ", round(rbo_res["ext"], 4))
    } else {
      message("⚠️ RBO 計算失敗，請確認 rbo() 函數的參數定義。")
    }
    
    # --- 5. 繪製 Replication 收斂趨勢圖 ---
    if (requireNamespace("ggplot2", quietly = TRUE) && requireNamespace("wdm", quietly = TRUE)) {
      suppressPackageStartupMessages(library(ggplot2))
      suppressPackageStartupMessages(library(wdm))
      
      max_rep <- max(df$REP, na.rm = TRUE)
      if (max_rep > 1) {
        message("\n📈 正在計算並繪製收斂趨勢圖 (Replication Convergence)...")
        
        global_rankings_list <- list()
        for (r in 1:max_rep) {
          sub_df <- df |> filter(REP <= r)
          sub_blocks <- sub_df |> group_by(REP, IBLOCK) |> group_split()
          
          sub_k_val <- max(sapply(sub_blocks, nrow))
          sub_matrix <- matrix(NA, nrow = length(sub_blocks), ncol = sub_k_val)
          
          for (j in seq_along(sub_blocks)) {
            sub_ordered <- sub_blocks[[j]]$TREATMENT[order(sub_blocks[[j]]$rank)]
            sub_matrix[j, 1:length(sub_ordered)] <- as.character(sub_ordered)
          }
          
          sub_R_obj <- PlackettLuce::as.rankings(sub_matrix, input = "orderings")
          sub_fit <- try(PlackettLuce::PlackettLuce(sub_R_obj), silent = TRUE)
          
          if (!inherits(sub_fit, "try-error")) {
            sub_worth <- PlackettLuce::itempar(sub_fit, log = FALSE, vcov = FALSE)
            global_rankings_list[[r]] <- names(sub_worth)[order(sub_worth, decreasing = TRUE)]
          } else {
            global_rankings_list[[r]] <- NA
          }
        }
        
        results_conv <- data.frame(Replication = integer(), Metric = character(), Similarity = numeric())
        for (r in 2:max_rep) {
          rank_curr <- global_rankings_list[[r]]
          rank_prev <- global_rankings_list[[r-1]]
          
          if (all(!is.na(rank_curr)) && all(!is.na(rank_prev))) {
            rbo_val <- try(rbo(rank_curr, rank_prev, p = 0.9), silent = TRUE)
            rbo_score <- if(!inherits(rbo_val, "try-error")) as.numeric(rbo_val["ext"]) else NA
            
            items_universe <- rank_curr
            num_curr <- match(items_universe, rank_curr)
            num_prev <- match(items_universe, rank_prev)
            tau_score <- try(wdm(num_curr, num_prev, method = "kendall"), silent = TRUE)
            tau_score <- if(!inherits(tau_score, "try-error")) tau_score else NA
            
            results_conv <- rbind(results_conv, data.frame(Replication = r, Metric = "RBO (p=0.9)", Similarity = rbo_score))
            results_conv <- rbind(results_conv, data.frame(Replication = r, Metric = "Traditional Tau", Similarity = tau_score))
          }
        }
        
        if (nrow(results_conv) > 0) {
          results_conv <- na.omit(results_conv)
          # 找出 y 軸合理的下界 (向下取整到最近的 0.05)
          min_val <- min(results_conv$Similarity, na.rm = TRUE)
          y_lower <- max(0, floor(min_val * 20) / 20 - 0.05)
          
          p <- ggplot(results_conv, aes(x = Replication, y = Similarity, color = Metric, group = Metric)) +
            geom_line(linewidth = 1.2, alpha = 0.8) +
            geom_point(size = 3) +
            geom_text(data = subset(results_conv, Replication == max_rep),
                      aes(label = sprintf("%.4f", Similarity),
                          vjust = ifelse(Metric == "RBO (p=0.9)", -0.8, 1.8)),
                      hjust = 0.5, size = 4.5, fontface = "bold", show.legend = FALSE) +
            theme_minimal() +
            labs(
              title = "Convergence of Global Ranking Over Replications",
              subtitle = "Similarity between GlobalRanking(r) and GlobalRanking(r-1)",
              x = "Replication (r)",
              y = "Similarity / Correlation Coefficient",
              color = "Evaluation Metric"
            ) +
            scale_x_continuous(breaks = 2:max_rep) +
            scale_y_continuous(limits = c(y_lower, 1.02), breaks = seq(y_lower, 1, by = 0.05)) +
            theme(
              plot.title = element_text(face = "bold", size = 16),
              legend.position = "bottom",
              text = element_text(size = 12)
            )
          
          out_plot_name <- gsub("\\.csv$", "_Convergence.png", basename(latest_csv))
          out_plot <- file.path(results_dir, out_plot_name)
          
          suppressWarnings(ggsave(out_plot, plot = p, width = 8, height = 6, dpi = 300, bg = "white"))
          message("✅ 繪圖完成！圖表已儲存至: ", out_plot)
        } else {
          message("⚠️ 無法繪圖，無足夠的有效收斂資料點。")
        }
      } else {
        message("ℹ️ 資料只有 1 個 Replication，無法繪製收斂趨勢圖。")
      }
    } else {
      message("⚠️ 尚未安裝 ggplot2 或 wdm 套件，跳過繪製收斂趨勢圖。")
    }
    
  } else {
    message("⚠️ Plackett-Luce 模型擬合失敗 (可能是資料稀疏導致網路未完全連結)")
  }
} else {
  message("⚠️ 找不到評分資料，請先取消註解並執行 Gemini API 腳本！")
}
