library(FielDHub)
library(BradleyTerry2)

# 定義 Gumbel 隨機產生函數 (若未安裝 evd 套件可用此自訂版本)
rgumbel <- function(n, location = 0, scale = 1) {
  return(location - scale * log(-log(runif(n))))
}

# 模擬核心函數
run_simulation <- function(temp = 1, t = 20, k = 2, r = 2, noise_mode = "plot") {
  # noise_mode: 
  #   "plot" (Approach b): 每次出現都加新雜訊 (Momentary distraction)
  #   "item" (Approach a): 每個項目固定雜訊 (Fixed individual perception)
  
  # 1. 產生 Alpha Lattice 設計
  design <- alpha_lattice(t = t, k = k, r = r, seed = 123)
  fb <- design$fieldBook
  
  # 2. 設定真實價值 (True Value: 項目 1 最強, 20 最弱)
  # 我們假設 V_i = 21 - i
  true_values <- 21 - as.numeric(fb$ENTRY)
  
  # 3. 加入雜訊模擬受訪者決策
  if (noise_mode == "plot") {
    # Approach (b): 每一個 plot (nrow(fb)) 都有獨立雜訊
    fb$Utility <- true_values + temp * rgumbel(nrow(fb))
  } else if (noise_mode == "item") {
    # Approach (a): 每個項目 (t 個) 產生一次雜訊，然後對應回 fb
    item_noise <- data.frame(
      TREATMENT = unique(as.character(fb$TREATMENT)),
      Noise = rgumbel(t)
    )
    fb <- merge(fb, item_noise, by = "TREATMENT")
    # 重新計算 Utility: V + Temp * Fixed_Item_Noise
    # 注意：這裡的 true_values 需要重新從合併後的 fb 計算以確保順序正確
    fb$Utility <- (21 - as.numeric(fb$ENTRY)) + temp * fb$Noise
  }
  
  # 4. 根據 Utility 選出每一組的 Winner (Best) 與 Loser (Worst)
  results_list <- list()
  unique_blocks <- unique(fb[, c("REP", "IBLOCK")])
  
  for(i in 1:nrow(unique_blocks)) {
    block_data <- fb[fb$REP == unique_blocks[i, "REP"] & fb$IBLOCK == unique_blocks[i, "IBLOCK"], ]
    winner <- block_data$TREATMENT[which.max(block_data$Utility)]
    loser <- block_data$TREATMENT[which.min(block_data$Utility)]
    results_list[[i]] <- data.frame(Winner = winner, Loser = loser)
  }
  
  final_df <- do.call(rbind, results_list)
  all_players <- unique(as.character(fb$TREATMENT))
  final_df$Winner <- factor(final_df$Winner, levels = all_players)
  final_df$Loser <- factor(final_df$Loser, levels = all_players)
  
  # 5. 擬合 BT 模型
  bt_model <- BTm(1, Winner, Loser, data = final_df)
  abilities <- BTabilities(bt_model)[, "ability"]
  
  # 6. 計算評估指標：Spearman Rho (估計排名 vs 真實排名)
  # 真實排名是 1, 2, ..., 20
  estimated_rank <- rank(-abilities) # Ability 越大排名越前面
  true_rank <- 1:t
  names(true_rank) <- all_players
  
  # 對齊順序
  rho <- cor(estimated_rank, true_rank[names(estimated_rank)], method = "spearman")
  return(rho)
}

# --- 執行實驗：測試不同 Temperature 與 Noise Modes ---
set.seed(42)
temps <- seq(0, 10, by = 1)

results_plot <- data.frame(
  Temperature = temps, 
  Rho = sapply(temps, function(x) run_simulation(temp = x, noise_mode = "plot")),
  Mode = "Plot-level (Approach b)"
)

results_item <- data.frame(
  Temperature = temps, 
  Rho = sapply(temps, function(x) run_simulation(temp = x, noise_mode = "item")),
  Mode = "Item-level (Approach a)"
)

final_results <- rbind(results_plot, results_item)
print(final_results)

# 繪製趨勢圖
library(ggplot2)
ggplot(final_results, aes(x = Temperature, y = Rho, color = Mode, linetype = Mode)) +
  geom_line(size = 1) + geom_point(size = 2) +
  labs(title = "Comparison of Noise Modes on Ranking Accuracy",
       subtitle = "Alpha Lattice Design (t=20, k=2, r=2)",
       x = "Temperature (Noise Scale)",
       y = "Spearman Rho (Accuracy)") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1")

