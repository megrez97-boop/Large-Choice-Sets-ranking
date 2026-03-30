# =================================================================================
# 終極極速版：Multi-k Automated Alpha Lattice Simulation (全核心平行 + 進度條)
# 專為 Ryzen 桌機 + 大容量 RAM 打造
# =================================================================================

# 1. 自動檢查並安裝缺少的套件 (包含 doSNOW 進度條套件)
required_packages <- c("FielDHub", "BradleyTerry2", "ggplot2", "doSNOW", "foreach")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# 2. 載入套件
library(FielDHub)
library(BradleyTerry2)
library(ggplot2)
library(doSNOW)
library(foreach)

run_multi_k_desktop_turbo <- function(t_val = 240, k_values = c(3, 4, 5), r_limit = 50, seed_val = 1006) {
  
  # [Step 1] 建立任務網格 (使用 seq 跳步法，5, 10, 15...50)
  r_steps <- 2:r_limit
  task_grid <- expand.grid(k = k_values, r = r_steps)
  total_tasks <- nrow(task_grid)
  
  # [Step 2] 啟動平行運算環境
  # 總共 12 執行緒，扣掉 4 顆保留給系統，使用 8 顆核心全力運算
  num_cores <- max(1, parallel::detectCores() - 4) 
  cl <- makeCluster(num_cores)
  registerDoSNOW(cl)
  
  cat(sprintf("\n🚀 引擎啟動！已徵召 %d 顆核心同時運算，系統保留 %d 顆以維持順暢。\n", num_cores, parallel::detectCores() - num_cores))
  cat(sprintf("總共 %d 個排隊任務，準備起飛...\n\n", total_tasks))
  
  # [Step 3] 設定進度條 (視覺化儀表板)
  pb <- txtProgressBar(max = total_tasks, style = 3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress = progress)
  
  # [Step 4] 使用 foreach 進行平行多工運算
  final_data <- foreach(i = 1:total_tasks, 
                        .combine = rbind, 
                        .packages = c("FielDHub", "BradleyTerry2"),
                        .options.snow = opts) %dopar% {
    
    current_k <- task_grid$k[i]
    current_r <- task_grid$r[i]
    
    # --- 產生實驗設計 ---
    design <- try(alpha_lattice(t = t_val, k = current_k, r = current_r, seed = seed_val), silent = TRUE)
    if (inherits(design, "try-error")) return(NULL) 
    
    fb <- design$fieldBook
    fb$TrueValue <- (t_val + 1) - as.numeric(fb$ENTRY)
    
    # --- 光速配對 (向量化) ---
    block_tags <- paste(fb$REP, fb$IBLOCK, sep = "_")
    matches <- do.call(rbind, lapply(split(fb, block_tags), function(block) {
      pairs_mat <- combn(block$TREATMENT, 2)
      val1 <- block$TrueValue[match(pairs_mat[1, ], block$TREATMENT)]
      val2 <- block$TrueValue[match(pairs_mat[2, ], block$TREATMENT)]
      winners <- ifelse(val1 > val2, pairs_mat[1, ], pairs_mat[2, ])
      losers  <- ifelse(val1 > val2, pairs_mat[2, ], pairs_mat[1, ])
      data.frame(Winner = winners, Loser = losers)
    }))
    
    # --- 執行 BT 模型 (包含提早放棄機制 maxit = 5) ---
    all_players <- unique(c(as.character(matches$Winner), as.character(matches$Loser)))
    matches$Winner <- factor(matches$Winner, levels = all_players)
    matches$Loser  <- factor(matches$Loser, levels = all_players)
    
    fit <- suppressWarnings(try(BTm(1, Winner, Loser, data = matches, control = glm.control(maxit = 5)), silent = TRUE))
    
    # --- 計算 Spearman's Rho 相關係數 ---
    if (!inherits(fit, "try-error")) {
      abilities <- suppressWarnings(try(BTabilities(fit), silent = TRUE))
      if (!inherits(abilities, "try-error")) {
        player_ids <- as.numeric(gsub("[^0-9]", "", rownames(abilities)))
        player_abs <- abilities[, "ability"]
        res_df <- data.frame(ID = player_ids, Ability = player_abs)
        res_df <- res_df[!is.na(res_df$Ability), ]
        
        if (nrow(res_df) > 2) {
          res_df$TrueRank <- rank(res_df$ID) 
          res_df$PredRank <- rank(-res_df$Ability)
          rho_score <- cor(res_df$PredRank, res_df$TrueRank, method = "spearman")
          
          return(data.frame(k = as.factor(current_k), r = current_r, error = rho_score))
        }
      }
    }
    return(NULL) 
  }
  
  # [Step 5] 關閉進度條與核心通道
  close(pb)
  stopCluster(cl)
  
  if (is.null(final_data) || nrow(final_data) == 0) {
    stop("\n❌ Error: 所有模型皆運算失敗。請檢查參數設定。")
  }
  
  cat("\n✅ 平行運算完成！繪製圖表中...\n")
  
  # [Step 6] 繪製圖表 Visualization
  p <- ggplot(final_data, aes(x = r, y = error, color = k)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 2.5, alpha = 0.8) +
    labs(title = paste("Simulation Analysis (t =", t_val, ")"),
         subtitle = "Comparing Convergence Across Different Block Sizes (k)",
         x = "Number of Replications (r)", 
         y = "Spearman's Rho (Correlation)", 
         color = "Block Size (k)") + 

    coord_cartesian(ylim = c(0.95, 1.00))
    theme_minimal() +
    theme(legend.position = "bottom",
          plot.title = element_text(face = "bold", size = 14))
  
  return(list(data = final_data, plot = p))
}

# =================================================================================
# 執行模擬測試
# =================================================================================

# 直接挑戰 t=240，k=3, 4, 5 三條線齊發！
result <- run_multi_k_desktop_turbo(t_val = 240, k_values = 3, r_limit = 20, seed_val = 1006)

# 印出精美圖表
print(result$plot)