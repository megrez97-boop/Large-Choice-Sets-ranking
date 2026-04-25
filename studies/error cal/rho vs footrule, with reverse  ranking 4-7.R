# =================================================================================
# 終極雙指標版：Multi-k Automated Alpha Lattice Simulation
# 任務解鎖：Footrule vs Rho 雙圖表 + 基準線計算 (Baselines)
# =================================================================================

library(FielDHub)
library(BradleyTerry2)
library(ggplot2)
library(doSNOW)
library(foreach)

run_multi_k_dual_metrics <- function(t_val = 240, k_values = c(2, 3, 4, 5), r_limit = 20, seed_val = 1006) {
  
  # [Task 1] 設定 r=2..20; k=2..5
  r_steps <- 2:r_limit
  task_grid <- expand.grid(k = k_values, r = r_steps)
  total_tasks <- nrow(task_grid)
  
  # 啟動平行運算環境
  num_cores <- max(1, parallel::detectCores() - 2) 
  cl <- makeCluster(num_cores)
  registerDoSNOW(cl)
  
  cat(sprintf("\n🚀 雙指標引擎啟動！已徵召 %d 顆核心。\n", num_cores))
  cat(sprintf("正在執行 %d 個任務 (k=2~5, r=2~20)...\n\n", total_tasks))
  
  pb <- txtProgressBar(max = total_tasks, style = 3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress = progress)
  
  # [Task 2] 同時計算 Footrule 與 Rho
  final_data <- foreach(i = 1:total_tasks, 
                        .combine = rbind, 
                        .packages = c("FielDHub", "BradleyTerry2"),
                        .options.snow = opts) %dopar% {
    
    current_k <- task_grid$k[i]
    current_r <- task_grid$r[i]
    
    design <- try(alpha_lattice(t = t_val, k = current_k, r = current_r, seed = seed_val), silent = TRUE)
    if (inherits(design, "try-error")) return(NULL) 
    
    fb <- design$fieldBook
    fb$TrueValue <- (t_val + 1) - as.numeric(fb$ENTRY)
    
    block_tags <- paste(fb$REP, fb$IBLOCK, sep = "_")
    matches <- do.call(rbind, lapply(split(fb, block_tags), function(block) {
      pairs_mat <- combn(block$TREATMENT, 2)
      val1 <- block$TrueValue[match(pairs_mat[1, ], block$TREATMENT)]
      val2 <- block$TrueValue[match(pairs_mat[2, ], block$TREATMENT)]
      winners <- ifelse(val1 > val2, pairs_mat[1, ], pairs_mat[2, ])
      losers  <- ifelse(val1 > val2, pairs_mat[2, ], pairs_mat[1, ])
      data.frame(Winner = winners, Loser = losers)
    }))
    
    all_players <- unique(c(as.character(matches$Winner), as.character(matches$Loser)))
    matches$Winner <- factor(matches$Winner, levels = all_players)
    matches$Loser  <- factor(matches$Loser, levels = all_players)
    
    fit <- suppressWarnings(try(BTm(1, Winner, Loser, data = matches, control = glm.control(maxit = 5)), silent = TRUE))
    
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
          
          # ==========================================
          # 1. 實打實計算：正常模型 (Normal)
          # ==========================================
          rho_normal <- cor(res_df$PredRank, res_df$TrueRank, method = "spearman")
          footrule_normal <- sum(abs(res_df$PredRank - res_df$TrueRank))
          
          # ==========================================
          # 2. 實打實計算：反向模型 (Reverse)
          # ==========================================
          # 把模型排出來的名次完全顛倒 (例如共 240 人，第 1 名變成 240 名)
          # 使用 nrow(res_df) 比直接用 240 更安全，以防 BT 模型中途剔除無效玩家
          reverse_pred_rank <- (nrow(res_df) + 1) - res_df$PredRank 
          
          # 拿顛倒後的名次，去跟真實名次算相關係數與誤差
          rho_reverse <- cor(reverse_pred_rank, res_df$TrueRank, method = "spearman")
          footrule_reverse <- sum(abs(reverse_pred_rank - res_df$TrueRank))
          
          # ==========================================
          # 3. 把正向與反向打包成兩列資料，一起丟出去！
          # ==========================================
          df_normal <- data.frame(k = as.factor(current_k), 
                                  r = current_r, 
                                  rho = rho_normal, 
                                  footrule = footrule_normal)
          
          df_reverse <- data.frame(k = paste(current_k, "(Reversed)"), 
                                   r = current_r, 
                                   rho = rho_reverse, 
                                   footrule = footrule_reverse)
          
          return(rbind(df_normal, df_reverse))
        }
      }
    }
    return(NULL) 
  }
  
  close(pb)
  stopCluster(cl)
  
  # =======================================================================
  # [Task 3] 計算 Random 與 Reverse 的理論基準線 (Baselines)
  # =======================================================================
  cat("\n\n📊 ================= 基準線報告 (Baselines) =================\n")
  cat(sprintf("當 t = %d 時：\n", t_val))
  
  # 1. 隨機排序 (Random Ranking) 基準
  random_footrule_expected <- (t_val^2 - 1) / 3
  cat(sprintf("➤ [隨機瞎猜 Random] Rho 期望值: 0.00\n"))
  cat(sprintf("➤ [隨機瞎猜 Random] Footrule 期望值: %.0f 分\n", random_footrule_expected))
  
  # 2. 完全反向排序 (Reverse Ranking) 基準
  max_footrule <- if(t_val %% 2 == 0) (t_val^2)/2 else (t_val^2 - 1)/2
  cat(sprintf("➤ [完全反向 Reverse] Rho 值: -1.00\n"))
  cat(sprintf("➤ [完全反向 Reverse] Footrule 最大值: %.0f 分\n", max_footrule))
  cat("=============================================================\n\n")

  # =======================================================================
  # 繪製兩張圖表
  # =======================================================================
  
  # 圖表 A: Spearman's Rho (滿分是 1)
  p_rho <- ggplot(final_data, aes(x = r, y = rho, color = k)) +
    geom_line(linewidth = 1.2) + geom_point(size = 2.5, alpha = 0.8) +
    labs(title = "Spearman's Rho (Correlation)",
         subtitle = "Closer to 1.0 is better",
         x = "Number of Replications (r)", y = "Rho", color = "Block Size (k)") + 
    coord_cartesian(ylim = c(0, 1.00)) + theme_minimal() + theme(legend.position = "bottom")
  
  # 圖表 B: Spearman's Footrule (0 分最好)
  p_footrule <- ggplot(final_data, aes(x = r, y = footrule, color = k)) +
    geom_line(linewidth = 1.2) + geom_point(size = 2.5, alpha = 0.8) +
    labs(title = "Spearman's Footrule (Absolute Error)",
         subtitle = "Closer to 0 is better",
         x = "Number of Replications (r)", y = "Total Error Score", color = "Block Size (k)") + 
    theme_minimal() + theme(legend.position = "bottom")
  
  return(list(data = final_data, plot_rho = p_rho, plot_footrule = p_footrule))
}

# 執行程式碼
result <- run_multi_k_dual_metrics(t_val = 240, k_values = c(2, 3, 4, 5), r_limit = 20, seed_val = 1006)

# 印出兩張圖表 (在 RStudio 的 Plots 視窗可以切換上一張/下一張看)
print(result$plot_rho)
print(result$plot_footrule)







