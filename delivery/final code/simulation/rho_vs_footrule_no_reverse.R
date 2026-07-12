# =================================================================================
# Patched Version: Multi-k Automated Alpha Lattice Simulation
# Focus: Normal Spearman's Rho and Footrule (No Reversed Ranking / No SLM)
# Working Directory & Output set to: delivery/final code/simulation
# =================================================================================

# Set Working Directory
setwd("C:/Users/user/Documents/R/Thesis/Large-Choice-Sets-ranking/delivery/final code/simulation")

required_packages <- c("FielDHub", "BradleyTerry2", "ggplot2", "doSNOW", "foreach")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

library(FielDHub)
library(BradleyTerry2)
library(ggplot2)
library(doSNOW)
library(foreach)

run_multi_k_dual_metrics <- function(t_val = 240, k_values = c(2, 3, 4, 5), r_limit = 20, seed_val = 1006) {
  
  # 設定 r=2..20; k=2..5
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
  
  # 平行計算 Footrule 與 Rho (無反向排序，無 SLM)
  final_data <- foreach(i = 1:total_tasks, 
                        .combine = rbind, 
                        .packages = c("FielDHub", "BradleyTerry2"),
                        .options.snow = opts) %dopar% {
    
    current_k <- task_grid$k[i]
    current_r <- task_grid$r[i]
    
    # 【重現性關鍵修復】設定與當前任務參數綁定的穩定隨機數種子
    worker_seed <- (seed_val * 10000) + (current_k * 100) + current_r
    set.seed(worker_seed)
    
    design <- try(alpha_lattice(t = t_val, k = current_k, r = current_r, seed = worker_seed), silent = TRUE)
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
          
          # 計算正常模型指標 (Normal)
          rho_normal <- cor(res_df$PredRank, res_df$TrueRank, method = "spearman")
          footrule_normal <- sum(abs(res_df$PredRank - res_df$TrueRank))
          
          return(data.frame(k = as.factor(current_k), 
                            r = current_r, 
                            rho = rho_normal, 
                            footrule = footrule_normal))
        }
      }
    }
    return(NULL) 
  }
  
  close(pb)
  stopCluster(cl)
  
  # =======================================================================
  # 計算 Random 的理論基準線 (Baselines)
  # =======================================================================
  cat("\n\n📊 ================= 基準線報告 (Baselines) =================\n")
  cat(sprintf("當 t = %d 時：\n", t_val))
  
  # 隨機排序 (Random Ranking) 基準
  random_footrule_expected <- (t_val^2 - 1) / 3
  cat(sprintf("➤ [隨機瞎猜 Random] Rho 期望值: 0.00\n"))
  cat(sprintf("➤ [隨機瞎猜 Random] Footrule 期望值: %.0f 分\n", random_footrule_expected))
  cat("=============================================================\n\n")

  # =======================================================================
  # 繪製圖表並加入基準線
  # =======================================================================
  
  # 圖表 A: Spearman's Rho (滿分是 1)
  p_rho <- ggplot(final_data, aes(x = r, y = rho, color = k)) +
    geom_line(linewidth = 1.2) + geom_point(size = 2.5, alpha = 0.8) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 0.8) +
    annotate("text", x = r_limit - 4, y = 0.05, label = "Random Baseline (0.00)", color = "red", size = 4) +
    labs(title = "Spearman's Rho (Correlation) Convergence",
         subtitle = "Closer to 1.0 is better. Dashed line represents random baseline.",
         x = "Number of Replications (r)", y = "Rho", color = "Block Size (k)") + 
    coord_cartesian(ylim = c(0, 1.00)) + theme_minimal() + theme(legend.position = "bottom")
  
  # 圖表 B: Spearman's Footrule (0 分最好)
  p_footrule <- ggplot(final_data, aes(x = r, y = footrule, color = k)) +
    geom_line(linewidth = 1.2) + geom_point(size = 2.5, alpha = 0.8) +
    geom_hline(yintercept = random_footrule_expected, linetype = "dashed", color = "red", linewidth = 0.8) +
    annotate("text", x = r_limit - 4, y = random_footrule_expected - (random_footrule_expected * 0.05), 
             label = sprintf("Random Expected (%.0f)", random_footrule_expected), color = "red", size = 4) +
    labs(title = "Spearman's Footrule (Absolute Error) Convergence",
         subtitle = "Closer to 0 is better. Dashed line represents random expected baseline.",
         x = "Number of Replications (r)", y = "Total Error Score", color = "Block Size (k)") + 
    theme_minimal() + theme(legend.position = "bottom")
  
  # 自動存圖
  ggsave("spearman_rho_k_2_5.png", plot = p_rho, width = 8, height = 6)
  ggsave("spearman_footrule_k_2_5.png", plot = p_footrule, width = 8, height = 6)
  cat("💾 圖表已自動存檔至工作目錄為: spearman_rho_k_2_5.png 與 spearman_footrule_k_2_5.png\n")
  
  return(list(data = final_data, plot_rho = p_rho, plot_footrule = p_footrule))
}

# 執行程式碼
result <- run_multi_k_dual_metrics(t_val = 240, k_values = c(2, 3, 4, 5), r_limit = 20, seed_val = 1006)

# 印出兩張圖表
print(result$plot_rho)
print(result$plot_footrule)
