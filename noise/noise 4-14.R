# =================================================================================
# 終極雙指標版：Multi-k Automated Alpha Lattice Simulation
# 任務解鎖：Footrule vs Rho 雙圖表 + 基準線計算 (Baselines)
# =================================================================================

if (!require("evd")) install.packages("evd")
library(evd)
library(FielDHub)
library(BradleyTerry2)
library(ggplot2)
library(doSNOW)
library(foreach)

run_multi_k_dual_metrics <- function(t_val = 240, k_values = c(2, 3, 4, 5), r_limit = 20, seed_val = 1006, temp = 0.5) {
  
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
                        .packages = c("FielDHub", "BradleyTerry2", "evd"),
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
      
      # [核心修改] 加入 Temp * Gumbel(0,1) 雜訊
      # 模擬感官效用 (Utility): U = V + Temp * Noise
      u1 <- val1 + temp * rgumbel(length(val1), 0, 1)
      u2 <- val2 + temp * rgumbel(length(val2), 0, 1)
      
      # 根據帶雜訊的效用決定勝負
      winners <- ifelse(u1 > u2, pairs_mat[1, ], pairs_mat[2, ])
      losers  <- ifelse(u1 > u2, pairs_mat[2, ], pairs_mat[1, ])
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
          # 1. 計算排名指標
          # ==========================================
          rho_val <- cor(res_df$PredRank, res_df$TrueRank, method = "spearman")
          footrule_val <- sum(abs(res_df$PredRank - res_df$TrueRank))
          
          return(data.frame(k = as.factor(current_k), 
                            r = current_r, 
                            rho = rho_val, 
                            footrule = footrule_val))
        }
      }
    }
    return(NULL) 
  }
  
  close(pb)
  stopCluster(cl)
  
  return(final_data)
}

# 執行程式碼
result_data <- run_multi_k_dual_metrics(t_val = 240, k_values = c(2, 3, 4, 5), r_limit = 10, seed_val = 1006, temp = 0)








