# 載入所需的額外套件 (Load required additional packages)
library(Matrix) [cite: 501]
library(dplyr) [cite: 502]
library(ggplot2) [cite: 503]

# 設置隨機種子以確保結果可重現 (Set random seed for reproducibility)
set.seed(1989) [cite: 504]

# 設定項目/玩家的數量 (Set the number of items/players)
n_items <- 1000 [cite: 505]

# ## 步驟 A: 生成稀疏的、對稱的二項式總數矩陣 Nmatrix (Step A: Generate a sparse, symmetric matrix of binomial totals (Nmatrix))
# Nvalues 隨機生成比較次數 (Comparison counts are randomly generated)
Nvalues <- rpois(n = n_items * (n_items - 1) / 2, lambda = 1) [cite: 507]
# 找出 Nvalues 中大於 0 的索引 (Find indices in Nvalues that are greater than 0)
notzero <- Nvalues > 0 [cite: 508]
# 初始化一個 n_items x n_items 的矩陣 (Initialize an n_items x n_items matrix)
Nmatrix <- Matrix(nrow = n_items, ncol = n_items) [cite: 509]
# 找出下三角矩陣 (lower.tri) 中非零元素的索引 (Find the indices of the non-zero elements in the lower triangle)
ij <- which(lower.tri(Nmatrix), arr.ind = TRUE)[notzero, ] [cite: 510]
# 使用找到的索引和值創建一個稀疏矩陣 (Create a sparse matrix using the found indices and values)
# symmetric = TRUE 表示 Nmatrix 是對稱的，i.e., N_ij = N_ji (symmetric = TRUE indicates Nmatrix is symmetric, i.e., N_ij = N_ji)
Nmatrix <- sparseMatrix(
  i = ij[, 1],
  j = ij[, 2],
  x = Nvalues[notzero],
  symmetric = TRUE,
  dims = c(n_items, n_items)
) [cite: 511, 512, 513, 514, 515]

# ## 步驟 B: 生成標準化後的真實 '玩家能力' 參數 (Step B: Generate the normalized true 'player ability' parameters)
# 使用常態分佈生成對數尺度的能力 (Generate log-scale abilities using a normal distribution)
pi_vec <- exp(rnorm(n_items) / 4) [cite: 517]
# 將 pi_vec 標準化使其平均值為 1 (Normalize pi_vec so its mean is 1)
pi_vec <- pi_vec / mean(pi_vec) [cite: 518]

# ## 步驟 C: 從 Bradley-Terry 模型模擬比賽結果 (Step C: Simulate contest outcome counts from the Bradley-Terry model)
# 使用 pi_vec (真實能力) 和 Nmatrix (比較次數) 模擬單次結果 (Simulate a single outcome using pi_vec (true abilities) and Nmatrix (comparison counts))
big_matrix <- simulate_BT(pi_vec, Nmatrix, nsim = 1, seed = 1)[[1]] [cite: 519]
# 將結果矩陣轉換為 btdata 對象 (Convert the resulting matrix into a btdata object)
big_btdata <- btdata(big_matrix) [cite: 519]

# ## 步驟 D: 擬合 Bradley-Terry 模型到模擬數據 (Step D: Fit the Bradley-Terry model to the simulated data)
# 使用 MLE ($a=1$) 擬合模型 (Fit the model using MLE ($a=1$))
the_model <- btfit(big_btdata, a = 1) [cite: 520]
# 提取擬合的能力參數 (pi) (Extract the fitted ability parameters (pi))
pi_fitted <- the_model$pi$full_dataset [cite: 521]

# ## 步驟 E: 繪製擬合與真實能力的圖表 (Step E: Plot fitted vs true abilities)
# 準備用於繪圖的資料框 (Prepare the data frame for plotting)
plot_df <- tibble(
  x = log(pi_vec[as.numeric(names(pi_fitted))]), # 真實強度 (true strength, log-scale)
  y = log(pi_fitted)                              # 最大似然估計 (maximum likelihood estimate, log-scale)
) [cite: 523, 524]

# 使用 ggplot2 繪圖 (Plotting using ggplot2)
ggplot(plot_df, aes(x, y)) + [cite: 525]
geom_point(alpha = 0.5) + # 添加半透明點 (Add semi-transparent points) [cite: 526]
  geom_abline() +           # 添加對角線 (y=x) 作為參考 (Add the diagonal line (y=x) as reference) [cite: 527]
  xlab("true strength") +   # 設定 x 軸標籤 (Set x-axis label) [cite: 528]
  ylab("maximum likelihood estimate") + # 設定 y 軸標籤 (Set y-axis label) [cite: 529]
  ggtitle("1000-player simulation from a Bradley-Terry model") + # 設定標題 (Set the title) [cite: 530]
  theme(plot.title = element_text(hjust = 0.5)) # 標題居中 (Center the title) [cite: 531]