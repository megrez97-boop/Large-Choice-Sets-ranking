# Gumbel Distribution Comparison Script

# 手動定義 Gumbel 分佈生成函數
# Formula: x = location - scale * log(-log(u)) where u ~ Uniform(0,1)
rgumbel <- function(n, loc = 0, scale = 1) {
  u <- runif(n)
  return(loc - scale * log(-log(u)))
}

set.seed(42)
n <- 10000

# 準備存檔
pdf("C:/Users/User/Documents/R/Thesis/noise/gumbel/gumbel_comparison.pdf", width = 10, height = 8)
par(mfrow = c(2, 3)) # 2列3欄的佈局

# 1. 調整 Scale (驗證 temp 的影響)
g01 <- rgumbel(n, 0, 1)
hist(g01, breaks = 50, main = "Gumbel(0, 1)\nStandard", col = "skyblue", xlim = c(-5, 50))
abline(v = 0, col = "red", lwd = 2, lty = 2) # 標記 Location

g05 <- rgumbel(n, 0, 5)
hist(g05, breaks = 50, main = "Gumbel(0, 5)\nScale/Temp up", col = "lightgreen", xlim = c(-5, 50))
abline(v = 0, col = "red", lwd = 2, lty = 2)

g010 <- rgumbel(n, 0, 10)
hist(g010, breaks = 50, main = "Gumbel(0, 10)\nScale/Temp higher", col = "orange", xlim = c(-5, 50))
abline(v = 0, col = "red", lwd = 2, lty = 2)

# 2. 調整 Location (驗證位置移動)
g51 <- rgumbel(n, 5, 1)
hist(g51, breaks = 50, main = "Gumbel(5, 1)\nLocation shifted to 5", col = "pink", xlim = c(-5, 50))
abline(v = 5, col = "red", lwd = 2, lty = 2)

g101 <- rgumbel(n, 10, 1)
hist(g101, breaks = 50, main = "Gumbel(10, 1)\nLocation shifted to 10", col = "purple", xlim = c(-5, 50))
abline(v = 10, col = "red", lwd = 2, lty = 2)

dev.off()

cat("PDF plots generated at C:/Users/User/Documents/R/Thesis/noise/gumbel/gumbel_comparison.pdf\n")
