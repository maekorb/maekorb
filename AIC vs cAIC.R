library(lme4)
library(cAIC4)
library(MASS)

# シミュレーションパラメータ
set.seed(123)
m <- 10         # グループ数
n <- 10         # 観測回数 / グループ
beta0 <- 0
beta1 <- 1
sd_b <- 0.5     # ランダム切片の標準偏差
sd_eps <- 1     # 誤差の標準偏差

# データ生成
sim_data <- data.frame()
for (i in 1:m) {
  b_i <- rnorm(1, mean = 0, sd = sd_b)
  x <- runif(n, 0, 1)
  eps <- rnorm(n, mean = 0, sd = sd_eps)
  y <- beta0 + beta1 * x + b_i + eps
  sim_data <- rbind(sim_data, data.frame(
    y = y,
    x = x,
    Subject = factor(i)
  ))
}

# モデル①：ランダム切片のみ（真のモデル）
mod_true <- lmer(y ~ x + (1 | Subject), data = sim_data, REML = FALSE)

# モデル②：ランダム切片＋傾き（過剰なモデル）
mod_overfit <- lmer(y ~ x + (x | Subject), data = sim_data, REML = FALSE)

# AICとcAICの比較
mAIC_true <- AIC(mod_true)
mAIC_over <- AIC(mod_overfit)

cAIC_true <- cAIC(mod_true)$caic
cAIC_over <- cAIC(mod_overfit)$caic

# 結果表示
cat("【mAIC】\n")
cat("ランダム切片のみ      :", round(mAIC_true, 2), "\n")
cat("ランダム切片＋傾き    :", round(mAIC_over, 2), "\n\n")

cat("【cAIC】\n")
cat("ランダム切片のみ      :", round(cAIC_true, 2), "\n")
cat("ランダム切片＋傾き    :", round(cAIC_over, 2), "\n")