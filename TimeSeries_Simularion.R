set.seed(123)  # 再現性のためのシード

# 長さ100のホワイトノイズ
wn_data <- rnorm(100, mean = 0, sd = 1)

# 時系列データとして扱う
ts_data <- ts(wn_data)

# 可視化
plot(ts_data, main = "ホワイトノイズ", ylab = "Value", xlab = "Time")

library(forecast)
library(tseries)

# ① 原系列とヒストグラム
autoplot(ts_data) + ggtitle("原系列のプロット")
hist(ts_data, breaks = 20, main = "ヒストグラム", xlab = "値")

# ② 平均・分散・外れ値確認
mean(ts_data)
sd(ts_data)
boxplot(ts_data, main = "外れ値の確認")

# ③ 差分・対数・前期比など（対数系は正値が必要なので注意）
diff_data <- diff(ts_data)
log_data <- log(ts_data - min(ts_data) + 1)  # シフトして正値に
log_diff_data <- diff(log_data)

# ④ ヒストグラムで正規性の確認
hist(diff_data, breaks = 20, main = "差分のヒストグラム")
hist(log_diff_data, breaks = 20, main = "対数差分のヒストグラム")

# ⑤ 移動平均
ma3 <- stats::filter(ts_data, rep(1/3, 3), sides = 2)
autoplot(cbind(原系列 = ts_data, 移動平均3 = ma3)) +
  ggtitle("原系列と3期移動平均")
hist(ma3, breaks = 20, main = "3期移動平均のヒストグラム")

# ⑥ Box-Cox変換（λを自動推定）
lambda <- BoxCox.lambda(ts_data)
ts_boxcox <- BoxCox(ts_data, lambda)
autoplot(ts_boxcox) + ggtitle("Box-Cox変換後")
hist(ts_boxcox, breaks = 20, main = "Box-Cox変換後ヒストグラム")

# ① 自己相関（ACF）→ MA過程を仮定
acf(ts_data, main = "自己相関")

# ② 偏自己相関（PACF）→ AR過程を仮定
pacf(ts_data, main = "偏自己相関")

# ③ ピリオドグラム（周期性の検討）
spec.pgram(ts_data, main = "ピリオドグラム", taper = 0, log = "no")

# ④ 平滑化されたピリオドグラム
spec.pgram(ts_data, spans = c(3,3), main = "平滑化ピリオドグラム")


# AR(1)モデル: X_t = 0.8 * X_{t-1} + ε_t
set.seed(123)  # 再現性のためのシード
ar_data <- arima.sim(n = 100, model = list(ar = 0.8), sd = 1)

# 結果の表示と可視化
ts_data <- ts(ar_data)
plot(ts_data, main = "AR(1)+ホワイトノイズ", ylab = "Value", xlab = "Time")


# MA(1)モデル（θ = 0.6）
ma1_data <- arima.sim(n = 100, model = list(ma = 0.6), sd = 1)

# 時系列として可視化
ts_data <- ts(ma1_data)
plot(ts_data, main = "MA(1)+ホワイトノイズ", ylab = "Value", xlab = "Time")


# ARMA(1,1): φ = 0.7, θ = 0.5, σ = 1
arma_data <- arima.sim(n = 100, model = list(ar = 0.7, ma = 0.5), sd = 1)

ts_arma <- ts(arma_data)
plot(ts_arma, main = "ARMA(1,1)", ylab = "Value", xlab = "Time")


# 実データ
library(dplyr)
library(stringr)
library(lubridate)
library(tidyverse)
df <- read.csv("jissu.csv",encoding = "utf-8")
df
unique(df$西暦)

df_long <- df %>%
  rename(年 = 西暦) %>%
  mutate(年 = str_remove(年, "年")) %>%         # 「年」を削除
  pivot_longer(
    cols = -年,
    names_to = "月",
    values_to = "値"
  )

# ==== 2. 月・年・値を整形し、2024年以前のみにフィルタ ====
df_long_cleaned <- df_long %>%
  mutate(
    月 = str_remove_all(月, "[^0-9]"),   # "X10月" などを "10" に
    年 = as.integer(年),
    月 = as.integer(月),
    値 = as.numeric(値)
  ) %>%
  filter(!is.na(月) & !is.na(年) & !is.na(値) & 年 <= 2024) %>%
  mutate(日付 = as.Date(sprintf("%04d-%02d-01", 年, 月)))

# ==== 3. 折れ線グラフ（X軸：年月、Y軸：値） ====
ggplot(df_long_cleaned, aes(x = 日付, y = 値)) +
  geom_line() +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "1 year") +
  labs(
    title = "月別推移（2024年まで）",
    x = "年月",
    y = "値"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ① 原系列とヒストグラム
autoplot(df_long_cleaned$値) + ggtitle("原系列のプロット")
hist(df_long_cleaned$値, breaks = 20, main = "ヒストグラム", xlab = "値")

# ② 平均・分散・外れ値確認
mean(df_long_cleaned$値)
sd(df_long_cleaned$値)
boxplot(df_long_cleaned$値, main = "外れ値の確認")

# ③ 差分・対数・前期比など（対数系は正値が必要なので注意）
diff_data <- diff(df_long_cleaned$値)
log_data <- log(df_long_cleaned$値 - min(df_long_cleaned$値) + 1)  # シフトして正値に
log_diff_data <- diff(log_data)

# ④ ヒストグラムで正規性の確認
hist(diff_data, breaks = 20, main = "差分のヒストグラム")
hist(log_diff_data, breaks = 20, main = "対数差分のヒストグラム")

# ⑤ 移動平均
ma3 <- stats::filter(df_long_cleaned$値, rep(1/3, 3), sides = 2)
autoplot(cbind(原系列 = df_long_cleaned$値, 移動平均3 = ma3)) +
  ggtitle("原系列と3期移動平均")
hist(ma3, breaks = 20, main = "3期移動平均のヒストグラム")

# ⑥ Box-Cox変換（λを自動推定）
lambda <- BoxCox.lambda(df_long_cleaned$値)
ts_boxcox <- BoxCox(df_long_cleaned$値, lambda)
autoplot(ts_boxcox) + ggtitle("Box-Cox変換後")
hist(ts_boxcox, breaks = 20, main = "Box-Cox変換後ヒストグラム")

# ① 自己相関（ACF）→ MA過程を仮定
acf(df_long_cleaned$値, main = "自己相関")

# ② 偏自己相関（PACF）→ AR過程を仮定
pacf(df_long_cleaned$値, main = "偏自己相関")

# ③ ピリオドグラム（周期性の検討）
spec.pgram(df_long_cleaned$値, main = "ピリオドグラム", taper = 0, log = "no")

# ④ 平滑化されたピリオドグラム
spec.pgram(df_long_cleaned$値, spans = c(3,3), main = "平滑化ピリオドグラム")
