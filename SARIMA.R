library(dplyr)
library(stringr)
library(lubridate)
library(tidyverse)

# データの読み込み
df <- read.csv("data/beer.csv")
head(df)

str(df)
df$日付 <- as.Date(df$日付)
df <- cbind(df,tenki)

colMeans(df[ , -1])

library(xts)
train_df <- df[df$日付 <= as.Date("2017-06-30"), ]
test_df  <- df[df$日付 >  as.Date("2017-06-30"), ]

nodogoshi <- xts(df$のどごし, order.by = df$日付)
colnames(nodogoshi) <- "のどごし"
head(nodogoshi)

nodogoshi_ts <- ts(df$のどごし, start = c(2017, 1), frequency = 7)
nodogoshi_ts

plot(nodogoshi_ts, main = "のどごし", ylab = "売上(個)", xlab = "日")

library(forecast)
library(tseries)

# ① 原系列とヒストグラム
autoplot(nodogoshi_ts) + ggtitle("原系列のプロット")
hist(nodogoshi_ts, breaks = 20, main = "ヒストグラム", xlab = "値")

mean(nodogoshi_ts)
sd(nodogoshi_ts)
boxplot(nodogoshi_ts, main = "外れ値の確認")

adf.test(nodogoshi_ts)

acf(nodogoshi_ts, main = "自己相関")
pacf(nodogoshi_ts, main = "偏自己相関")
spec.pgram(nodogoshi_ts, main = "ピリオドグラム", taper = 0, log = "no")

model <- auto.arima(nodogoshi_ts, seasonal = TRUE)
summary(model)

checkresiduals(model)

install.packages("holidayJPN")
library(timeDate)
holidays_2017 <- as.Date(holiday.JP(2017))

holidays_2017 <- as.Date(c(
  "2017-01-01", "2017-01-02",  # 元日、振替休日
  "2017-01-09",  # 成人の日
  "2017-02-11",  # 建国記念の日
  "2017-03-20",  # 春分の日
  "2017-04-29",  # 昭和の日
  "2017-05-03", "2017-05-04", "2017-05-05",  # 憲法記念日〜こどもの日
  "2017-07-17",  # 海の日
  "2017-08-11",  # 山の日
  "2017-09-18",  # 敬老の日
  "2017-09-23",  # 秋分の日
  "2017-10-09",  # 体育の日
  "2017-11-03",  # 文化の日
  "2017-11-23",  # 勤労感謝の日
  "2017-12-23"   # 天皇誕生日
))

# 分析対象の日付ベクトル（例：2017年の全日）
dates <- seq.Date(from = as.Date("2017-01-01"), to = as.Date("2017-12-31"), by = "day")

# フラグ化
holiday_flag <- as.integer(dates %in% holidays_2017)

tenki <- read.csv("data/data.csv")
head(tenki)
tenki$rain_flag <- as.integer(tenki$降水量の合計.mm. > 20)

tenki$holiday_flag <- holiday_flag 
head(tenki)
xreg <- as.matrix(tenki[, c("holiday_flag")])

model <- auto.arima(nodogoshi_ts, xreg = xreg, seasonal = TRUE)
summary(model)

model <- auto.arima(nodogoshi_ts, xreg = xreg, seasonal = TRUE)
summary(model)

df <- data.frame(
  date = seq.Date(from = as.Date("2017-01-01"), to = as.Date("2017-12-31"), by = "day")
)
df$weekday <- weekdays(df$date)
df$weekday <- factor(df$weekday, levels = c("日曜日", "月曜日", "火曜日", "水曜日", "木曜日", "金曜日", "土曜日"))
weekday_dummies <- model.matrix(~ weekday, data = df)[, -1]  # 最初の列（intercept）は除く
tenki <- cbind(tenki, weekday_dummies)
tenki

nodogoshi <- ts(nodogoshi$のどごし, start = c(2017, 1), frequency = 1)
nodogoshi

xreg_vars <- c("rain_flag", "holiday_flag", "weekday月曜日", "weekday火曜日", "weekday水曜日", "weekday木曜日", "weekday金曜日", "weekday土曜日")
xreg <- as.matrix(tenki[, xreg_vars])

model <- auto.arima(nodogoshi, xreg = xreg, seasonal = FALSE)
summary(model)

ts_train <- ts(train_df$のどごし, frequency = 1)

# 説明変数（rain, holiday, 曜日など）
xreg_train <- as.matrix(train_df[, c("holiday_flag", "weekday月曜日", "weekday火曜日", 
                                     "weekday水曜日", "weekday木曜日", "weekday金曜日", "weekday土曜日")])

# モデル学習
model <- auto.arima(ts_train, xreg = xreg_train, seasonal = FALSE)

# 未来の説明変数（テスト用）
xreg_test <- as.matrix(test_df[, c( "holiday_flag", "weekday月曜日", "weekday火曜日", 
                                   "weekday水曜日", "weekday木曜日", "weekday金曜日", "weekday土曜日")])

# 予測（h = テストデータの行数）
forecast_result <- forecast(model, xreg = xreg_test, h = nrow(test_df))

# 予測結果
pred <- as.numeric(forecast_result$mean)

# 実測値との比較
actual <- test_df$のどごし

# 指標計算
library(Metrics)

rmse_val <- rmse(actual, pred)
mae_val  <- mae(actual, pred)
mape_val <- mape(actual, pred) * 100  # %

cat("RMSE:", rmse_val, "\nMAE:", mae_val, "\nMAPE:", mape_val, "%\n")

library(ggplot2)

result_df <- data.frame(
  date = test_df$日付,
  actual = actual,
  pred = pred
)
head(result_df)

ggplot(result_df, aes(x = date)) +
  geom_line(aes(y = actual), color = "black") +
  geom_line(aes(y = pred), color = "blue") +
  labs(title = "予測 vs 実測", y = "売上", x = "日付")

nrow(xreg_test) == nrow(test_df)    # TRUEか？
any(is.na(xreg_test))  
