## MASSパッケージのcorresp関数を使う方法 ##
library(MASS)

# データの読み込み
N1 = read.csv("cross2.csv", header=T, row.names = 1)
colnames(N1) <- gsub("^X", "", colnames(N1))
head(N1)


# 対応分析の実施
res1 = corresp(N1, nf=2)
res1

# プロット図の出力
plot(res1)
abline(h=0, v=0)



## caパッケージのca関数を使う方法 ##
install.packages("ca")
library(ca)

# 対応分析の実施
res2 = ca(N1)
res2

# プロット図の出力
plot(res2)
plot(res2, arrows = c(TRUE, TRUE)) 


#（参考）出力される座標値とプロット図上の座標値の関係
# 寄与率を考慮している
res2
res2$sv
res2$rowcoord
res2$colcoord

x1 <- res2$sv[1] * res2$rowcoord[,1]
x2 <- res2$sv[2] * res2$rowcoord[,2]
y1 <- res2$sv[1] * res2$colcoord[,1]
y2 <- res2$sv[2] * res2$colcoord[,2]

plot(c(x1,y1),c(x2,y2))
abline(h=0,v=0)



## FactoMineRパッケージを使う方法　##

# パッケージのインストール
install.packages("FactoMineR")
install.packages("factoextra")

# 必要なパッケージの読み込み
library(FactoMineR)
library(factoextra)  # 結果の可視化に便利

# 対応分析の実行
res.ca <- CA(N1)

# 結果の要約表示
summary(res.ca)

# 固有値（寄与率など）の確認
eig.val <- res.ca$eig
print(eig.val)

## factoextraパッケージを使ったプロット

# プロット：行（row）と列（column）を同時に描く（バイプロット）
fviz_ca_biplot(res.ca, repel = TRUE)  # repel=TRUEでラベルが重ならない

# 追加：行と列を別々にプロットする場合
fviz_ca_row(res.ca, repel = TRUE)    # 行の座標だけ
fviz_ca_col(res.ca, repel = TRUE)    # 列の座標だけ

