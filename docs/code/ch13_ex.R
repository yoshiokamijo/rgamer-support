## ch13_ex.R
## R codes for Ch. 13 of Kamijo and Yanai's Game Theory Book
## 2022-04-27, 2023-02-11, Yoshio Kamijo

library(rgamer)


### 練習問題 13.1

# boston, man proposal
m_ex13.1_bm <- matching(
  g1_names = c("M1", "M2"),
  g1_prefs = list(c("W1", "W2", "W3"),
                  c("W1", "W3", "W2")),
  g2_names = c("W1", "W2", "W3"),
  g2_prefs = list(c("M1", "M2"),
                  c("M2", "M1"),
                  c("M1", "M2")), 
  switch = FALSE,
  verbose = FALSE,
  algorithm = "Boston"
)

print(m_ex13.1_bm)


# boston, woman proposal
m_ex13.1_bw <- matching(
  g1_names = c("M1", "M2"),
  g1_prefs = list(c("W1", "W2", "W3"),
                  c("W1", "W3", "W2")),
  g2_names = c("W1", "W2", "W3"),
  g2_prefs = list(c("M1", "M2"),
                  c("M2", "M1"),
                  c("M1", "M2")), 
  switch = TRUE,
  verbose = FALSE,
  algorithm = "Boston"
)

print(m_ex13.1_bw)


# DA, man proposal
m_ex13.1_dm <- matching(
  g1_names = c("M1", "M2"),
  g1_prefs = list(c("W1", "W2", "W3"),
                  c("W1", "W3", "W2")),
  g2_names = c("W1", "W2", "W3"),
  g2_prefs = list(c("M1", "M2"),
                  c("M2", "M1"),
                  c("M1", "M2")), 
  switch = FALSE,
  verbose = FALSE,
  algorithm = "DA"
)

print(m_ex13.1_dm)


# DA, woman proposal
m_ex13.1_dw <- matching(
  g1_names = c("M1", "M2"),
  g1_prefs = list(c("W1", "W2", "W3"),
                  c("W1", "W3", "W2")),
  g2_names = c("W1", "W2", "W3"),
  g2_prefs = list(c("M1", "M2"),
                  c("M2", "M1"),
                  c("M1", "M2")), 
  switch = TRUE,
  verbose = FALSE,
  algorithm = "DA"
)

print(m_ex13.1_dw)


# 安定性の確認
is_stable(m_ex13.1_bm)
is_stable(m_ex13.1_bw)
is_stable(m_ex13.1_dm)
is_stable(m_ex13.1_dw)







### 練習問題 13.2

# boston, man proposal
m_ex13.2_bm <- matching(
  g1_names = c("M1", "M2", "M3"),
  g1_prefs = list(c("W1"),
                  c("W1", "W3"),
                  c("W2", "W3", "W1")),
  g2_names = c("W1", "W2", "W3"),
  g2_prefs = list(c("M1", "M2", "M3"),
                  c("M2", "M3", "M1"),
                  c("M1", "M3", "M2")), 
  switch = FALSE,
  verbose = FALSE,
  algorithm = "Boston"
)

print(m_ex13.2_bm)


# boston, woman proposal
m_ex13.2_bw <- matching(
  g1_names = c("M1", "M2", "M3"),
  g1_prefs = list(c("W1"),
                  c("W1", "W3"),
                  c("W2", "W3", "W1")),
  g2_names = c("W1", "W2", "W3"),
  g2_prefs = list(c("M1", "M2", "M3"),
                  c("M2", "M3", "M1"),
                  c("M1", "M3", "M2")), 
  switch = TRUE,
  verbose = FALSE,
  algorithm = "Boston"
)

print(m_ex13.2_bw)


# DA, man proposal
m_ex13.2_dm <- matching(
  g1_names = c("M1", "M2", "M3"),
  g1_prefs = list(c("W1"),
                  c("W1", "W3"),
                  c("W2", "W3", "W1")),
  g2_names = c("W1", "W2", "W3"),
  g2_prefs = list(c("M1", "M2", "M3"),
                  c("M2", "M3", "M1"),
                  c("M1", "M3", "M2")), 
  switch = FALSE,
  verbose = FALSE,
  algorithm = "DA"
)

print(m_ex13.2_dm)


# DA, woman proposal
m_ex13.2_dw <- matching(
  g1_names = c("M1", "M2", "M3"),
  g1_prefs = list(c("W1"),
                  c("W1", "W3"),
                  c("W2", "W3", "W1")),
  g2_names = c("W1", "W2", "W3"),
  g2_prefs = list(c("M1", "M2", "M3"),
                  c("M2", "M3", "M1"),
                  c("M1", "M3", "M2")), 
  switch = TRUE,
  verbose = FALSE,
  algorithm = "DA"
)

print(m_ex13.2_dw)


# 安定性の確認
is_stable(m_ex13.2_bm)
is_stable(m_ex13.2_bw)
is_stable(m_ex13.2_dm)
is_stable(m_ex13.2_dw)


### 練習問題 13.3
# 省略






### 練習問題 13.4

set.seed(1234)

# 各グループの人数を指定 
n <- 20

# 男性側の選好をランダムに生成して、リストにする 
m_prefs <- lapply(1:n,FUN = function(i){sample(1:n, n, replace=FALSE)})

# 女性側の選好をランダムに生成して、リストにする 
w_prefs <- lapply(1:n,FUN = function(i){sample(1:n, n, replace=FALSE)})

# マッチングを計算 
m_ex13_4 <- matching(
  g1_names = 1:n, # 男性の名前は 1, 2, ..., n 
  g1_prefs = m_prefs,
  g2_names = 1:n, # 女性の名前は 1, 2, ..., n 
  g2_prefs = w_prefs,
  algorithm = "B",
  verbose = TRUE
)

print(m_ex13_4) #決まった順

is_stable(m_ex13_4) 
# blocking pair は５組存在

m_ex13_4$data




### 練習問題 13.5


# 各グループの人数を指定 
n <- 20

# 男性側の選好をランダムに生成して、リストにする 
m_prefs <- lapply(1:n,FUN = function(i){sample(1:n, n, replace=FALSE)})

# 女性側の選好をランダムに生成して、リストにする 
w_prefs <- lapply(1:n,FUN = function(i){sample(1:n, n, replace=FALSE)})

# マッチングを計算 
m_ex13_5 <- matching(
  g1_names = 1:n, # 男性の名前は 1, 2, ..., n 
  g1_prefs = m_prefs,
  g2_names = 1:n, # 女性の名前は 1, 2, ..., n 
  g2_prefs = w_prefs,
  algorithm = "DA",
  verbose = FALSE
)

print(m_ex13_5)

is_stable(m_ex13_5) 

# 上記一連のコードを実行するごとにマッチング結果は変化するが、常に stable である。




















