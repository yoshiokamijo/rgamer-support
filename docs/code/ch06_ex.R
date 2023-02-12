## ch06_ex.R
## R codes for Ch. 6 of Kamijo and Yanai's Game Theory Book
## 2022-04-20 Yoshio Kamijo

library(rgamer)





### 練習問題 6.1

# game3_2 と同一のゲーム
g_ex6.1 <- normal_form(
  players = c("大豚", "子豚"),
  s1 = c("スイッチ", "待つ"), 
  s2 = c("スイッチ", "待つ"),
  payoffs1 = c(7, 10, 4, 0), 
  payoffs2 = c(1, -1, 5, 0)
)

g_ex6.1_sol <- solve_nfg(g_ex6.1, mixed = TRUE)
g_ex6.1_sol$br_plot






### 練習問題 6.2

# game4_1 と同一のゲーム
g_ex6.2 <- normal_form(
  players = c("A", "B"),
  s1 = c("C", "D"), 
  s2 = c("C", "D"),
  payoffs1 = c(-2, -1, -10, -5), 
  payoffs2 = c(-2, -10, -1, -5),
)

g_ex6.2_sol <- solve_nfg(g_ex6.2, mixed = TRUE)
g_ex6.2_sol$br_plot




### 練習問題 6.3

# game4_2 と同一のゲーム
g_ex6.3 <- normal_form(
  players = c("1", "2"),
  s1 = c("T", "M", "B"),
  s2 = c("L", "C", "R"),
  payoffs1 = c(1, 0, 0, 0, 1, 0, 0, 0, 1), 
  payoffs2 = c(0, 0, 1, 0, 1, 0, 1, 0, 0),
)

g_ex6.3_sol <- solve_nfg(g_ex6.3, mixed = TRUE)
g_ex6.3_sol$msNE

# [(1/3, 1/3, 1/3), (1/3, 1/3, 1/3)] も msNE である
# ただし、このゲームには、[(1/2, 0, 1/2), (1/2, 0, 1/2)] という msNE もある





### 練習問題 6.4

# game6_1 と同一のゲーム
g_ex6.4 <- normal_form(
  players = c("あなた", "バルタン星人"), 
  s1 = c("グー", "チョキ", "パー"),
  s2 = c("グー", "チョキ"),
  payoffs1 = c(1, 0, 2, 2, 1, 0), 
  payoffs2 = c(1, 2, 0, 0, 1, 2),
)

g_ex6.4_sol <- solve_nfg(g_ex6.4, mixed = TRUE)
g_ex6.4_sol$msNE

# このゲームには、あなたが「チョキ」を正の確率で選択するような msNE が存在しないため
# Full Support の msNE は存在しない。このようなケースでは msNE を計算するのは非常に困難である
# Full  Support でない msNE の候補を探す際に、[It might be useful to check 'msNE_df'] という
# メッセージに従い、msNE_df をみることが有益。
# これには、Full Support でない純粋戦略の組に対して期待利得の同値定理を成り立たせるような、
# 混合戦略の組が計算されている。
# ここから msNE を見つけることができる場合もある。

g_ex6.4_sol$msNE_df

# これをチェックすると、
# (グー, パー) (グー, チョキ) を (2/3, 1/3), (2/3, 1/3) でとるような混合戦略
# の組が、同値定理を満たすことが確認できる。これは実際に、msNE である。

# その一方で、(チョキ, パー) (グー, チョキ) を (2/3, 1/3), (1/3, 2/3) でとるような
# 混合戦略の組も同値定理を満たすことがわかる。しかし、player 1 は、チョキに割り振った
# 確率を グーに割り振ったほうが得なので、これは msNE ではない

