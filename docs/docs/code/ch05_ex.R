## ch05_ex.R
## R codes for Ch. 5 of Kamijo and Yanai's Game Theory Book
## 2022-04-20 Yoshio Kamijo

library(rgamer)



### 練習問題 5.1

g_ex5.1 <- normal_form(
  players = c("1", "2"),
  s1 = c("C", "D"),
  s2 = c("C", "D"),
  payoffs1 = c(-2, -1, -10, -5) + 10, 
  payoffs2 = c(-2, -10, -1, -5) + 20
)

g_ex5.1_sol <- solve_nfg(g_ex5.1)
# よって、ナッシュ均衡は (D, D) であり、変化しない




### 練習問題 5.2
g_ex5.2 <- normal_form(
  players = c("1", "2"),
  s1 = c("20", "30"),
  s2 = c("20", "30"),
  payoffs1 = c(1000, 1200 - 100, 800, 900 - 100), 
  payoffs2 = c(1000, 800, 1200 - 100, 900 - 100)
)

g_ex5.2_sol <- solve_nfg(g_ex5.2, mixed = TRUE)
# このケースでは、(30, 30) も NE となっており、過剰放牧が生じる


# 整数範囲で考えるのならば、200 以上のペナルティが必要である。

g_ex5.2a <- normal_form(
  players = c("1", "2"),
  s1 = c("20", "30"),
  s2 = c("20", "30"),
  payoffs1 = c(1000, 1200 - 201, 800, 900 - 201), 
  payoffs2 = c(1000, 800, 1200 - 201, 900 - 201)
)

g_ex5.2a_sol <- solve_nfg(g_ex5.2a)
# NE は (20, 20) だけ


g_ex5.2b <- normal_form(
  players = c("1", "2"),
  s1 = c("20", "30"),
  s2 = c("20", "30"),
  payoffs1 = c(1000, 1200 - 199, 800, 900 - 199), 
  payoffs2 = c(1000, 800, 1200 - 199, 900 - 199)
)

g_ex5.2b_sol <- solve_nfg(g_ex5.2b)
# NE は (30, 20), (20, 30) であり、過剰放牧が起きる



### 練習問題 5.3

# game5_7 と同一のゲーム
g_ex5.3 <- normal_form(
    players = c("1", "2"),
    s1 = c("Join", "Not Join"),
    s2 = c("Join", "Not Join"),
    payoffs1 = c(950, 900, 900 - 100, 900), 
    payoffs2 = c(950, 900 - 100, 900, 900),
  )

g_ex5.3_sol <- solve_nfg(g_ex5.3)
  
set.seed(2022-04-20)
g_ex5.3_sim <- sim_game(g_ex5.3, 
                        n_samples = 50,
                        n_periods = 10
                        )

g_ex5.3_sim$plot_mean

# グラフから分かる通り、Join, Not Join は同程度に選択されている
# ただし、のちの章で説明される他の行動調整ルールを考えると、結果は
# 大きく変更される可能せがある。どのような NE に到達するのか、しない
# のか、は行動調整ルールに大きく依存している。

