## ch04_ex.R
## R codes for Ch. 4 of Kamijo and Yanai's Game Theory Book
## 2022-04-20 Yoshio Kamijo

library(rgamer)

### 練習問題 4.1

# 日本語文字化け問題への対処のために
# 黙秘、自白をそれぞれ C, D とする

# game4_1 と同一のゲーム
g_ex4.1 <- normal_form(
  players = c("A", "B"),
  s1 = c("C", "D"), 
  s2 = c("C", "D"),
  payoffs1 = c(-2, -1, -10, -5), 
  payoffs2 = c(-2, -10, -1, -5),
)

set.seed(2022-04-20)
g_ex4.1_sim <- sim_game(g_ex4.1, 
                      n_samples = 50,
                      n_periods = 10)

g_ex4.1_sim$plot_mean



### 練習問題 4.2

# game4_2 と同一のゲーム
g_ex4.2 <- normal_form(
  players = c("1", "2"),
  s1 = c("T", "M", "B"),
  s2 = c("L", "C", "R"),
  payoffs1 = c(1, 0, 0, 0, 1, 0, 0, 0, 1), 
  payoffs2 = c(0, 0, 1, 0, 1, 0, 1, 0, 0),
)

set.seed(2022-04-20)
g_ex4.2_sim <- sim_game(g_ex4.2, 
                        n_samples = 50,
                        n_periods = 10)

g_ex4.2_sim$plot_mean

# グラフから分かる通り、(M, C) に到達するとは限らない



### 練習問題 4.3
set.seed(2022-04-20)
g_ex4.2_sim <- sim_game(g_ex4.2, 
                        n_samples = 50,
                        n_periods = 10, 
                        init1 = "M", 
                        init2 ="C")

g_ex4.2_sim$plot_mean

# グラフから分かる通り、(M, C) から始まれば、
# そこから変化はしない。これがナッシュ均衡の性質である


