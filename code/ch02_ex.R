## ch02_ex.R
## R codes for Ch. 2 of Kamijo and Yanai's Game Theory Book
## 2022-04-20 Yoshio Kamijo

library(rgamer)

### 練習問題 2.1
### 宝探しゲーム：か上條が宝を隠し、矢内が探すゲーム

g_ex2.1 <- normal_form(
  players = c("上條", "矢内"),
  s1 = c("Left", "Right"), 
  s2 = c("Left", "Right"),
  payoffs1 = c(0, 1, 1, 0),
  payoffs2 = c(1, 0, 0, 1)
)

g_ex2.1_sol <- solve_nfg(g_ex2.1)


### 練習問題 2.2
g_ex2.2 <- normal_form(
  players = c("上條", "矢内"),
  s1 = c("グー", "チョキ", "パー"),
  s2 = c("グー", "チョキ", "パー"), 
  payoffs1 = c(1, 2, 0, 0, 1, 2, 2, 0, 1), 
  payoffs2 = c(1, 0, 2, 2, 1, 0, 0, 2, 1),
  byrow = TRUE
)

g_ex2.2_sol <- solve_nfg(g_ex2.2)

