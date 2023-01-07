## ch02.R
##
## R codes for Ch. 2 of Kamijo and Yanai's Game Theory Book
##
## 2022-03-28 Yuki Yanai
## 2022-04-07 YY
## 2022-05-02 YY
## 2022-09-07 YY

library(rgamer)

game2_1 <- normal_form(
  players = c("矢内", "上條"),
  s1 = c("表", "裏"), 
  s2 = c("表", "裏"),
  payoffs1 = c(1, 0, 0, 1), 
  payoffs2 = c(0, 1, 1, 0),
)

g2_1_sol <- solve_nfg(game2_1)


game2_2 <- normal_form(
  players = c("上條", "矢内"),
  s1 = c("グー", "チョキ", "パー"), 
  s2 = c("グー", "チョキ", "パー"),
  payoffs1 = c(1, 0, 2, 2, 1, 0, 0, 2, 1), 
  payoffs2 = c(1, 2, 0, 0, 1, 2, 2, 0, 1),
)

g2_2_sol <- solve_nfg(game2_2)
