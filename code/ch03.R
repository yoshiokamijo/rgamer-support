## ch03.R
##
## R codes for Ch. 3 of Kamijo and Yanai's Game Theory Book
##
## 2021-03-09 Yuki Yanai
## 2021-03-24 YY
## 2022-03-08 YY
## 2022-04-07 YY
## 2022-05-02 YY

library(rgamer)

game3_1 <- normal_form(
  players = c("上條", "矢内"),
  s1 = c("グー", "チョキ", "パー"), 
  s2 = c("グー", "チョキ", "パー"),
  payoffs1 = c(0, 0, 2, 2, 0, 0, 0, 2, 2),
  payoffs2 = c(2, 2, 0, 0, 2, 2, 2, 0, 0)
)

solve_nfg(game3_1, mark_br = FALSE)$table

game3_1b <- eliminate_strategy(game3_1,
                               player = "上條",
                               eliminated = "チョキ")
solve_nfg(game3_1b, mark_br = FALSE)$table

g3_1_dominated <- dom(game3_1)


game3_2 <- normal_form(
  players = c("大豚", "子豚"),
  s1 = c("スイッチ", "待つ"), 
  s2 = c("スイッチ", "待つ"),
  payoffs1 = c(7, 10, 4, 0), 
  payoffs2 = c(1, -1, 5, 0),
)

solve_nfg(game3_2, mark_br = FALSE)$table

game3_2a <- eliminate_strategy(
  game3_2,
  player = "子豚",
  eliminated = "スイッチ")
solve_nfg(game3_2a, mark_br = FALSE)$table

g3_2a_dominated <- dom(game3_2a)

game3_2b <- eliminate_strategy(
  game3_2a,
  player = "大豚",
  eliminated = "待つ")

solve_nfg(game3_2b, mark_br = FALSE)$table0

g3_2b_dominated <- dom(game3_2b)


g3_1_dominant <- dom(game3_1, type = "dominant")
g3_2_dominant <- dom(game3_2, type = "dominant")
