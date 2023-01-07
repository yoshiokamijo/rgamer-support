## ch03_ex.R
## R codes for Ch. 3 of Kamijo and Yanai's Game Theory Book
## 2022-04-20 Yoshio Kamijo

library(tidyverse)
library(rgamer)

### 練習問題 3.1
g_ex3.1 <- normal_form(
  players = c("上條", "矢内"),
  s1 = c("グー", "チョキ", "パー"), 
  s2 = c("グー", "チョキ", "パー"),
  payoffs1 = c(0, 0, 2, 2, 0, 0, 0, 2, 0),
  payoffs2 = c(2, 2, 0, 0, 2, 2, 2, 0, 2)
)

g_ex3.1_sol <- solve_nfg(g_ex3.1)

dom(g_ex3.1)
# よって支配される戦略は存在しない。


### 練習問題 3.2
g_ex3.2 <- normal_form(
  players = c("大豚", "子豚"),
  s1 = c("スイッチ", "待つ"), 
  s2 = c("スイッチ", "待つ"),
  payoffs1 = c(7 - 5, 10, 4 - 5, 0), 
  payoffs2 = c(1, -1, 5, 0)
)

g_ex3.2_sol <- solve_nfg(g_ex3.2)

dom(g_ex3.2)
# 大豚、子豚ともに、スイッチは待つによって支配されるので、
# 互いに待つを選択すると予想できる

