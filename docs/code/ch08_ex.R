## ch08_ex.R
## R codes for Ch. 8 of Kamijo and Yanai's Game Theory Book
## 2022-04-20, 2023-02-11,Yoshio Kamijo

library(rgamer)





### 練習問題 8.1

# game8_3 と同一のゲーム
g_ex8.1 <- normal_form(
  players = c("F1", "F2"),
  pars = c("x1", "x2"),
  par1_lim = c(0, 60),
  par2_lim = c(0, 60),
  payoffs1 = "(60 - x1 - x2) * x1 - 20 * x1", 
  payoffs2 = "(60 - x1 - x2) * x2",
)

g_ex8.1_sim <- sim_game(g_ex8.1, 
                       n_samples = 1,
                       n_periods = 5, 
                       init1 = 20, 
                       init2 = 20)

g_ex8.1_sim$plot_samples





### 練習問題 8.2

g_ex8.2 <- normal_form(
  players = c("F1", "F2"),
  pars = c("x1", "x2"),
  par1_lim = c(0, 60),
  par2_lim = c(0, 60),
  payoffs1 = "(60 - x1 - x2) * x1 + x1 + x2", 
  payoffs2 = "(60 - x1 - x2) * x2",
)

g_ex8.2_sol <- solve_nfg(g_ex8.2)

# NE は (20.7, 19.7)
x1 <- 20.7
x2 <- 19.7

(60 - x1 - x2) * x1
(60 - x1 - x2) * x2
# よって、公企業のほうが利潤が高い!





### 練習問題 8.3

pi1r <- function(x1, x2){
  endowment <- 0
  punish <- 10
  
  if (x1 == 10){
    profit <- endowment - x1
  } else if (x1 < 10 & x1 > x2) {
    profit <- endowment - x1
  } else {
    profit <- endowment - x1 - punish
  }
  
  return(profit)
  
}


pi2r <- function(x1, x2){
  endowment <- 0
  punish <- 10
  
  if (x2 == 10){
    profit <- endowment - x2
  } else if (x2 < 10 & x2 > x1) {
    profit <- endowment - x2
  } else {
    profit <- endowment - x2 - punish
  }
  
  return(profit)
  
}


g_ex8.3 <- normal_form(
  s1 = seq(from = 0, to = 10, by = 1),
  s2 = seq(from = 0, to = 10, by = 1),
  payoffs1 = pi1r,
  payoffs2 = pi2r,
  pars = c("x1", "x2"),
  discretize = TRUE,
)

g_ex8.3_sol <- solve_nfg(g_ex8.3)

