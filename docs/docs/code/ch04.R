## ch04.R
##
## R codes for Ch. 4 of Kamijo and Yanai's Game Theory Book
##
## 2021-03-09 Yuki Yanai
## 2022-03-08 YY
## 2022-03-16 YY
## 2022-03-30 YY
## 2022-04-07 YY
## 2022-04-20 YY
## 2022-05-02 YY
## 2022-09-07 YY

library(rgamer)
theme_set(theme_gray(base_size = 9,
                     base_family = "IPAexGothic"))

game4_1 <- normal_form(
  players = c("A", "B"),
  s1 = c("黙秘", "自白"), 
  s2 = c("黙秘", "自白"),
  payoffs1 = c(-2, -1, -10, -5), 
  payoffs2 = c(-2, -10, -1, -5),
)

g4_1_sol <- solve_nfg(game4_1, mark_br = TRUE)

g4_1_dominant <- dom(game4_1, type = "dominant")


game3_2 <- normal_form(
  players = c("大豚", "子豚"),
  s1 = c("スイッチ", "待つ"), 
  s2 = c("スイッチ", "待つ"),
  payoffs1 = c(7, 10, 4, 0), 
  payoffs2 = c(1, -1, 5, 0),
)

g3_2_sol <- solve_nfg(game3_2, mark_br = TRUE)


game4_2 <- normal_form(
  players = c("1", "2"),
  s1 = c("T", "M", "B"), 
  s2 = c("L", "C", "R"),
  payoffs1 = c(1, 0, 0, 0, 1, 0, 0 , 0, 1), 
  payoffs2 = c(0, 0, 1, 0, 1, 0, 1, 0, 0),
)
g4_2_sol <- solve_nfg(game4_2)


set.seed(2022-03-16)
sim_g3_2 <- sim_game(game3_2, 
                     n_samples = 100,
                     n_periods = 8)
plot(sim_g3_2$plot_mean)

#cairo_pdf(file = file.path("figs", "ch04_simulation.pdf"),
#          family = "sans", width = 6, height = 3)
#print(sim_g3_2$plot_mean)
#dev.off()
