## ch05.R
##
## R codes for Ch. 5 of Kamijo and Yanai's Game Theory Book
##
## 2021-03-23 Yuki Yanai
## 2021-05-25 YY
## 2022-03-08 YY
## 2022-03-16 YY
## 2022-03-23 YY
## 2022-04-07 YY
## 2022-05-02 YY

library(rgamer)
theme_set(theme_gray(base_family = "IPAexGothic",
                     base_size = 9))

game5_1 <- normal_form(
  players = c("1", "2"),
  s1 = c("C", "D"), 
  s2 = c("C", "D"),
  payoffs1 = c(-2, -1, -10, -5) + 10, 
  payoffs2 = c(-2, -10, -1, -5) + 10,
)

g5_1_sol <- solve_nfg(game5_1)


game5_2 <- normal_form(
  players = c("1", "2"),
  s1 = c("20", "30"),
  s2 = c("20", "30"),
  payoffs1 = c(1000, 1200, 800, 900),
  payoffs2 = c(1000, 800, 1200, 900),
)

g5_2_sol <- solve_nfg(game5_2)


game5_3 <- normal_form(
  players = c("1", "2"),
  s1 = c("20", "30"),
  s2 = c("20", "30"),
  payoffs1 = c(1000, 1200 - 400, 800, 900 - 400),
  payoffs2 = c(1000, 800, 1200 - 400, 900 - 400),
)

g5_3_sol <- solve_nfg(game5_3)


game5_4 <- normal_form(
  players = c("1", "2"),
  s1 = c("参加する", "参加しない"), 
  s2 = c("参加する", "参加しない"),
  payoffs1 = c(950, 900, 900, 900), 
  payoffs2 = c(950, 900, 900, 900),
)

g5_4_sol <- solve_nfg(game5_4)


set.seed(2021-05-25)
g5_4_sim <- sim_game(game5_4,
                     n_samples = 100,
                     n_periods = 10)
g5_4_sim$plot_mean

#cairo_pdf(file = file.path("figs", "ch05_sim_commons1.pdf"),
#          family = "sans", width = 6, height = 2.5)
#print(g5_4_sim$plot_mean)
#dev.off()


game5_5 <- normal_form(
  players = c("1", "2"),
  s1 = c("表", "裏"),
  s2 = c("表", "裏"),
  payoffs1 = c(2, 0, 0, 1),
  payoffs2 = c(2, 0, 0, 1),
)

g5_5_sol <- solve_nfg(game5_5)


set.seed(2021-05-25)
g5_5_sim <- sim_game(game5_5,
                     n_samples = 100,
                     n_periods = 10,
                     plot_range_y = c(0.3, 0.7))

g5_5_sim$plot_mean

cairo_pdf(file = file.path("figs", "ch05_coord1.pdf"), 
          family = "sans", width = 6, height = 2.5)
print(g5_5_sim$plot_mean)
dev.off()


game5_6 <- normal_form(
  players = c("1", "2"),
  s1 = c("鹿", "兎"),
  s2 = c("鹿", "兎"),
  payoffs1 = c(3, 2, 0, 2),
  payoffs2 = c(3, 0, 2, 2),
)

g5_6_sol <- solve_nfg(game5_6)


game5_7 <- normal_form(
  players = c("1", "2"),
  s1 = c("参加する", "参加しない"), 
  s2 = c("参加する", "参加しない"),
  payoffs1 = c(950, 900, 900 - 100, 900), 
  payoffs2 = c(950, 900 - 100, 900, 900),
)

g5_7_sol <- solve_nfg(game5_7)
