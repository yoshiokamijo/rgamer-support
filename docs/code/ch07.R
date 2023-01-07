## ch07.R
##
## R codes for Ch. 7 of Kamijo and Yanai's Game Theory Book
##
## 2021-05-26 Yuki Yanai
## 2022-03-16 YY
## 2022-04-06 YY
## 2022-05-02 YY
## 2022-09-07 YY

library(rgamer)
theme_set(theme_gray(base_size = 9,
                     base_family = "IPAexGothic"))


game7_1 <- normal_form(
  players = c("あなた", "バルタン星人"),
  s1 = c("グー", "パー"), 
  s2 = c("グー", "チョキ"),
  payoffs1 = c(1, 2, 2, 0), 
  payoffs2 = c(1, 0, 0, 2),
)

g7_1_sol <- solve_nfg(game7_1)


set.seed(2022-03-16)
g7_1_sim <- sim_game(game7_1, 
                     n_samples = 100, 
                     n_periods = 100,
                     plot_range_y = c(0.3, 0.7))
plot(g7_1_sim$plot_mean)

#cairo_pdf(file = file.path("figs", "ch07_baltan_sim.pdf"),
#          family = "sans", width = 6, height = 3)
#print(g7_1_sim$plot_mean)
#dev.off()

set.seed(2022-03-21)
g7_1_sim2 <- sim_learning(game7_1, 
                          n_samples = 100, 
                          n_periods = 100, 
                          type = "belief", 
                          lambda = 10)
g7_1_sim2$plot_mean

#cairo_pdf(file = file.path("figs", "ch07_baltan_sim2.pdf"),
#          family = "sans", width = 6, height = 3)
#print(g7_1_sim2$plot_mean)
#dev.off()


set.seed(2022-04-06)
g7_1_sim3 <- sim_fict(game7_1,
                      n_samples = 100,
                      n_periods = 100,
                      lambda = 100)
g7_1_sim3$plot_mean

#cairo_pdf(file = file.path("figs", "ch07_baltan_sim3_mean.pdf"),
#          family = "sans", width = 6, height = 3)
#print(g7_1_sim3$plot_mean)
#dev.off()

plot(g7_1_sim3$plot_B)

#cairo_pdf(file = file.path("figs", "ch07_baltan_sim3_B.pdf"),
#          family = "sans", width = 6, height = 3)
#print(g7_1_sim3$plot_B)
#dev.off()

plot(g7_1_sim3$plot_P)

#cairo_pdf(file = file.path("figs", "ch07_baltan_sim3_P.pdf"),
#          family = "sans", width = 6, height = 3)
#print(g7_1_sim3$plot_P)
#dev.off()
