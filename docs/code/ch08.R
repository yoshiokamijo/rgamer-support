## ch08.R
##
## R codes for Ch. 8 of Kamijo and Yanai's Game Theory Book
##
## 2020-11-13 Yuki Yanai
## 2021-05-26 YY: Rename from ch07.R to ch08.R
## 2022-03-16 YY
## 2022-03-23 YY
## 2022-04-07 YY
## 2022-05-02 YY
## 2022-09-07 YY

library(rgamer)
library(patchwork)
theme_set(theme_gray(base_size = 9,
                     base_family = "IPAexGothic"))


# Cournot competition: binary choice
game8_1 <- normal_form(
  players = c("企業１", "企業２"),
  s1 = c("15", "20"),
  s2 = c("15", "20"),
  payoffs1 = c(450, 500, 375, 400),
  payoffs2 = c(450, 375, 500, 400),
)

g8_1_sol <- solve_nfg(game8_1)


game8_1b <- normal_form(
  players = c("企業１", "企業２"),
  s1 = c("20", "30"),
  s2 = c("20", "30"),
  payoffs1 = c(400, 300, 200, 0),
  payoffs2 = c(400, 200, 300, 0)
)

g8_1b_sol <- solve_nfg(game8_1b)


# Cournot competition
game8_2 <- normal_form(
  players = c("企業１", "企業２"),
  pars = c("x1", "x2"),
  par1_lim = c(0, 60),
  par2_lim = c(0, 60),
  payoffs1 = "(60 - x1 - x2) * x1",
  payoffs2 = "(60 - x1 - x2) * x2",
)
g8_2_sol <- solve_nfg(game8_2)

game8_3 <- normal_form(
  players = c("企業１", "企業２"),
  pars = c("x1", "x2"),
  par1_lim = c(0, 60),
  par2_lim = c(0, 60),
  payoffs1 = "(60 - x1 - x2) * x1 - 20 * x1",
  payoffs2 = "(60 - x1 - x2) * x2",
)
g8_3_sol <- solve_nfg(game8_3) 

game8_4 <- normal_form(
  players = c("企業１", "企業２"),
  pars = c("x1", "x2"),
  par1_lim = c(0, 60),
  par2_lim = c(0, 60),
  payoffs1 = "(60 - x1 - x2) * x1 - 20 * x1",
  payoffs2 = "(60 - x1 - x2) * x2 - 20 * x2",
)
g8_4_sol <- solve_nfg(game8_4) 

# use the same scale for 3 figs.
br8_2 <- g8_2_sol$br_plot_NE +
  xlim(0, 70) + 
  ylim(0, 70) + 
  labs(caption = "(a) game8_2 の最適反応") +
  theme(legend.position = "none",
        plot.caption = element_text(size = 9,
                                    hjust = 0.5)) 
plot(br8_2)

br8_3 <- g8_3_sol$br_plot_NE +
  xlim(0, 70) + 
  ylim(0, 70) +
  labs(caption = "(b) game8_3 の最適反応") +
  theme(legend.position = "right",
        plot.caption = element_text(size = 9,
                                    hjust = 0.5))
plot(br8_3)

br8_4 <- g8_4_sol$br_plot_NE +
  xlim(0, 70) + 
  ylim(0, 70) +
　labs(caption = "(c) game8_4 の最適反応") +
  theme(legend.position = "none",
        plot.caption = element_text(size = 9,
                                    hjust = 0.5))
plot(br8_4)

plot(br8_2 / br8_3 / br8_4)

#cairo_pdf(file = file.path("figs", "ch08_br_cournot.pdf"),
#          width = 5, height = 8)
#plot(br8_2 / br8_3 / br8_4)
#dev.off()


# Similar fig without NE marked on plots
br8_2b <- g8_2_sol$br_plot +
  xlim(0, 70) + 
  ylim(0, 70) + 
  labs(caption = "(a) game8_2 の最適反応") +
  theme(legend.position = "none",
        plot.caption = element_text(size = 9,
                                    hjust = 0.5)) 
plot(br8_2b)

br8_3b <- g8_3_sol$br_plot +
  xlim(0, 70) + 
  ylim(0, 70) +
  labs(caption = "(b) game8_3 の最適反応") +
  theme(legend.position = "right",
        plot.caption = element_text(size = 9,
                                    hjust = 0.5))
plot(br8_3b)

br8_4b <- g8_4_sol$br_plot +
  xlim(0, 70) + 
  ylim(0, 70) +
　labs(caption = "(c) game8_4 の最適反応") +
  theme(legend.position = "none",
        plot.caption = element_text(size = 9,
                                    hjust = 0.5))
plot(br8_4b)

plot(br8_2b / br8_3b / br8_4b)

#cairo_pdf(file = file.path("figs", "fig8-1-rev.pdf"),
#          width = 5, height = 8)
#plot(br8_2b / br8_3b / br8_4b)
#dev.off()



## Simulation
set.seed(2022-03-16)
g8_2_sim <- sim_game(game8_2, 
                     n_samples = 30, 
                     n_periods = 20)

g8_2_sim$plot_samples

#cairo_pdf(file = file.path("figs", "ch08_cournot_sim.pdf"),
#          width = 6, height = 2.5)
#plot(g8_2_sim$plot_samples)
#dev.off()

g8_2_imi <- sim_game(game8_2, 
                     n_samples = 30,
                     n_periods = 50 , 
                     type = "imitation",
                     eta = 0.1,
                     plot_range_y = "full")
g8_2_imi$plot_mean

cairo_pdf(file = file.path("figs", "ch08_cournot_imitation.pdf"),
          width = 6, height = 2.5)
plot(g8_2_imi$plot_mean)
dev.off()



## Bertrand competition
pi_b_1 <- function(p1, p2) {
  if (p1 > p2) { # 相手よりも価格が高い場合
    profit <- 0
  } else if (p1 == p2) { # 価格が等しい場合
    profit <- (60 - p1) * p1 / 2
  } else { # その他の場合
    profit <- (60 - p1) * p1
  }
  return(profit)
}

pi_b_2 <- function(p1, p2) {
  if (p2 > p1) { # 相手よりも価格が高い場合
    profit <- 0
  } else if (p2 == p1) { # 価格が等しい場合
    profit <- (60 - p2) * p2 / 2
  } else { # その他の場合
    profit <- (60 - p2) * p2
  }
  return(profit)
}

game8_6 <- normal_form(
  payoffs1 = pi_b_1,
  payoffs2 = pi_b_2,
  pars = c("p1", "p2"),
  par1_lim = c(0, 60),
  par2_lim = c(0, 60),
)
g8_6_sol <- solve_nfg(game8_6)

#cairo_pdf(file = file.path("figs", "ch08_br_g8_6.pdf"),
#          family = "sans", width = 4, height = 2.5)
#plot(g8_6_sol$br_plot)
#dev.off()


game8_7 <- normal_form(
  s1 = seq(from = 0, to = 10, by = 2),
  s2 = seq(from = 0, to = 10, by = 2),
  payoffs1 = pi_b_1,
  payoffs2 = pi_b_2,
  pars = c("p1", "p2"),
  discretize = TRUE,
)

g8_7_sol <- solve_nfg(game8_7)


game8_7b <- normal_form(
  s1 = seq(from = 0, to = 10, by = 1),
  s2 = seq(from = 0, to = 10, by = 1),
  payoffs1 = pi_b_1,
  payoffs2 = pi_b_2,
  pars = c("p1", "p2"),
  discretize = TRUE,
)

g8_7b_sol <- solve_nfg(game8_7b)


pi_b_1_b <- function(p1, p2) {
  if (p1 > p2) { # 相手よりも価格が高い場合
    profit <- 0
  } else if (p1 == p2) { # 価格が等しい場合
    profit <- (60 - p1) * (p1 - 10) / 2
  } else { # その他の場合
    profit <- (60 - p1) * (p1 - 10)
  }
  return(profit)
}

game8_8 <- normal_form(
  s1 = seq(from = 0, to = 20, by = 1),
  s2 = seq(from = 0, to = 20, by = 1),
  payoffs1 = pi_b_1_b,
  payoffs2 = pi_b_2,
  pars = c("p1", "p2"),
  discretize = TRUE,
)
g8_8_sol <- solve_nfg(game8_8, show_table = FALSE)
