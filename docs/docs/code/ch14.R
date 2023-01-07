## ch14.R
##
## R codes for Ch. 14 of Kamijo and Yanai's Game Theory Book
##
## 2022-03-31 Yuki Yanai
## 2022-04-01 YY
## 2022-04-04 YY
## 2022-04-06 YY
## 2022-04-07 YY
## 2022-04-20 YY
## 2022-05-08 YY

library(tidyverse)
library(patchwork)
library(rgamer)
theme_set(theme_gray(base_size = 9,
                     base_family = "IPAexGothic"))

## Cournot competition, again
game14_1 <- normal_form(
  players = c("企業1", "企業2"),
  pars = c("x1", "x2"),
  par1_lim = c(0, 60),
  par2_lim = c(0, 60),
  payoffs1 = "(60 - x1 - x2) * x1",
  payoffs2 = "(60 - x1 - x2) * x2",
)

set.seed(2022-04-06)
g14_1_sim <- sim_game(game14_1,
                      n_samples = 1,
                      n_periods = 9,
                      omega = 0,
                      init1 = 28,
                      init2 = 12)

draw_process <- function(data,
                         xrange = c(0, 60),
                         yrange = c(0, 60),
                         title = "") {
  df_long <- data
  df1 <- df_long %>% 
    filter(player == "企業1") %>% 
    rename(x1 = strategy) %>% 
    select(period, x1)
  df2 <- df_long %>% 
    filter(player == "企業2") %>% 
    rename(x2 = strategy) %>% 
    select(period, x2)
  df_wide <- full_join(df1, df2, by = "period")

  p <- ggplot(df_wide, 
              aes(x = x1, 
                  y = x2)) +
    geom_abline(data = NULL,
                intercept = 30,
                slope = -0.5,
                color = "gray50") +
    geom_abline(data = NULL,
                intercept = 60,
                slope = -2,
                color = "gray50") +
    geom_segment(aes(xend = c(tail(x1, n = -1), NA),
                     yend = c(tail(x2, n = -1), NA)),
                 arrow = arrow(length = unit(0.2, "cm"),
                               type = "open"),
                 color = "gray50") +
    geom_point() +
    xlim(xrange[1], xrange[2]) +
    ylim(yrange[1], yrange[2]) +
    labs(x = expression(x[1]),
         y = expression(x[2]),
         subtitle = title)
  p
}

  
c_d_1 <- draw_process(g8_2_sim$data,
                      xrange = c(15, 30),
                      yrange = c(10, 25)) 
cairo_pdf(file.path("figs", "ch14_cournot_sim1.pdf"),
          width = 4, height = 4)
print(c_d_1)
dev.off()


# With inertia (omega > 0)
set.seed(2022-03-31)
g14_1_omega_01 <- sim_game(game14_1,
                           n_samples = 50,
                           n_periods = 20,
                           omega = 0.1,
                           init1 = 28,
                           init2 = 12, 
                           plot_range = "full")
#p1 <- draw_process(g14_1_omega_1$data,
#                   title = "omega = 0.1",
#                   xrange = c(15, 30),
#                   yrange = c(10, 25))
#p1
#g14_1_omega_2 <- sim_game(game14_1,
#                         n_samples = 1,
#                         n_periods = 20,
#                         omega = 0.3,
#                         init1 = 28,
#                         init2 = 12,
#                         plot_range_y = "full")
#p2 <- draw_process(g14_1_omega_2$data,
#                   title = "omega = 0.3",
#                   xrange = c(15, 30),
#                   yrange = c(10, 25))
#p2
#g14_1_omega_3 <- sim_game(game14_1,
#                         n_samples = 1,
#                         n_periods = 20,
#                         omega = 0.5,
#                         init1 = 28,
#                        init2 = 12,
#                         plot_range_y = "full")
#p3 <- draw_process(g14_1_omega_3$data,
#                   title = "omega = 0.5",
#                   xrange = c(15, 30),
#                   yrange = c(10, 25))
#p3
g14_1_omega_08 <- sim_game(game14_1,
                           n_samples = 50,
                           n_periods = 20,
                           omega = 0.8,
                           init1 = 28,
                           init2 = 12,
                           plot_range = "full")
#p4 <- draw_process(g14_1_omega_4$data,
#                   title = "omega = 0.8",
#                   xrange = c(15, 30),
#                   yrange = c(10, 25))
#p4

#cairo_pdf(file.path("figs", "ch14_cournot_sim2.pdf"),
#          width = 6, height = 6)
#print((p1 | p2) / (p3 | p4))
#dev.off()


g14_1_omega_01$plot_mean
#g14_1_omega_2$plot_mean
#g14_1_omega_3$plot_mean
g14_1_omega_08$plot_mean

cairo_pdf(file.path("figs", "ch14_cournot_sim3a.pdf"),
          width = 6, height = 3)
print(g14_1_omega_01$plot_mean)
dev.off()
cairo_pdf(file.path("figs", "ch14_cournot_sim3b.pdf"),
          width = 6, height = 3)
print(g14_1_omega_08$plot_mean)
dev.off()


## Softmax
get_prob <- function(s1,
                     s2_prev,
                     lambda = 0,
                     s1_lb = 0,
                     s1_ub = 60) {

  get_pi <- function(s) {
    (60 - s - s2_prev) * s
  }
  
  pi_1 <- sapply(s1, get_pi)

  enum <- exp(lambda * pi_1)
  denom <- sum(enum)  
  
  enum / denom
}


s1 <- seq(from = 0, to = 60, by = 1)
p1_lambda1 <- get_prob(s1,
                       s2_prev = 12, 
                       lambda = 0.01)

plt_lam_1a <- ggplot(tibble(x = s1, y = p1_lambda1),
       aes(x , y)) + 
  geom_vline(xintercept = 24,
             linetype = "dashed") +
  geom_bar(stat = "identity",
           width = 0.5) +
  scale_x_continuous(breaks = seq(0, 60, by = 12)) +
  labs(x = expression(x[1]),
       y = "選択確率",
       subtitle = expression(paste(lambda, "= 0.01"))) +
  ylim(0, 0.4)
plt_lam_1a

p1_lambda2 <- get_prob(s1,
                       s2_prev = 12, 
                       lambda = 0.4)

plt_lam_1b <- ggplot(tibble(x = s1, y = p1_lambda2),
                     aes(x , y)) + 
  geom_vline(xintercept = 24,
             linetype = "dashed") +
  geom_bar(stat = "identity",
           width = 0.5) +
  scale_x_continuous(breaks = seq(0, 60, by = 12)) +
  labs(x = expression(x[1]),
       y = "選択確率",
       subtitle = expression(paste(lambda, "= 0.4"))) +
  ylim(0, 0.4)
plt_lam_1b

p2_lambda1 <- get_prob(s1,
                       s2_prev = 28, 
                       lambda = 0.01)

plt_lam_2a <- ggplot(tibble(x = s1, y = p2_lambda1),
                     aes(x , y)) + 
  geom_vline(xintercept = 16,
             linetype = "dashed") +
  geom_bar(stat = "identity",
           width = 0.5) +
  scale_x_continuous(breaks = seq(0, 56, by = 8)) +
  labs(x = expression(x[2]),
       y = "選択確率",
       subtitle = expression(paste(lambda, "= 0.01"))) +
  ylim(0, 0.4)
plt_lam_2a

p2_lambda2 <- get_prob(s1,
                       s2_prev = 28, 
                       lambda = 0.4)

plt_lam_2b <- ggplot(tibble(x = s1, y = p2_lambda2),
                     aes(x , y)) + 
  geom_vline(xintercept = 16,
             linetype = "dashed") +
  geom_bar(stat = "identity",
           width = 0.5) +
  scale_x_continuous(breaks = seq(0, 56, by = 8)) +
  labs(x = expression(x[2]),
       y = "選択確率",
       subtitle = expression(paste(lambda, "= 0.4"))) +
  ylim(0, 0.4)
plt_lam_2b

cairo_pdf(file.path("figs", "ch14_softmax.pdf"),
          width = 6, height = 6)
print((plt_lam_1a | plt_lam_2a) / (plt_lam_1b | plt_lam_2b))
dev.off()


set.seed(2022-04-01)
g14_1_sbr1 <- sim_game(game14_1,
                       type = "sbr",
                       n_samples = 20,
                       n_periods = 50,
                       lambda = 0.01,
                       omega = 0.00,
                       plot_range_y = "full",
                       )
g14_1_sbr1$plot_mean
cairo_pdf(file.path("figs", "ch14_sbr1.pdf"),
          width = 6, height = 3)
g14_1_sbr1$plot_mean
dev.off()

g14_1_sbr2 <- sim_game(game14_1,
                       type = "sbr",
                       n_samples = 20,
                       n_periods = 50,
                       lambda = 0.4,
                       omega = 0.00,
                       plot_range_y = "full",
                       )
g14_1_sbr2$plot_mean

cairo_pdf(file.path("figs", "ch14_sbr2.pdf"),
          width = 6, height = 3)
g8_2_sbr2$plot_mean
dev.off()


## imitation
g14_1_imi <- sim_game(game14_1,
                      type = "imitation",
                      n_samples = 50,
                      n_periods = 100,
                      eta = 0.05,
                      plot_range_y = "full")
g14_1_imi$plot_mean

cairo_pdf(file.path("figs", "ch14_imitation1.pdf"),
          width = 6, height = 3)
g14_1_imi$plot_mean
dev.off()

pi_1 <- function(x1, x2) {
  (60 - x1 - x2) * x1 - (60 - x1 - x2) * x2
}
pi_2 <- function(x1, x2) {
  (60 - x1 - x2) * x2 - (60 - x1 - x2) * x1
}

game14_2 <- normal_form(
  payoffs1 = pi_1,
  payoffs2 = pi_2,
  par1_lim = c(0, 60),
  par2_lim = c(0, 60), 
  pars = c("x1", "x2"),
)
g14_2_sol <- solve_nfg(game14_2)


## Learning
game14_3 <- normal_form(
  players = c("企業1", "企業2"),
  pars = c("x1", "x2"),
  par1_lim = c(0, 60),
  par2_lim = c(0, 60),
  payoffs1 = "(60 - x1 - x2) * x1",
  payoffs2 = "(60 - x1 - x2) * x2",
  discretize = TRUE,
  discrete_points = c(7, 7)
)


## Reinforcement learning
set.seed(2022-04-06)
g14_3_reinf <- sim_learning(game14_3,
                            n_samples = 100,
                            n_periods = 50,
                            type = "reinforcement",
                            lambda = 0.1,
                            plot_range_y = c(0, 0.55),
                            )
g14_3_reinf$plot_mean
cairo_pdf(file.path("figs", "ch14_reinforcement_sim.pdf"),
          width = 6, height = 3)
print(g14_3_reinf$plot_mean)
dev.off()


## Belief-based learning
g14_3_belief <- sim_learning(game14_3,
                             n_samples = 100,
                             n_periods = 50,
                             type = "belief",
                             lambda = 0.1)
g14_3_belief$plot_mean
cairo_pdf(file.path("figs", "ch14_belief_sim.pdf"),
          width = 6, height = 3)
print(g14_2_belief$plot_mean)
dev.off()


## EWA
g14_3_ewa <- sim_learning(game14_3,
                          n_samples = 100,
                          n_periods = 50,
                          type = "EWA",
                          lambda = 0.1,
                          phi = 0.5,
                          rho = 0.1,
                          delta = 0.7)
g14_3_ewa$plot_mean
cairo_pdf(file.path("figs", "ch14_ewa_sim.pdf"),
          width = 6, height = 3)
print(g14_2_ewa$plot_mean)
dev.off()
