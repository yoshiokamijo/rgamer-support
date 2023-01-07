## ch01.R
##
## R codes for Ch. 1 of Kamijo and Yanai's Game Theory Book
##
## 2020-10-28 Yuki Yanai
## 2022-02-08 YY
## 2022-03-16 YY
## 2022-03-28 YY
## 2022-04-07 YY
## 2022-04-13 YY
## 2022-05-02 YY
## 2022-09-07 YY

library(rgamer)
library(tidyverse)
theme_set(theme_gray(base_size = 9,
                     base_family = "IPAexGothic"))

## 大数の法則の例示：コイン投げ
set.seed(2020-10-28)
n_flips <- 300
lln_coin <- tibble(
  coin_s1 = rbinom(n_flips, size = 1, prob = 0.5),
  coin_s2 = rbinom(n_flips, size = 1, prob = 0.5),
  coin_s3 = rbinom(n_flips, size = 1, prob = 0.5)
  ) |> 
  pivot_longer(cols = coin_s1:coin_s3,
               names_to = "series",
               names_prefix = "coin_",
               values_to = "result") |> 
  group_by(series) |> 
  summarize(cumsum = cumsum(result),
            .groups = "keep") |> 
  mutate(N = 1:n_flips,
         ratio = cumsum / N)

coin_sim <- ggplot(lln_coin, aes(x = N, y = ratio,
                                 group = series)) + 
  geom_hline(yintercept = 0.5, 
             linetype = "dotted") +
  geom_line() +
  labs(x = "コイン投げの回数", y = "平均値") +
  theme(guide_legend = NULL)
plot(coin_sim)

#cairo_pdf(file = file.path("figs", "ch01_LLN.pdf"), 
#          height = 2.5, width = 4)
#print(coin_sim)
#dev.off()

## game1
game1 <- normal_form(
  players = c("矢内", "上條"),
  s1 = c("表", "裏"),
  s2 = c("表", "裏"),
  payoffs1 = c(1, 0, 0, 1),
  payoffs2 = c(0, 1, 1, 0),
)
g1_sol1 <- solve_nfg(game1)

g1_sol2 <- solve_nfg(game1, mixed = TRUE)
g1_sol2$msNE
g1_sol2$br_plot

# 最適反応の図を保存
#cairo_pdf(file = file.path("figs", "ch01_g1_br.pdf"), 
#          height = 2.5, width = 4)
#print(g1_sol2$br_plot)
#dev.off()

g1_sim <- sim_game(game1,
                   n_samples = 100,
                   n_periods = 50)

g1_sim$plot_mean

#cairo_pdf(file = file.path("figs", "ch01_penny.pdf"),
#          width = 6, height = 2.5)
#print(g1_sim$plot_mean)
#dev.off()
