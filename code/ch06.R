## ch06.R
##
## R codes for Ch. 6 of Kamijo and Yanai's Game Theory Book
##
## 2021-05-25 Yuki Yanai
## 2022-03-16 YY
## 2022-04-07 YY
## 2022-05-02 YY
## 2022-09-07 YY

library(rgamer)
theme_set(theme_gray(base_family = "IPAexGothic",
                     base_size = 9))


game6_1 <- normal_form(
  players = c("あなた", "バルタン星人"),
  s1 = c("グー", "チョキ", "パー"), 
  s2 = c("グー", "チョキ"),
  payoffs1 = c(1, 0, 2, 2, 1, 0), 
  payoffs2 = c(1, 2, 0, 0, 1, 2),
)

g6_1_sol <- solve_nfg(game6_1)

g6_1_dominated <- dom(game6_1)

game6_2 <- eliminate_strategy(game6_1,
                              player = "あなた",
                              eliminated = "チョキ")

g6_2_sol <- solve_nfg(game6_2)


g6_2_mixed <- solve_nfg(game6_2,
                        mixed = TRUE,
                        show_table = FALSE)

g6_2_mixed$br_plot

#cairo_pdf(file = file.path("figs", "ch06_br_plot1.pdf"),
#          family = "sans", width = 4, height = 3)
#print(g6_2_mixed$br_plot)
#dev.off()


game6_3 <- normal_form(
  players = c("子供１", "子供２"),
  s1 = c("グー", "チョキ", "パー"), 
  s2 = c("グー", "チョキ", "パー"), 
  payoffs1 = c(0, 0, 6, 3, 0, 0, 0, 6, 0), 
  payoffs2 = c(0, 3, 0, 0, 0, 6, 6, 0, 0),
)

g6_3_sol <- solve_nfg(game6_3)

g6_3_mixed <- solve_nfg(game6_3, 
                        mixed = TRUE,
                        show_table = FALSE)


game6_4 <- normal_form(
  players = c("1", "2"),
  s1 = c("鹿", "兎"),
  s2 = c("鹿", "兎"),
  payoffs1 = c(3, 2, 0, 2),
  payoffs2 = c(3, 0, 2, 2),
)

g6_4_mixed <- solve_nfg(game6_4, 
                        mixed = TRUE, 
                        show_table = FALSE)
g6_4_mixed$br_plot

#cairo_pdf(file = file.path("figs", "ch06_br_plot2.pdf"),
#          family = "sans", width = 4, height = 3)
#print(g6_4_mixed$br_plot)
#dev.off()
