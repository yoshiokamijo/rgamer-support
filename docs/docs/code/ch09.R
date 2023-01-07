## ch09.R
##
## R codes for Ch. 9 of Kamijo and Yanai's Game Theory Book
##
## 2022-03-23 Yuki Yanai
## 2022-04-07 YY
## 2022-05-02 YY

library(rgamer)
theme_set(theme_gray(base_size = 9,
                     base_family = "IPAexGothic"))


# 鹿狩りゲーム：展開型
game9_1 <- extensive_form(
  players = list("P1", # n1
                 c("P2", "P2"),      # n2, n3
                 c(NA, NA, NA, NA)), # n4 - n7
  actions = list(c("鹿", "兎"),      # n1 でとれる行動
                c("鹿", "兎"),       # n2 でとれる行動
                c("鹿", "兎")),      # n3 でとれる行動
  payoffs = list(P1 = c(3, 0, 1, 1), # P1 の n4 - n7 の利得
                 P2 = c(3, 1, 0, 1)),# P2 の n4 - n7 の利得
  family = "IPAexGothic",
)

cairo_pdf(file = file.path("figs", "ch09_staghunt.pdf"),
          width = 5, height = 4)
plot(game9_1$tree)
dev.off()


g9_1_sol <- solve_efg(game9_1)
g9_1_sol$trees

cairo_pdf(file = file.path("figs", "ch09_staghunt_backward.pdf"),
          width = 5, height = 4)
plot(g9_1_sol$trees[[1]])
dev.off()

g9_1_sol$sols


## column
game9_1_right <- extensive_form(
  players = list("P1", # n1
                 c("P2", "P2"),      # n2, n3
                 c(NA, NA, NA, NA)), # n4 - n7
  actions = list(c("鹿", "兎"),       # n1 でとれる行動
                 c("鹿", "兎"),       # n2 でとれる行動
                 c("鹿", "兎")),      # n3 でとれる行動
  payoffs = list(P1 = c(3, 0, 1, 1), # P1 の n4 - n7 の利得
                 P2 = c(3, 1, 0, 1)),# P2 の n4 - n7 の利得
  show_node_id = FALSE,
  direction = "right",
  color_palette = "Dark2",
  family = "IPAexGothic",
)

g9_1r_sol <- solve_efg(game9_1_right)
g9_1r_sol$trees

g9_1r_sol$trees[[1]] + 
  scale_color_brewer(palette = "Accent",
                     guide = "none")

g9_1r_sol$trees[[1]] + 
  scale_color_viridis_d(guide = "none")

g9_1r_sol$trees[[1]] + 
  theme_minimal()
