## ch12.R
##
## R codes for Ch. 12 of Kamijo and Yanai's Game Theory Book
##
## 2022-02-08 Yuki Yanai
## 2022-03-27 YY
## 2022-05-03 YY
## 2022-10-10 YY
## 2023-01-05 YY

library(rgamer)
library(patchwork)
theme_set(theme_gray(base_size = 9,
                     base_family = "IPAexGothic"))


game12_1 <- extensive_form(
    players = list("P",
                   rep("R", 2),
                   rep(NA, 4)
    ),
    actions = list(c("8:2", "5:5"), # n1: k
                   c("Y", "N"), ## n2: y
                   c("Y", "N") ## n3: y
    ),
    payoffs = list(P = c(8, 0, 5, 0),
                   R = c(2, 0, 5, 0)),
    direction = "right",
)
g12_1_sol <- solve_efg(game12_1)
g12_1_sol$trees

cairo_pdf(file = "figs/ch12_game12_1_tree.pdf", 
          width = 5, height = 4)
print(s12_1$tree[[1]])
dev.off()


cairo_pdf(file = "figs/ch12_game12_1_tree_b.pdf", 
          width = 5, height = 4)
draw_path(game12_1,
          actions = list(P = c("5:5"), 
                         R = c("N", "Y")))
dev.off()


game12_1m <- to_matrix(game12_1)
sol_12_1m <- solve_nfg(game12_1m)
    

# じゃんけん
game12_2 <- extensive_form(
    players = list(
        "Opp",
        rep("You", 3),
        rep(NA, 9)),
    actions = list(
        c("R", "P", "S"),
        c("R", "P", "S"),
        c("R", "P", "S"),
        c("R", "P", "S")),
    payoffs = list(
        Opp  = c(0, -1, 1, 1, 0, -1, -1, 1, 0),
        You  = c(0, 1, -1, -1, 0, 1, 1, -1, 0)), 
    info_set = list(c(2, 3, 4)), # ここを消せば、情報集合のない パネル a を得る。
)

game12_2a <- extensive_form(
    players = list(
        "Opp",
        rep("You", 3),
        rep(NA, 9)),
    actions = list(
        c("R", "P", "S"),
        c("R", "P", "S"),
        c("R", "P", "S"),
        c("R", "P", "S")),
    payoffs = list(
        Opp  = c(0, -1, 1, 1, 0, -1, -1, 1, 0),
        You  = c(0, 1, -1, -1, 0, 1, 1, -1, 0)), 
#    info_set = list(c(2, 3, 4)), # ここを消せば、情報集合のない パネル a を得る。
)

g12_2a_tree <- game12_2a$tree +
  labs(caption = "(a) 状況 3") +
  theme(plot.caption = element_text(size = 12,
                                    hjust = 0.5))

game12_2b <- extensive_form(
    players = list(
        "Opp",
        rep("You", 3),
        rep(NA, 9)),
    actions = list(
        c("R", "P", "S"),
        c("R", "P", "S"),
        c("R", "P", "S"),
        c("R", "P", "S")),
    payoffs = list(
        Opp  = c(0, -1, 1, 1, 0, -1, -1, 1, 0),
        You  = c(0, 1, -1, -1, 0, 1, 1, -1, 0)), 
    info_set = list(c(2, 3, 4)), # ここを消せば、情報集合のない パネル a を得る。
)


g12_2b_tree <- game12_2b$tree +
  labs(caption = "(b) 状況 1 & 2") +
  theme(plot.caption = element_text(size = 12,
                                    hjust = 0.5))


cairo_pdf(file = "figs/ch12_rps_tree.pdf", 
          width = 8, height = 8)
g12_2a_tree / g12_2b_tree
dev.off()


game12_2m <- to_matrix(game12_2)
sol_12_2m <- solve_nfg(game12_2m)


# ミニ最後通牒ゲーム2
game12_3 <- extensive_form(
    players = list("P",
                   rep("R", 2),
                   rep(NA, 4)
    ),
    actions = list(c("8:2", "5:5"), # n1: k
                   c("Y", "N"), ## n2: y
                   c("Y", "N") ## n3: y
    ),
    payoffs = list(P = c(8, 0, 5, 0),
                   R = c(2, 0, 5, 0)),
    direction = "right",
    info_set = list(c(2, 3)),
)

cairo_pdf(file = "figs/ch12_ultim2_tree.pdf", 
          width = 5, height = 4)
game12_3$tree
dev.off()

solve_efg(game12_3) # error


game12_3m <- to_matrix(game12_3)
g12_3m_sol <- solve_nfg(game12_3m)



g12_2a_sg <- subgames(game12_2a)

cairo_pdf(file.path("figs", "ch12_rps_subgame_n2.pdf"),
          width = 4.5, height = 2.7)
g12_2a_sg[[2]]
dev.off()

# 先手がチョキの場合のみ識別可能
game12_2d <- extensive_form(
  players = list(
    "Opp",
    rep("You", 3),
    rep(NA, 9)),
  actions = list(
    c("R", "P", "S"),
    c("R", "P", "S"),
    c("R", "P", "S"),
    c("R", "P", "S")),
  payoffs = list(
    Opp  = c(0, -1, 1, 1, 0, -1, -1, 1, 0),
    You  = c(0, 1, -1, -1, 0, 1, 1, -1, 0)), 
  info_sets = list(c(2, 3)),
)

cairo_pdf(file = "figs/ch12_rps_tree_2.pdf", 
          width = 6, height = 5)
game12_2d$tree
dev.off()



game12_4 <- extensive_form(
  players = list("P",
                 c("P", NA), 
                 rep("R", 2),
                 rep(NA, 4)
  ),
  actions = list(c("Join", "Not Join"), # n1: k
                 c("8:2", "5:5"), # n3: k
                 c("Y", "N"), ## n4: y
                 c("Y", "N")  ## n5: y
  ),
  payoffs = list(P = c(3, 8, 0, 5, 0),
                 R = c(3, 2, 0, 5, 0)), 
  direction = "right", 
  info_set = list(c(4, 5)),
)

cairo_pdf(file = "figs/ch12_game_12_4.pdf", 
          width = 6, height = 5)
game12_4$tree
dev.off()

g12_4_sg <- subgames(game12_4)
g12_4_sg

g12_4_m <- to_matrix(game12_4)
g12_4_sol <- solve_nfg(g12_4_m)

g12_4_spe <- solve_efg(game12_4, concept = "spe")
g12_4_spe$trees

cairo_pdf(file = "figs/ch12_g12_4_spe.pdf", 
          width = 6, height = 5)
g12_4_spe$trees[[1]]
dev.off()


game14_4a <- extensive_form(
  players = list("P",
                 c(NA, NA)),
  actions = list(c("Join", "Not Join") # n1: k
  ),
  payoffs = list(P = c(8, 3),
                 R = c(2, 3)),
  direction = "right", 
)
game14_4a$tree

cairo_pdf("figs/ch12_restrict_a.pdf",
          width = 6, height = 4)
game14_4a$tree
dev.off()

game12_4b <- restrict_action(
  game12_4,
  action = list(n2 = "8:2", 
                n4 = "Y",
                n5 = "Y")
)
game12_4b$tree

cairo_pdf("figs/ch12_restrict_b.pdf",
          width = 6, height = 4)
game12_4b$tree
dev.off()

g12_4b_m <- to_matrix(game12_4b)
solve_nfg(g12_4b_m)
