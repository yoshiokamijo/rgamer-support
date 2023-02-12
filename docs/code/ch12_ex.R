## ch12_ex.R
## R codes for Ch. 12 of Kamijo and Yanai's Game Theory Book
## 2022-04-22 Yoshio Kamijo

library(rgamer)

### 練習問題 12.1
# game12_1 と同一
g_ex12.1 <- extensive_form(
  players = list("P",
                 rep("R", 2),
                 rep(NA, 4)
  ),
  actions <- list(c("8:2", "5:5"), # n1: k
                  c("Y", "N"), ## n2: y
                  c("Y", "N")  ## n3: y
  ),
  payoffs = list(P = c(8, 0, 5, 0),
                 R = c(2, 0, 5, 0)), 
  direction = "right",
)

# to matrix
g_ex12.1_m <- to_matrix(g_ex12.1)
g_ex12.1_m_sol <- solve_nfg(g_ex12.1_m)

# 1. [(8:2), (Y, Y)] は 8, 2
draw_path(g_ex12.1, 
          actions = list(P = c("8:2"), 
                         R = c("Y", "Y")))

# 2. [(8:2), (Y, N)] は 8, 2
draw_path(g_ex12.1, 
          actions = list(P = c("8:2"), 
                         R = c("Y", "N")))

# 3. [(8:2), (N, Y)] は 0, 0
draw_path(g_ex12.1, 
          actions = list(P = c("8:2"), 
                         R = c("N", "Y")))

# 4. [(8:2), (N, N)] は 0, 0
draw_path(g_ex12.1, 
          actions = list(P = c("8:2"), 
                         R = c("N", "N")))

# 5. [(5:5), (Y, Y)] は 5, 5
draw_path(g_ex12.1, 
          actions = list(P = c("5:5"), 
                         R = c("Y", "Y")))

# [(5:5), (Y, N)] は 0, 0
draw_path(g_ex12.1, 
          actions = list(P = c("5:5"), 
                         R = c("Y", "N")))

# [(5:5), (N, Y)] は 5, 5
draw_path(g_ex12.1, 
          actions = list(P = c("5:5"), 
                         R = c("N", "Y")))

# [(5:5), (N, N)] は 0, 0
draw_path(g_ex12.1, 
          actions = list(P = c("5:5"), 
                         R = c("N", "N")))





### 練習問題 12.2
# game11_1 と同一
g_ex12.2 <- extensive_form(
  players = list("P", # n1
                 rep("R", 5), # n2 - n6
                 rep(NA, 10) # n7 - n16
  ),
  actions <- list(c("(4, 0)", "(3, 1)", "(2, 2)", 
                    "(1, 3)","(0, 4)"), # n1: P
                  c("Y", "N"), ## n2: R
                  c("Y", "N"), ## n3: R
                  c("Y", "N"), ## n4: R
                  c("Y", "N"), ## n5: R
                  c("Y", "N") ## n6: R
  ),
  payoffs = list(P = c(4, 0, 3, 0, 2, 0, 1, 0, 0, 0),
                 R = c(0, 0, 1, 0, 2, 0, 3, 0, 4, 0)), 
  direction = "right", 
  show_tree = FALSE,
)

# to matrix
g_ex12.2_m <- to_matrix(g_ex12.2)
g_ex12.2_m_sol <- solve_nfg(g_ex12.2_m)

# R の戦略は 32 こもあるので、利得表を求めるのはとても大変





### 練習問題 12.3
g_ex12.3 <- extensive_form(
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
  info_sets = list(c(3, 4)), 
)


# to matrix
g_ex12.3_m <- to_matrix(g_ex12.3)
g_ex12.3_m_sol <- solve_nfg(g_ex12.3_m)





### 練習問題 12.4
g_ex12.3_sub <- subgames(g_ex12.3)

# よって、subgame は二つ

g_ex12.3_sol <- solve_efg(g_ex12.3, concept = "spe")

# これは解けない。
# 純粋戦略の NE がないから? 
# warnings を出すべき案件。



### 練習問題 12.5
g_ex12.5 <- extensive_form(
  players = list("P1", # n1
                 c("P2", "P2"), # n2, n3 
                 c(NA, NA, NA, NA)), # n4 - n7
  action = list(c("C", "D"), # n1 でとれる行動
                c("C", "D"), # n2 でとれる行動
                c("C", "D")), # n3 でとれる行動
  payoffs=list(
    P1=c(8,0,9,5), #P1のn4-n7の利得
    P2 = c(8, 9, 0, 5)), # P2 の n4 - n7 の利得 
  info_sets = list(c(2, 3)), 
)

# to matrix
g_ex12.5_m <- to_matrix(g_ex12.5)
g_ex12.5_m_sol <- solve_nfg(g_ex12.5_m)





### 練習問題 12.6

pd1 <- c(8, 0, 9, 5) # P1 の利得を定義。ただし行優先で定義する。
pd2 <- c(8, 9, 0, 5) # P2 の利得を定義。ただし行優先で定義する。

g_ex12.6 <- extensive_form(
  players = list(
    "P1",
    rep("P2", 2),
    rep("P1", 4),
    rep("P2", 8),
    rep(NA, 16)),
  actions = 
    rep(list(c("C", "D")),15),
  payoffs = list(
    P1  = c(pd1 + 8, pd1 + 0, pd1 + 9, pd1 + 5),
    P2  = c(pd2 + 8, pd2 + 9, pd2 + 0, pd2 + 5)
  ),
  info_sets = list(c(2, 3), c(8, 9), c(10, 11), c(12, 13), c(14,15)),
  show_node_id = FALSE,
  direction = "right"
)

# 時間がかかるので注意
g_ex12.6_m <- to_matrix(g_ex12.6) 
g_ex12.6_m_sol <- solve_nfg(g_ex12.6_m)




### 練習問題 12.7
# 時間がかかるので注意
g_ex12.6_sol <- solve_efg(g_ex12.6, concept = "spe") 

# subgame を導出
g_ex12.6_sub <- subgames(g_ex12.6)

# subgame 2 ~ 5 の NE を導出
g_ex12.6_sub2 <- to_matrix(g_ex12.6_sub[[2]])
g_ex12.6_sub2_sol <- solve_nfg(g_ex12.6_sub2) 
# よって NE は (D, D)

g_ex12.6_sub3 <- to_matrix(g_ex12.6_sub[[3]])
g_ex12.6_sub3_sol <- solve_nfg(g_ex12.6_sub3) 
# よって NE は (D, D)

g_ex12.6_sub4 <- to_matrix(g_ex12.6_sub[[4]])
g_ex12.6_sub4_sol <- solve_nfg(g_ex12.6_sub4) 
# よって NE は (D, D)

g_ex12.6_sub5 <- to_matrix(g_ex12.6_sub[[5]])
g_ex12.6_sub5_sol <- solve_nfg(g_ex12.6_sub5) 
# よって NE は (D, D)

# subgame を縮約
g_ex12.6_r <- restrict_action(
  g_ex12.6,
  action = list(n4 = "D",
                n5 = "D",
                n6 = "D",
                n7 = "D",
                n8 = "D",
                n9 = "D",
                n10 = "D",
                n11 = "D",
                n12 = "D",
                n13 = "D",
                n14 = "D",
                n15 = "D")
  )

g_ex12.6_r$tree

# 縮約後のゲームを定義
g_ex12.6_r2 <- extensive_form(
  players = list("P1", # n1
                 c("P2", "P2"), # n2, n3 
                 c(NA, NA, NA, NA)), # n4 - n7
  action = list(c("C", "D"), # n1 でとれる行動
                c("C", "D"), # n2 でとれる行動
                c("C", "D")), # n3 でとれる行動
  payoffs=list(
    P1=c(13, 5, 14, 10), 
    P2 = c(13, 14, 5, 10)),  
  info_sets = list(c(2, 3)),
  direction = "right"
)

g_ex12.6_r2_sol <- solve_efg(g_ex12.6_r2, concept = "spe")


# 以下はエラーになる。今後の検討課題
#g_ex12.6_r_m <- to_matrix(g_ex12.6_r) 
#g_ex12.6_r_m_sol <- solve_nfg(g_ex12.6_r_m) 
#g_ex12.6_r_sol <- solve_efg(g_ex12.6_r, concept = "spe")

### この後の検討課題
# restricted した後のゲームに対して
# to_matrix は可能か
# solve_efg は可能か

