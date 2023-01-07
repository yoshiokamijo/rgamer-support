## ch10_ex.R
## R codes for Ch. 10 of Kamijo and Yanai's Game Theory Book
## 2022-04-22 Yoshio Kamijo

library(rgamer)





### 練習問題 10.1
g_ex10.1 <- seq_form(
  players = c("P1", "P2"),
  s1 = c("C", "D"), 
  s2 = c("C", "D"),
  payoffs1 = c(8, 0, 9, 5), 
  payoffs2 = c(8, 9, 0, 5),
  byrow = TRUE
)

g_ex10.1_ef <- seq_extensive(g_ex10.1, family = "IPAexGothic")
g_ex10.1_ef_sol <- solve_efg(g_ex10.1_ef)
g_ex10.1_ef_sol$trees[[1]]

# seq_form を使えるようなゲームであれば、こちらを使用したほうが定義が簡単である。
# 当然、どちらで解いても結果は変わらない。



### 練習問題 10.2
g_ex10.2 <- extensive_form(
  players = list("P1", # n1
                 rep("P2", 2), # n2, n3
                 rep("P3", 4), # n4 - n7
                 rep(NA, 8)
  ),
  actions <- list(c("P", "N"), # n1: P1
                  c("P", "N"), ## n2: P2
                  c("P", "N"), ## n3: P2
                  c("P", "N"), ### n4: P3
                  c("P", "N"), ### n5: P3
                  c("P", "N"), ### n6: P3
                  c("P", "N")  ### n7: P3
  ),
  payoffs = list(P1 = c(2, 2, 2, 2, 1, 1, 1, 1),
                 P2 = c(2, 2, 1, 1, 2, 2, 1, 1),
                 P3 = c(2, 1, 2, 1, 2, 1, 2, 1)), 
  direction = "right",
)

g_ex10.2_sol <- solve_efg(g_ex10.2)
g_ex10.2_sol$trees[[1]]





### 練習問題 10.3
# マッチングペニーなどが後手が有利になる例である。
g_ex10.3 <- seq_form(
  players = c("P1", "P2"),
  s1 = c("Heads", "Tails"), 
  s2 = c("Heads", "Tails"),
  payoffs1 = c(1, 0, 0, 1), 
  payoffs2 = c(0, 1, 1, 0),
)

g_ex10.3_ef <- seq_extensive(g_ex10.3)
g_ex10.3_ef_sol <- solve_efg(g_ex10.3_ef)
g_ex10.3_ef_sol$trees[[1]]
g_ex10.3_ef_sol$trees[[2]]

# 以下のコマンドを使用すると、複数の BI を同時にグラフィカルに表示できる
show_path(g_ex10.3_ef, all = TRUE)


