## ch10_ex.R
## R codes for Ch. 11 of Kamijo and Yanai's Game Theory Book
## 2022-04-22 Yoshio Kamijo

library(rgamer)





### 練習問題 11.1
g_ex11.1 <- extensive_form(
  players = list("P", # n1
                 rep("R", 2), # n2, n3
                 c(NA, "R", NA, "R"), # n4-n7
                 rep("P", 4), # n8-n11
                 rep(NA, 8) # n12-n19
  ),
  actions <- list(c("(8, 2)", "(5, 5)"), # n1: P
                  c("Y", "N"),  ## n2: R
                  c("Y", "N"),  ## n3: R
                  c("(1, 2)", "(1.5, 1.5)"),  ### n5: R
                  c("(1, 2)", "(1.5, 1.5)"),  ### n7: R
                  c("Y", "N"),  #### n8: P
                  c("Y", "N"),  #### n9: P
                  c("Y", "N"),  #### n10: P
                  c("Y", "N")   #### n11: P
  ),
  payoffs = list(P = c(8, 5, 1, 0, 1.5, 0, 1, 0, 1.5, 0),
                 R = c(2, 5, 2, 0, 1.5, 0, 2, 0, 1.5, 0)), 
  direction = "right", 
  show_tree = FALSE,
)

show_path(g_ex11.1, all = TRUE)
# みて分かる通り、n2 での R の選択が無差別になるので、それに伴い BI は２通り存在する




### 練習問題 11.2
ineq_averse <- function(alpha = 0, beta = 0) {
  g <- extensive_form(
    players = list("P",
                   rep("R", 2),
                   rep(NA, 4)
    ),
    actions <- list(c("(8, 2)", "(5, 5)"), # n1: P
                    c("Y", "N"), ## n2: R
                    c("Y", "N") ## n3: R
    ),
    payoffs = list(P = c(8 - beta * (8-2), 0, 5, 0),
                   R = c(2 - alpha * (8-2), 0, 5, 0)),
    direction = "right",
    show_tree = FALSE)
  show_path(g, all = TRUE)
}

# 例えば、以下のような値を指定してみる
ineq_averse(alpha = - 0.2, beta = - 0.2)





### 練習問題 11.3
ineq_averse2 <- function(alpha = 0, beta = 0) {
  g <- extensive_form(
    players = list("P",
                   rep("R", 2),
                   rep(NA, 4)
    ),
    actions <- list(c("(8, 2)", "(5, 5)"), # n1: P
                    c("Y", "N"), ## n2: R
                    c("Y", "N") ## n3: R
    ),
    payoffs = list(P = c(8 - beta * (8-2)^2, 0, 5, 0),
                   R = c(2 - alpha * (8-2)^2, 0, 5, 0)),
    direction = "right",
    show_tree = FALSE)
  show_path(g, all = TRUE)
}


# テキストの実行例と同じ値を指定してみる
ineq_averse2(alpha = 0.1)
ineq_averse2(alpha = 0.4)
ineq_averse2(beta = 0.6)

# 二乗の効果により、不平等から発生する不満が大きくなるため、alpha = 0.1 のケースでも
# 不平等な提案は拒否され、それを見越して平等な提案が選ばれている。

