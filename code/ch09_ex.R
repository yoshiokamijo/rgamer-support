## ch09_ex.R
## R codes for Ch. 9 of Kamijo and Yanai's Game Theory Book
## 2022-04-20 Yoshio Kamijo

library(rgamer)





### 練習問題 9.1
g_ex9.1 <- extensive_form(
  players = list("P1", # n1
                 c("P2", "P2"), # n2, n3 
                 c(NA, NA, NA, NA)), # n4 - n7
  action = list(c("鹿", "兎"), # n1 でとれる行動
                c("鹿", "兎"), # n2 でとれる行動
                c("鹿", "兎")), # n3 でとれる行動
  payoffs=list(
    P1=c(3,0,1,1), #P1のn4-n7の利得
    P2 = c(3, 1, 0, 1)), # P2 の n4 - n7 の利得 
  family = "IPAexGothic",
  direction = "right",
  )
  
# direction に指定できるのは、
# "right", "up", "down", "bidirectional", "horizontal", "vertical". 
# デフォルトは "down".




### 練習問題 9.2
g_ex9.2 <- extensive_form(
  players = list("P1", # n1
                 c("P2", "P2"), # n2, n3 
                 c(NA, NA, NA, NA)), # n4 - n7
  action = list(c("C", "D"), # n1 でとれる行動
                c("C", "D"), # n2 でとれる行動
                c("C", "D")), # n3 でとれる行動
  payoffs=list(
    P1=c(8,0,9,5), #P1のn4-n7の利得
    P2 = c(8, 9, 0, 5)), # P2 の n4 - n7 の利得 
  family = "IPAexGothic"
)

g_ex9.2_sol <- solve_efg(g_ex9.2)
g_ex9.2_sol$trees

