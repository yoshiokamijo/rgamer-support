## ch10.R
##
## R codes for Ch. 10 of Kamijo and Yanai's Game Theory Book
##
## 2022-03-23 Yuki Yanai
## 2022-04-07 YY
## 2022-05-02 YY
## 2022-10-10 YY
## 2023-01-05 YY

library(rgamer)
theme_set(theme_gray(base_size = 9,
                     base_family = "IPAexGothic"))

## Number 3
game10_1 <- extensive_form(
  players = list("K",                # n1
                 rep("Y", 2),        # n2, n3
                 c("K", rep(NA, 2)), # n4, n5, n6
                 c(NA)               # n7
  ),
  actions = list(c("1", "2"), # n1: sum=0: K
                 c("1", "2"), ## n2: sum=1: Y
                 c("1"),      ## n3: sum=2: Y
                 c("1")),     ### n4: sum=1+1=2: K
                 # End        ### n5: sum=1+2=3: NA
                 # End        ### n6: sum=2+1=3: NA
                 # End        #### n7: sum=1+1+1=3: NA
  payoffs = list(K = c(0, 0, 3),
                 Y = c(3, 3, 0)),
  direction = "right",
)

g10_1_sol <- solve_efg(game10_1)
g10_1_sol$trees


cairo_pdf(file = file.path("figs", "ch10_number3.pdf"),
          width = 5, height = 4)
plot(g10_1_sol$tree[[1]])
dev.off()

## Number 4
N4 <- extensive_form(
  players = list("K",                # n1
                 rep("Y", 2),        # n2, n3
                 c(rep("K", 3), NA), # n4 - n7
                 c("Y", rep(NA, 3)), # n8 - n 11
                 NA),                # n12
  actions = list(c("1", "2"), # n1: sum=0: K
                 c("1", "2"), ## n2: sum=1: Y
                 c("1", "2"), ## n3: sum=2: Y
                 c("1", "2"), ### n4: sum=1+1=2: K
                 c("1"),      ### n5: sum=1+2=3: K
                 c("1"),      ### n6: sum=2+1=3: K
                 # End        ### n7: sum=2+2=4: NA
                 c("1")),    #### n8: sum=1+1+1=3: Y
                 # End        #### n9: sum=1+1+2=4: NA
                 # End        #### n10: sum=1+2+1=4: NA
                 # End        #### n11: sum=2+1+1=4: NA
                 # End        ##### n12: sum=1+1+1+1=4 NA
  payoffs = list(K = c(0, 4, 4, 4, 0),
                 Y = c(4, 0, 0, 0, 4)),
  direction = "right",
)

N4_sol <- solve_efg(N4)


cairo_pdf(file = file.path("figs", "ch10_number4.pdf"),
          width = 5, height = 4)
plot(N4_sol$tree[[1]])
dev.off()

## Number 5
N5 <- extensive_form(
  players = list("K",                # n1
                 rep("Y", 2),        # n2, n3
                 c(rep("K", 4)),     # n4 - n7
                 c("Y", "Y", "Y", NA, "Y", NA, NA), # n8 - n14
                 c("K", rep(NA, 4)), # n15 - n19
                 NA),               # n20
  actions = list(c("1", "2"), # n1: sum=0: K
                 c("1", "2"), ## n2: sum=1: Y
                 c("1", "2"), ## n3: sum=2: Y
                 c("1", "2"), ### n4: sum=1+1=2: K
                 c("1", "2"), ### n5: sum=1+2=3: K
                 c("1", "2"), ### n6: sum=2+1=3: K
                 c("1"),      ### n7: sum=2+2=4: K
                 c("1", "2"), #### n8: sum=1+1+1=3: Y
                 c("1"),      #### n9: sum=1+1+2=4: Y
                 c("1"),      #### n10: sum=1+2+1=4: Y
                 # End        #### n11: sum=1+2+2=5: NA
                 c("1"),      #### n12: sum=2+1+1=4: Y
                 # End        #### n13: sum=2+1+2=5: NA
                 # End        #### n14: sum=2+2+1=5: NA
                 c("1")),     ##### n15: sum=1+1+1+1=4: K
                 # End        ##### n16: sum=1+1+1+2=5: NA
                 # End        ##### n17: sum=1+1+2+1=5: NA
                 # End        ##### n18: sum=1+2+1+1=5: NA
                 # End        ##### n19: sum=2+1+1+1=5: NA
                 # End        ###### n20: sum=1+1+1+1+1=5: NA
  payoffs = list(K = c(5, 5, 5, 0, 0, 0, 0, 5),
                 Y = c(0, 0, 0, 5, 5, 5, 5, 0)),
  direction = "right",
)

N5_sol <- solve_efg(N5)

cairo_pdf(file = file.path("figs", "ch10_number5.pdf"),
          width = 6, height = 5)
plot(N5_sol$tree[[1]])
dev.off()


game10_2 <- seq_form(
  players = c("P1", "P2"),
  s1 = c("A", "B"),
  s2 = c("a", "b"),
  payoffs1 = c(3, 4, 1, 2),
  payoffs2 = c(3, 2, 2, 4),
)
g10_2_sol <- solve_seq(game10_2)

g10_2ef <- seq_extensive(game10_2)
g10_2ef_sol <- solve_efg(g10_2ef)
g10_2ef_sol$trees[[1]]

cairo_pdf(file = file.path("figs", "ch10_seq_tree.pdf"),
          width = 6, height = 4)
plot(g10_2ef_sol$trees[[1]])
dev.off()


## Stackelberg competition
game10_3 <- seq_form(
  players = c("企業1", "企業2"),
  pars = c("x1", "x2"),
  par1_lim = c(0, 60),
  par2_lim = c(0, 60),
  payoffs1 = "(60 - x1 - x2) * x1",
  payoffs2 = "(60 - x1 - x2) * x2",
)
g10_3_sol <- solve_seq(game10_3)


## 3-player sequential stag-hunt
game10_4 <- extensive_form(
  players = list("P1", # n1
                 rep("P2", 2), # n2, n3
                 rep("P3", 4), # n4 - n7
                 rep(NA, 8)
                 ),
  actions = list(c("P", "N"), # n1: P1
                 c("P", "N"), ## n2: P2
                 c("P", "N"), ## n3: P2
                 c("P", "N"), ### n4: P3
                 c("P", "N"), ### n5: P3
                 c("P", "N"), ### n6: P3
                 c("P", "N")  ### n7: P3
                 ),
  payoffs = list(P1 = c(2, 2, 2, 0, 1, 1, 1, 1),
                 P2 = c(2, 2, 1, 1, 2, 0, 1, 1),
                 P3 = c(2, 1, 2, 1, 2, 1, 0, 1)
                 ), 
  direction = "right", 
)
g10_4_sol <- solve_efg(game10_4)

cairo_pdf(file = file.path("figs", "ch10_3p_staghunt.pdf"),
          width = 7, height = 4)
plot(g10_4_sol$trees[[1]])
dev.off()

game10_4b <- extensive_form(
  players = list("P1", # n1
                 rep("P2", 2), # n2, n3
                 rep("P3", 4), # n4 - n7
                 rep(NA, 8)
  ),
  actions = list(c("P", "N"), # n1: P1
                 c("P", "N"), ## n2: P2
                 c("P", "N"), ## n3: P2
                 c("P", "N"), ### n4: P3
                 c("P", "N"), ### n5: P3
                 c("P", "N"), ### n6: P3
                 c("P", "N")  ### n7: P3
  ),
  payoffs = list(P1 = c(2, 0, 0, 0, 1, 1, 1, 1),
                 P2 = c(2, 0, 1, 1, 0, 0, 1, 1),
                 P3 = c(2, 1, 0, 1, 0, 1, 0, 1)
  ), 
  direction = "right", 
)
g10_4b_sol <- solve_efg(game10_4b)

cairo_pdf(file = file.path("figs", "ch10_3p_staghunt_k3.pdf"),
          width = 7, height = 4)
plot(g10_4b_sol$trees[[1]])
dev.off()


game10_5 <- seq_form(
  players = c("学生１", "学生２"),
  s1 = c("統計学", "歴史学"),
  s2 = c("統計学", "歴史学"),
  payoffs1 = c(3, 0, 1, 2),
  payoffs2 = c(2, 0, 1, 3),
)
g10_5_sol <- solve_seq(game10_5)

g10_5ef <- seq_extensive(game10_5, family = "IPAexGothic")
g10_5ef_sol <- solve_efg(g10_5ef)
g10_5ef_sol$trees[[1]]

cairo_pdf(file = file.path("figs", "ch10_battle_students.pdf"),
          width = 8, height = 5)
plot(g10_5ef_sol$trees[[1]])
dev.off()
