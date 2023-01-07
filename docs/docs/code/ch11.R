## ch11.R
##
## R codes for Ch. 11 of Kamijo and Yanai's Game Theory Book
##
## 2022-03-26 Yuki Yanai
## 2022-10-10 YY
## 2023-01-05 YY

library(tidyverse)
library(rgamer)
theme_set(theme_gray(base_size = 9,
                     base_family = "IPAexGothic"))

game11_1 <- extensive_form(
  players = list("P", # n1
                 rep("R", 5), # n2 - n6
                 rep(NA, 10) # n7 - n16
  ),
  actions = list(c("(4, 0)", "(3, 1)", "(2, 2)",
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
show_path(game11_1, all = TRUE)

g11_1_sol <- solve_efg(game11_1)
g11_1_sol$trees


cairo_pdf(file = file.path("figs", "ch11_g11_1a.pdf"),
          width = 3.5, height = 4.5)
print(g11_1_sol$trees[[1]])
dev.off()

cairo_pdf(file = file.path("figs", "ch11_g11_1b.pdf"),
          width = 3.5, height = 4.5)
print(g11_1_sol$trees[[2]])
dev.off()


game11_1outside <-  extensive_form(
  players = list("P", # n1
                 rep("R", 5), # n2 - n6
                 rep(NA, 10) # n7 - n16
  ),
  actions = list(c("(4, 0)", "(3, 1)", "(2, 2)",
                   "(1, 3)","(0, 4)"), # n1: P
                 c("Y", "N"), ## n2: R
                 c("Y", "N"), ## n3: R
                 c("Y", "N"), ## n4: R
                 c("Y", "N"), ## n5: R
                 c("Y", "N") ## n6: R
  ),
  payoffs = list(P = c(4, 0, 3, 0, 2, 0, 1, 0, 0, 0),
                 R = c(0, 1.5, 1, 1.5, 2, 1.5, 3, 1.5, 4, 1.5)), 
  direction = "right",
  show_tree = FALSE,
)
show_path(game11_1outside, all = TRUE)

cairo_pdf(file = file.path("figs", "ch11_g11_1outside.pdf"),
          width = 6, height = 4.5)
show_path(game11_1outside, all = TRUE)
dev.off()


game11_2 <- extensive_form(
  players = list("P", # n1
                 rep("R", 2), # n2,  n3
                 c(NA, "R", NA, "R"), # n4 - n7
                 rep(NA, 4) # n8 - n11
  ),
  actions = list(c("(8, 2)", "(5, 5)"), # n1: P
                 c("Y", "N"),  ## n2: R
                 c("Y", "N"),  ## n3: R
                 c("Out", "N"), ### n5: R
                 c("Out", "N")  ### n7: R
  ),
  payoffs = list(P = c(8, 5, -5, 0, -5, 0),
                 R = c(2, 5, -5, 0, -5, 0)), 
  direction = "right", 
  show_tree = FALSE,
)

show_path(game11_2, all = TRUE)

cairo_pdf(file = file.path("figs", "ch11_g11_2.pdf"),
          width = 6, height = 4.5)
show_path(game11_2, all = TRUE)
dev.off()


game11_3 <- extensive_form(
  players = list("P", # n1
                 rep("R", 2), # n2, n3
                 c(NA, "R", NA, "R"), # n4-n7
                 rep("P", 4), # n8-n11
                 rep(NA, 8) # n12-n19
  ),
  actions = list(c("(8, 2)", "(5, 5)"), # n1: P
                 c("Y", "N"),  ## n2: R
                 c("Y", "N"),  ## n3: R
                 c("(1, 4)", "(2.5, 2.5)"),  ### n5: R
                 c("(1, 4)", "(2.5, 2.5)"),  ### n7: R
                 c("Y", "N"),  #### n8: P
                 c("Y", "N"),  #### n9: P
                 c("Y", "N"),  #### n10: P
                 c("Y", "N")   #### n11: P
  ),
  payoffs = list(P = c(8, 5, 1, 0, 2.5, 0, 1, 0, 2.5, 0),
                 R = c(2, 5, 4, 0, 2.5, 0, 4, 0, 2.5, 0)), 
  direction = "right", 
  show_tree = FALSE,
)
show_path(game11_3, all = TRUE)


cairo_pdf(file = file.path("figs", "ch11_g11_3.pdf"),
          width = 6, height = 4.5)
show_path(game11_3, all = TRUE)
dev.off()


game11_3b <- extensive_form(
  players = list("P", # n1
                 rep("R", 2), # n2, n3
                 c(NA, "R", NA, "R"), # n4-n7
                 rep("P", 4), # n8-n11
                 rep(NA, 8) # n12-n19
  ),
  actions = list(c("(8, 2)", "(5, 5)"), # n1: P
                 c("Y", "N"),  ## n2: R
                 c("Y", "N"),  ## n3: R
                 c("(2, 8)", "(5, 5)"),  ### n5: R
                 c("(2, 8)", "(5, 5)"),  ### n7: R
                 c("Y", "N"),  #### n8: P
                 c("Y", "N"),  #### n9: P
                 c("Y", "N"),  #### n10: P
                 c("Y", "N")   #### n11: P
  ),
  payoffs = list(P = c(8, 5, 2, 0, 5, 0, 2, 0, 5, 0),
                 R = c(2, 5, 8, 0, 5, 0, 8, 0, 5, 0)), 
  direction = "right", 
  show_tree = FALSE,
)
show_path(game11_3b, all = TRUE)

cairo_pdf(file = file.path("figs", "ch11_g11_3b.pdf"),
          width = 8, height = 6)
show_path(game11_3b, all = TRUE)
dev.off()



ineq_averse <- function(alpha = 0, beta = 0) {
  g <- extensive_form(
    players = list("P",
                   rep("R", 2),
                   rep(NA, 4)
    ),
    actions = list(c("(8, 2)", "(5, 5)"), # n1: P
                   c("Y", "N"), ## n2: R
                   c("Y", "N") ## n3: R
    ),
    payoffs = list(P = c(8 - beta * (8-2), 0, 5, 0),
                   R = c(2 - alpha * (8-2), 0, 5, 0)),
    direction = "right",
    show_tree = FALSE)
  show_path(g, all = TRUE)
}

ineq_averse(alpha = 0.1)
ineq_averse(alpha = 0.4)
ineq_averse(beta = 0.6)

cairo_pdf(file.path("figs", "ch11_ineqaverse_1.pdf"),
          width = 6, height = 4)
ineq_averse(alpha = 0.1)
dev.off()

cairo_pdf(file.path("figs", "ch11_ineqaverse_2.pdf"),
          width = 6, height = 4)
ineq_averse(alpha = 0.4)
dev.off()

cairo_pdf(file.path("figs", "ch11_ineqaverse_3.pdf"),
          width = 6, height = 4)
ineq_averse(beta = 0.6)
dev.off()

