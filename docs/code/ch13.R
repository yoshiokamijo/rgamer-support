## ch13.R
##
## R codes for Ch. 13 of Kamijo and Yanai's Game Theory Book
##
## 2022-03-28 Yuki Yanai
## 2022-03-29 YY
## 2022-05-03 YY
## 2022-10-10 YY

library(tidyverse)
library(patchwork)
library(rgamer)
theme_set(theme_gray(base_size = 9,
                     base_family = "IPAexGothic"))

game13_1a <- normal_form(
  players = c("A", "B"),
  s1 = 1:3,
  s2 = 1:3,
  cells = list(c(15, 0),  c(15, 10), c(15, 5),
                 c(10, 15), c(10, 0),  c(10, 5),
                 c(5, 15),  c(5, 10),  c(5, 0)),
  byrow = TRUE,
)

g13_1a_sol <- solve_nfg(game13_1a)

game13_1b <- normal_form(
  players = c("A", "B"),
  s1 = 1:3,
  s2 = 1:3,
  cells = list(c(7.5, 7.5),  c(15, 10), c(15, 5),
               c(10, 15),    c(5, 5),   c(10, 5),
               c(5, 15),     c(5, 10),  c(2.5, 2.5)),
  byrow = TRUE,
)

g13_1b_sol <- solve_nfg(game13_1b)


m13_1_male <- matching(
  g1_names = c("M1", "M2"),
  g1_prefs = list(c("W1", "W2", "W3"), 
                  c("W3", "W2", "W1")),
  g2_names = c("W1", "W2", "W3"),
  g2_prefs = list(c("M1", "M2"), 
                  c("M2", "M1"), 
                  c("M1", "M2")),
  algorithm = "Boston",
)
print(m13_1_male)


m13_2_female <- matching(
  g1_names = c("M1", "M2"),
  g1_prefs = list(c("W1", "W2", "W3"), 
                  c("W3", "W2", "W1")),
  g2_names = c("W1", "W2", "W3"),
  g2_prefs = list(c("M1", "M2"), 
                  c("M2", "M1"), 
                  c("M1", "M2")),
  algorithm = "DA",
  switch = TRUE
)
print(m13_2_female)


m13_3_female <- matching(
  g1_names = c("M1", "M2"),
  g1_prefs = list(c("W1", "W2", "W3"), 
                  c("W3", "W2", "W1")),
  g2_names = c("W1", "W2", "W3"),
  g2_prefs = list(c("M1", "M2"), 
                  c("M2", "M1"), 
                  c("M2", "M1")),　#W3 が虚偽申告
  algorithm = "Boston", 
  switch = TRUE,
  verbose = FALSE
)
print(m13_3_female)

m13_4_female <- matching(
  g1_names = c("M1", "M2"),
  g1_prefs = list(c("W1", "W2", "W3"), 
                  c("W3", "W2", "W1")),
  g2_names = c("W1", "W2", "W3"),
  g2_prefs = list(c("M1", "M2"), 
                  c("M1", "M2"), #W2 が虚偽申告
                  c("M1", "M2")),
  algorithm = "DA",
  switch = TRUE,
  verbose = FALSE 
)
print(m13_4_female)


m13_5_female <- matching(
  g1_names = c("M1", "M2"),
  g1_prefs = list(c("W1", "W2", "W3"), 
                  c("W3", "W2", "W1")),
  g2_names = c("W1", "W2", "W3"),
  g2_prefs = list(c("M1", "M2"), 
                  c("M2", "M1"), 
                  c("M2", "M1")), # W3 の虚偽申告
  algorithm = "DA", 
  switch = TRUE,
  verbose = FALSE
)
print(m13_5_female)



## 提案側の虚偽申告のシミュレーション
set.seed(20220311)
n <- 20
num_repeat <- 1000

## Boston
X_B_g1 <- matrix(2 * num_repeat, ncol = 2, nrow = num_repeat)
for (i in 1:num_repeat) {
  
  m_prefs <- lapply(1:n, FUN = function(i) {sample(1:n, n)})
  w_prefs <- lapply(1:n, FUN = function(i) {sample(1:n, n)})
  
  m_true_reports <- m_prefs
  m_false_reports <- m_prefs
  m_false_reports[[1]] <- sample(1:n, n)
    
  m_true <- matching(
    g1_names = 1:n,
    g1_prefs = m_true_reports,
    g2_names = 1:n,
    g2_prefs = w_prefs,
    algorithm = "Boston", 
    verbose = FALSE
  )
  
  m_false <- matching(
    g1_names = 1:n,
    g1_prefs = m_false_reports,
    g2_names = 1:n,
    g2_prefs = w_prefs,
    algorithm = "Boston", 
    verbose = FALSE
  )
  
  X_B_g1[i, 1] <- eval_match(m_true, 
                            name = 1, 
                            group = "proposer", 
                            preference = m_prefs[[1]]) 
  X_B_g1[i, 2] <- eval_match(m_false, 
                             name = 1,
                             group = "proposer", 
                             preference = m_prefs[[1]])
  
}

p1_Boston <- tibble(rank_at_true = X_B_g1[, 1]) %>% 
  ggplot(aes(x = rank_at_true,
             y = stat(count / sum(count)))) + 
  geom_bar() +
  labs(x = "パートナーの選好順位",
       y = "割合") + 
  scale_x_continuous(breaks = seq(1, 19, by = 2),
                     minor_breaks = NULL) +
  ylim(0, 0.7)

p2_Boston <- tibble(rank_at_true = X_B_g1[, 1], 
             rank_at_false = X_B_g1[, 2]) %>% 
  ggplot(aes(x = rank_at_true, y = rank_at_false)) + 
  geom_jitter(alpha = 0.2, height = 0.2, width = 0.2) + 
  scale_x_continuous(breaks = 1:n) + 
  scale_y_continuous(breaks = 1:n) + 
  labs(x = "正直な申告時のパートナー",
       y = "虚偽申告時のパートナー")
  
plot(p1_Boston + p2_Boston)
mean(X_B_g1[, 1])


cairo_pdf(file.path("figs", "ch13_hist_Boston_g1.pdf"),
          width = 3, height = 2)
print(p1_Boston)
dev.off()

cairo_pdf(file.path("figs", "ch13_proposer_Boston.pdf"),
          width = 4, height = 4)
print(p2_Boston)
dev.off()


## DA
X_DA_g1 <- matrix(2 * num_repeat, ncol = 2, nrow = num_repeat)

for (i in 1:num_repeat) {
  
  m_prefs <- lapply(1:n, FUN = function(i){sample(1:n, n)})
  w_prefs <- lapply(1:n, FUN = function(i){sample(1:n, n)})
  
  m_true_reports <- m_prefs
  m_false_reports <- m_prefs
  m_false_reports[[1]] <- sample(1:n, n)
  
  m_true <- matching(
    g1_names = 1:n,
    g1_prefs = m_true_reports,
    g2_names = 1:n,
    g2_prefs = w_prefs,
    algorithm = "DA", 
    verbose = FALSE
  )
  
  m_false <- matching(
    g1_names = 1:n,
    g1_prefs = m_false_reports,
    g2_names = 1:n,
    g2_prefs = w_prefs,
    algorithm = "DA", 
    verbose = FALSE
  )
  
  X_DA_g1[i, 1] <- eval_match(m_true, 
                              name = 1, 
                              group = "proposer",
                              preference = m_prefs[[1]])
  X_DA_g1[i, 2] <- eval_match(m_false, 
                              name = 1, 
                              group = "proposer", 
                              preference = m_prefs[[1]])
  
}


p1_DA <- tibble(rank_at_true = X_DA_g1[, 1]) %>% 
  ggplot(aes(x = rank_at_true,
             y = stat(count / sum(count)))) + 
  geom_bar() +
  labs(x = "パートナーの選好順位",
       y = "割合") + 
  scale_x_continuous(breaks = seq(1, 19, by = 2),
                     minor_breaks = NULL) +
  ylim(0, 0.7)

p2_DA <- tibble(rank_at_true = X_DA_g1[, 1], 
                rank_at_false = X_DA_g1[, 2]) %>% 
  ggplot(aes(x = rank_at_true, y = rank_at_false)) + 
  geom_jitter(alpha = 0.2, height = 0.2, width  = 0.2) + 
  scale_x_continuous(breaks = 1:n) + 
  scale_y_continuous(breaks = 1:n) + 
  labs(x = "正直な申告時のパートナー",
       y = "虚偽申告時のパートナー")


plot(p1_DA + p2_DA)

mean(X_DA_g1[, 1])

cairo_pdf(file.path("figs", "ch13_hist_DA_g1.pdf"),
          width = 3, height = 2)
print(p1_DA)
dev.off()

cairo_pdf(file.path("figs", "ch13_proposer_DA.pdf"),
          width = 4, height = 4)
print(p2_DA)
dev.off()



## 受入側の虚偽申告のシミュレーション
set.seed(20220311)
n <- 20
num_repeat <- 1000

## Boston
X_B_g2 <- matrix(2 * num_repeat, ncol = 2, nrow = num_repeat)

for (i in 1:num_repeat) {
  
  m_prefs <- lapply(1:n, FUN = function(i){sample(1:n, n)})
  w_prefs <- lapply(1:n, FUN = function(i){sample(1:n, n)})
  
  w_true_reports <- w_prefs
  w_false_reports <- w_prefs
  w_false_reports[[1]] <- sample(1:n, n)
  
  w_true <- matching(
    g1_names = 1:n,
    g1_prefs = m_prefs,
    g2_names = 1:n,
    g2_prefs = w_true_reports,
    algorithm = "Boston", 
    verbose = FALSE
  )
  
  w_false <- matching(
    g1_names = 1:n,
    g1_prefs = m_prefs,
    g2_names = 1:n,
    g2_prefs = w_false_reports,
    algorithm = "Boston", 
    verbose = FALSE
  )
  
  X_B_g2[i, 1] <- eval_match(w_true, 
                             name = 1, 
                             group = "proposed", 
                             preference = w_prefs[[1]])
  X_B_g2[i, 2] <- eval_match(w_false, 
                             name = 1, 
                             group = "proposed", 
                             preference = w_prefs[[1]])
}


p3_Boston <- tibble(rank_at_true = X_B_g2[, 1]) %>% 
  ggplot(aes(x = rank_at_true,
             y = stat(count / sum(count)))) + 
  geom_bar() +
  labs(x = "パートナーの選好順位",
       y = "割合") + 
  scale_x_continuous(breaks = seq(1, 19, by = 2),
                     minor_breaks = NULL) +
  ylim(0, 0.2)

p4_Boston <- tibble(rank_at_true = X_B_g2[, 1], 
               rank_at_false = X_B_g2[, 2]) %>% 
  ggplot(aes(x = rank_at_true, y = rank_at_false)) + 
  geom_jitter(alpha = 0.2, height=0.2, width =0.2) + 
  scale_x_continuous(breaks = 1:n) + 
  scale_y_continuous(breaks = 1:n) +
  labs(x = "正直な申告時のパートナー",
       y = "虚偽申告時のパートナー")

plot(p3_Boston + p4_Boston)
mean(X_B_g2[, 1])

cairo_pdf(file.path("figs", "ch13_hist_Boston_g2.pdf"),
          width = 3, height = 2)
print(p3_Boston)
dev.off()

cairo_pdf(file.path("figs", "ch13_proposed_Boston.pdf"),
          width = 4, height = 4)
print(p4_Boston)
dev.off()



## DA
X_DA_g2 <- matrix(2 * num_repeat, ncol = 2, nrow = num_repeat)

for (i in 1:num_repeat) {
  
  m_prefs <- lapply(1:n, FUN = function(i){sample(1:n, n)})
  w_prefs <- lapply(1:n, FUN = function(i){sample(1:n, n)})
  
  w_true_reports <- w_prefs
  w_false_reports <- w_prefs
  w_false_reports[[1]] <- sample(1:n, n)
  
  w_true <- matching(
    g1_names = 1:n,
    g1_prefs = m_prefs,
    g2_names = 1:n,
    g2_prefs = w_true_reports,
    algorithm = "DA", 
    verbose = FALSE
  )
  
  w_false <- matching(
    g1_names = 1:n,
    g1_prefs = m_prefs,
    g2_names = 1:n,
    g2_prefs = w_false_reports,
    algorithm = "DA", 
    verbose = FALSE
  )
  
  X_DA_g2[i, 1] <- eval_match(w_true, 
                              name = 1, 
                              group = "proposed", 
                              preference = w_prefs[[1]])
  X_DA_g2[i, 2] <- eval_match(w_false, 
                              name = 1, 
                              group = "proposed", 
                              preference = w_prefs[[1]])
  
}


p3_DA <- tibble(rank_at_true = X_DA_g2[, 1]) %>% 
  ggplot(aes(x = rank_at_true,
             y = stat(count / sum(count)))) + 
  geom_bar() +
  labs(x = "パートナーの選好順位",
       y = "割合") + 
  scale_x_continuous(breaks = seq(1, 19, by = 2),
                     minor_breaks = NULL) +
  ylim(0, 0.2)

p4_DA <- tibble(rank_at_true = X_DA_g2[, 1], 
                rank_at_false = X_DA_g2[, 2]) %>%
  ggplot(aes(x = rank_at_true, y = rank_at_false)) + 
  geom_jitter(alpha = 0.2, height = 0.2, width = 0.2) + 
  scale_x_continuous(breaks = 1:n) + 
  scale_y_continuous(breaks = 1:n)  +
  labs(x = "正直な申告時のパートナー",
       y = "虚偽申告時のパートナー")

plot(p3_DA + p4_DA)


cairo_pdf(file.path("figs", "ch13_hist_DA_g2.pdf"),
          width = 3, height = 2)
print(p3_DA)
dev.off()

cairo_pdf(file.path("figs", "ch13_proposed_DA.pdf"),
          width = 4, height = 4)
print(p4_DA)
dev.off()

mean(X_DA_g2[, 1])

m13_1_female <- matching(
  g1_names = c("M1", "M2"),
  g1_prefs = list(c("W1", "W2", "W3"), 
                  c("W3", "W2", "W1")),
  g2_names = c("W1", "W2", "W3"),
  g2_prefs = list(c("M1", "M2"), 
                  c("M2", "M1"), 
                  c("M1", "M2")),
  algorithm = "Boston",
  switch = TRUE,
)

m13_1_m_s <- is_stable(m13_1_male)
m13_1_f_s <- is_stable(m13_1_female)

m13_1_m_s$stable
m13_1_f_s$stable
