source("chapter_1/formulas_chapter1.R")
tabla <- matrix(c(
  2, 0, 0, 0, 0, 0,
  4, 4, 0, 0, 0, 0,
  1, 6, 3, 1, 0, 0,
  0, 1, 4, 1, 1, 0,
  0, 0, 0, 0, 1, 1
), nrow = 5, byrow = TRUE,
dimnames = list(
  X = c("150-160", "160-170", "170-180", "180-190", "190-200"),
  Y = c("50-60", "60-70", "70-80", "80-90", "90-100", "100-110")
))

tabla

marginal_x <- rowSums(tabla)
print(marginal_x)
marginal_y <- colSums(tabla)
print(marginal_y)
N <- sum(tabla)
print(N)

class_mark <- function(lower, upper) {
  (lower + upper) / 2
}

class_mark_of_x <- class_mark(c(150, 160, 170, 180, 190), c(160, 170, 180, 190, 200))
print(class_mark_of_x)

class_mark_of_y <- class_mark(c(50, 60, 70, 80, 90), c(60, 70, 80, 90, 100, 110))
print(class_mark_of_y)


arithmetic_mean <- function(x, n = NULL) {
  n <- freq_or_ones(x, n)
  sum(x * n) / sum(n)
}

mean_x <- arithmetic_mean(class_mark_of_x, marginal_x)
print(mean_x)
mean_y <- arithmetic_mean(class_mark_of_y, marginal_y)
print(mean_y)










