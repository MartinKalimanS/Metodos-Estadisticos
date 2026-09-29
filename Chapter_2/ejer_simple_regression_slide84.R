source("chapter_1/formulas_chapter1.R")
source("chapter_2/formulas_chapter2.R")

# ---- Datos ----
tab <- matrix(c(
  3,3,1,0,0,0,
  3,4,2,0,0,0,
  1,3,2,1,0,0,
  0,1,1,2,3,2), nrow = 4, byrow = TRUE)

y_marks  <- class_mark(c(0,4,6,8), c(4,6,8,12))   # 2 5 7 10
x_values <- 0:5


tab_t <- t(tab)

marginals_x <- marginal_absolute_x(tab_t)   # 7 11 6 3 3 2
marginals_y <- marginal_absolute_y(tab_t)   # 7 9 7 9
N <- sum(marginals_x)                       # 32


# ==== PARTE 1: distribución de Y condicionada a 1 <= X <= 3 ====
sub <- tab_t[2:4, ]    # filas de X = 1, 2, 3

abs_cond <- colSums(sub)
print(abs_cond)    # 4 6 6 4
names(abs_cond) <- c("[0,4]","(4,6]","(6,8]","(8,12]")
rel_cond <- abs_cond / sum(abs_cond)

print(rel_cond)            # 0.2 0.3 0.3 0.2

sk <- fisher_skewness(y_marks, abs_cond)
ku <- kurtosis_coefficient(y_marks, abs_cond)
cat(sk, "->", interpret_skewness(sk), "\n")   # 0 -> Symmetric
cat(ku, "->", interpret_kurtosis(ku), "\n")   # -0.898 -> Platykurtic


# ==== PARTE 2: Regression lines ====
pares <- table_to_pairs(tab_t, x_values, y_marks)
print(pares)

regression_y_on_x(pares$x, pares$y, pares$n)   # y = 3.8385 + 1.3920 x
regression_x_on_y(pares$x, pares$y, pares$n)   # x = -0.5025 + 0.3539 y

plot_regression_lines(pares$x, pares$y, pares$n, main = "X vs Y")


# ==== PARTE 3: correlación y RMSE del modelo Y/X ====
PC <- pearson_correlation(pares$x, pares$y, pares$n)

print(PC)        # 0.701924

modelo   <- regression_y_on_x(pares$x, pares$y, pares$n)
predicho <- predict_linear(modelo$a, modelo$b, pares$x)
rmse     <- sqrt(mse_fit(pares$y, predicho, pares$n))
print(rmse)      # 2.072254


