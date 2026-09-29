source("chapter_1/formulas_chapter1.R")
source("chapter_2/formulas_chapter2.R")

tab <- matrix(c(
  2,0,0,0,0,0,
  4,4,0,0,0,0,
  1,6,3,1,0,0,
  0,1,4,1,1,0,
  0,0,0,0,1,1), nrow = 5, byrow = TRUE)

print(tab)

x_marks <- class_mark(c(150,160,170,180,190), c(160,170,180,190,200))
y_marks <- class_mark(c(50,60,70,80,90,100), c(60,70,80,90,100,110))
pares <- table_to_pairs(tab, x_marks, y_marks)
print(pares)

covariance(pares$x, pares$y, pares$n)          # Covarianza
regression_y_on_x(pares$x, pares$y, pares$n)   # R_Y/X
regression_x_on_y(pares$x, pares$y, pares$n)   # R_X/Y

plot_regression_lines(pares$x, pares$y, pares$n, main = "Altura vs Peso")


