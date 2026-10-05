source("chapter_1/formulas_chapter1.R")
source("chapter_2/formulas_chapter2.R")

x <- c(5, 5, 5, 5, 10, 10, 10, 10, 15, 15, 15, 15, 20, 20, 20, 20)
y <- c(1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4)
z <- c(28, 30, 48, 74, 29, 50, 57, 42, 20, 24, 31, 47, 9, 18, 22, 31)

model <- multiple_regression(cbind(x, y), z)
print(model$coefficients)
