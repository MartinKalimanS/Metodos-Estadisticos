source("chapter_1/formulas_chapter1.R")
source("chapter_2/formulas_chapter2.R")

x <- c(0, 1, 0, -1, 0, 1, 1, 2)
y <- c(0, 1, 1, 1, 3, 2, 2, 0)

fit_model("y = a*sin(x) + b*cos(x)")
fit_model("y = a*x^2 + b*x + c")

