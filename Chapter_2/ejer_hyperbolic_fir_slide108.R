source("chapter_1/formulas_chapter1.R")
source("chapter_2/formulas_chapter2.R")

x <- c(1,2,3,4,5)
y <- c(1, 0.5, 0.33, 0.25, 0.20)
fit_hyperbolic(x, y)
fit_model("y = 1 / (a + b*x)")
