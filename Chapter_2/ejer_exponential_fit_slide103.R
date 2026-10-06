source("chapter_1/formulas_chapter1.R")
source("chapter_2/formulas_chapter2.R")

x <- c(0,1,2,3,6)
y <- c(7, 5, 4, 3.5, 3)

fit_exponential(x, y)

mse_of_model("y = 5.88455021482024 * e^(-0.130719488645568 * x)")
