source("chapter_1/formulas_chapter1.R")
source("chapter_2/formulas_chapter2.R")

x <- c(1,2,3,4,5)
y <- c(1,1,2,4,8)

linear_regression(x, y)
fit_model("y = a*b^x")
