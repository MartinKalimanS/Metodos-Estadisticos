source("chapter_1/formulas_chapter1.R")
source("chapter_2/formulas_chapter2.R")

x <- c(1,2,3,4,5)
y <- c(1,1,2,4,8)

linear_regression(x, y)
exp_model <- fit_model("y = a*b^x")

predict_model(exp_model, data.frame(x = c(6,10)))

linear_model <- fit_model("y = a + b*x")
predict_model(linear_model, data.frame(x = c(6, 10)))

compare_fits(x, y, predict_at = c(6, 10))

