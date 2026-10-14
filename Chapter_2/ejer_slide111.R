source("chapter_1/formulas_chapter1.R")
source("chapter_2/formulas_chapter2.R")

tab <- matrix(c(2,0,0,
                1,3,2,
                1,3,2,
                2,0,0), nrow = 4, byrow = TRUE)
x <- c(20,30,40,50)
y <- c(0,1,2)

analyze_table(tab, x_values = x, y_values = y)

is_independent(tab)

fit_model("y = a + b*x + c*x^2")
