source("chapter_3/formulas_chapter3.R")
x <- c(1.2, 1.4, 2.0, 1.23, 1.37, 2.07, 1.25, 1.47, 2.09)
autocorrelation(x, lag = 3)
autocorrelation(x)
