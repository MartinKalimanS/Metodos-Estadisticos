source("chapter_1/formulas_chapter1.R")
source("chapter_2/formulas_chapter2.R")
source("chapter_3/formulas_chapter3.R")

x <- c(6, 6.2, 5.1, 4.9, 5.2, 5.8, 7)
t <- c(1959, 1960, 1961, 1962, 1963, 1964, 1965)

moving_average(x, k= 3, labels = t)

moving_average(x, k= 4, labels = t)
