source("chapter_1/formulas_chapter1.R")
source("chapter_2/formulas_chapter2.R")
source("chapter_3/formulas_chapter3.R")
x <- c(178.2, 153.2, 185.9, 163.6,
       196.3, 156.9, 197.9, 166.4,
       197.3, 159.7, 202.6, 175.6,
       209.5, 169.5, 202.4, 179.8,
       200.0, 168.6, 216.1, 178.5)

decompose_series(x, period = 4, start = c(1980, 1))
time_series_components(x, period = 4, start = c(1980, 1))

time_series_steps(x, period = 4, start = c(1980, 1))
