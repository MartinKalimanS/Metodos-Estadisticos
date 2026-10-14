source("chapter_1/formulas_chapter1.R")
source("chapter_2/formulas_chapter2.R")
source("chapter_3/formulas_chapter3.R")

x <- c(2.6767, 3.2050, 4.8714, 4.6275, 5.9008, 5.5200, 5.1186,
       4.9507, 4.6612, 6.2572)

moving_average(x, k = 3)
moving_average(x, k = 4)
