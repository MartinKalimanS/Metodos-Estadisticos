source("chapter_1/formulas_chapter1.R")
source("chapter_2/formulas_chapter2.R")

tab <- matrix(c(
  2,0,0,0,0,0,
  4,4,0,0,0,0,
  1,6,3,1,0,0,
  0,1,4,1,1,0,
  0,0,0,0,1,1), nrow = 5, byrow = TRUE)

analyze_table(tab,
              x_limits = c(150,160,170,180,190,200),
              y_limits = c(50,60,70,80,90,100,110),
              x_name = "height", y_name = "weight", given_x = 165)

