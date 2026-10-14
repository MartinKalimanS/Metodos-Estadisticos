source("chapter_3/formulas_chapter3.R")

deflate(1000, 135, percent = TRUE)

inv <- invert_index(135, percent = TRUE)
deflate(1000, inv, percent = TRUE)
