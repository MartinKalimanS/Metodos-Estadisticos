source("chapter_3/formulas_chapter3.R")

i_90_69 <- invert_index(1000, percent = TRUE)
i_69_06 <- chain_index(i_90_69, 75, percent = TRUE)

24000 * i_69_06 / 100

