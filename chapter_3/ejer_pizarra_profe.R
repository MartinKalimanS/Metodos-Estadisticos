source("chapter_3/formulas_chapter3.R")

x <- c(412, 365,398,  471,
       438, 389, 421, 502,
       466, 410, 449, 530,
       489, 437, 470, 561,
       515, 458, 497, 588)

time_series_steps(x, start = c(2019, 1), period = 4)
