source("chapter_1/formulas_chapter1.R")
source("chapter_2/formulas_chapter2.R")

D <- c(43, 55, 40, 52, 39, 33, 50, 33, 44, 21)
V <- c(27.0, 23.8, 30.7, 24.0, 34.8, 41.4, 27.0, 40.4, 31.7, 51.2)

plot(D, V, pch = 19, col = "steelblue",
     xlab = "D", ylab = "V", main = "Scatter diagram of V against D")

plot_regression_lines(D, V, main =  " D(veh/km) vs V(km/h)")

PC <- pearson_correlation(D, V)
print(PC)

r2 <- PC^2
print(r2) 
