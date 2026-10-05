source("chapter_1/formulas_chapter1.R")
source("chapter_2/formulas_chapter2.R")

x <- c(0, 1, 0, -1, 0, 1, 1, 2)
y <- c(0, 1, 1, 1, 3, 2, 2, 0)
z <- c(2, 3, 4, 5, 7, 4, 5, 0)

modelo <- multiple_regression(cbind(x, y), z)
print(modelo$coefficients)   # a = 2.2045, b = -1.0568, c = 1.6591

predicho <- modelo$fitted

print(sse_fit(z, predicho))        # SSE = 0.6477
print(mse_fit(z, predicho))        # MSE = 0.0810
print(r_squared_fit(z, predicho))  # R2  = 0.9794
