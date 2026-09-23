data <- c(3, 4, 3,
          1, 0, 1,
          2, 2, 2)

tabla <- matrix(data, nrow = 3, byrow = TRUE)

rownames(tabla) <- c("-2", "0", "2")
colnames(tabla) <- c("-1", "0", "1")

## relative frequencies
relative_freq <- tabla / sum(tabla)

## Marginal
marginal_x <- rowSums(relative_freq)
marginal_y <- colSums(relative_freq)

## outer agarra 2 vectores y multiplica todos sus elementos
expected <- outer(marginal_x, marginal_y)

print("--- INDEPENDENCIA ---")
if(isTRUE(all.equal(relative_freq, expected))) {
  print("Independent")
} else {
  print("Not independent")
}


x_vals <- as.numeric(rownames(relative_freq))
print(x_vals)
y_vals <- as.numeric(colnames(relative_freq))
print(y_vals)

mean_x <- sum(x_vals * marginal_x)
print(mean_x)
mean_y <- sum(y_vals * marginal_y)
print(mean_y)

## CENTRAL MOMENTO OF ORDER FUNCTION
central_moment_of_order <- function(r, s, mean_x, mean_y, f_ij) {
  x_vals <- as.numeric(rownames(f_ij))
  y_vals <- as.numeric(colnames(f_ij))
  
  mu_rs <- 0
  
  for (i in 1:nrow(f_ij)) {
    for (j in 1:ncol(f_ij)) {
      
      desv_x <- (x_vals[i] - mean_x)^r
      desv_y <- (y_vals[j] - mean_y)^s
      frecuencia <- f_ij[i, j]
      
      termino <- desv_x * desv_y * frecuencia
      
      mu_rs <- mu_rs + termino
    }
  }
  
  return(mu_rs)
}

covariance <- central_moment_of_order(1, 1, mean_x, mean_y, relative_freq)

print("--- CORRELATION ---")
print(paste("Covariance:", covariance))

if (round(covariance, 5) == 0) {
  print("Uncorrelated (Cov = 0)")
} else {
  print("Correlated")
}

