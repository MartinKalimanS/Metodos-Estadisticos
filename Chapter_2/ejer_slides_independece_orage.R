data <- c(3, 4, 3,
          1, 0, 1,
          2, 2, 2)

tabla <- matrix(data, nrow = 3, byrow = TRUE)

mu_x <- mean(row)

rownames(tabla) <- c("-2", "0", "2")
colnames(tabla) <- c("-1", "0", "1")
print(tabla)

## relative frequencies
relative_freq <- tabla / sum(tabla)
print(relative_freq)

## Marginal
marginal_x <- rowSums(relative_freq)
marginal_y <- colSums(relative_freq)
print(marginal_x)
print(marginal_y)

## outer agarra 2 vectores y multiplica todos sus elementos
expected <- outer(marginal_x, marginal_y)
print(expected)

if(isTRUE(all.equal(relative_freq, expected))) {
  print("Independant")
} else {
  print("Not independent")
}

entral_moment_of_order <- function(r, s, mean_x, mean_y, f_ij) {
  x_vals <- as.numeric(rownames(f_ij))
  y_vals <- as.numeric(colnames(f_ij))
  
  mu_rs <- 0
  
  for (i in 1:nrow(f_ij)) {
    for (j in 1:ncol(f_ij)) {
      
      # Calcular el producto para la celda actual según la fórmula
      desv_x <- (x_vals[i] - mean_x)^r
      desv_y <- (y_vals[j] - mean_y)^s
      frecuencia <- f_ij[i, j]
      
      termino <- desv_x * desv_y * frecuencia
      
      # Acumular el resultado
      mu_rs <- mu_rs + termino
    }
  }
  
  return(mu_rs)
}

central_moment_of_order(1, 1, )

