data <- c(1, 3, 2, 4,
          3, 9, 6, 12,
          2, 6, 4, 8)

tabla <- matrix(data, nrow = 3, byrow = TRUE)

          
rownames(tabla) <- c("x1" , "x2" , "x3")
colnames(tabla) <- c("y1" , "y2" , "y3", "y4")
print(tabla)

## calcular tabla de frecuencias relativas
relative_freq <- tabla / sum(tabla)
print(relative_freq)

## calcular marginales
marginal_x <- rowSums(relative_freq)
marginal_y <- colSums(relative_freq)
print(marginal_x)
print(marginal_y)

## outer agarra 2 vectores y multiplica todos sus elementos
expected <- outer(marginal_x, marginal_y)
print(expected)


if(isTRUE(all.equal(relative_freq, expected))) {
  print("Si es independiente")
} else {
  print("No es independiente")
}

