data <- c(3, 4, 3,
          1, 0, 1,
          2, 2, 2)

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
  print("They are independent and uncorrelated")
} else {
  print("No es independiente")
}

