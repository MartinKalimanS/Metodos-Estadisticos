data <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)
absolut_freqs <- c(2, 9, 14, 20, 18, 15, 9, 6, 4, 2, 1)

table <- data.frame(data, absolut_freqs)
print(table)

N <- sum(table$absolut_freqs)
print(N)

m <- sum(table$data *  table$absolut_freqs) / N
print(m)

## crea la lista con los datos repetidos tantas veces diga su frecuencia absoluta
expanded_table <- rep(table$data, table$absolut_freqs)
print(expanded_table)

## MEDIANA
Me <- quantile(expanded_table, 0.5)
print(Me)

## MODA
Mo <- table$data[which.max(table$absolut_freqs)]
print(Mo)

Q1 <- quantile(expanded_table, 0.25)
print(Q1)

Q3 <- quantile(expanded_table, 0.75)
print(Q3)
