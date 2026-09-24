data <- c(3, 4, 8, 10)
absolute_freq <- c(3, 15, 2, 4)

table <- data.frame(data, absolute_freq)
print(table)

N <- sum(table$absolute_freq)
print(N)

m <- sum(table$data * table$absolute_freq) / N
print(m)

