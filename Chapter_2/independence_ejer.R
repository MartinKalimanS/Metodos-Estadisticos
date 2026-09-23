data <- c(0.03, 0.06, 0.09, 0.06,0.06,
          0.07,0.14, 0.21, 0.14, 0.14)

tabla <- matrix(data, nrow= 2, byrow = TRUE)

rownames(tabla) <- c("0", "1")
colnames(tabla) <- c("1", "3" ,"5", "7", "9")

print(tabla)

normalized_table <- prop.table(tabla, margin = 2)
print(normalized_table)


