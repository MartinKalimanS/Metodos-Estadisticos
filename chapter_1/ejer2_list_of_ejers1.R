data <- c(
  5, 2, 4, 9, 5, 7, 4, 5, 6, 5, 7, 7, 5, 5, 2, 10, 5, 6, 5, 4,
  5, 8, 8, 4, 0, 8, 4, 8, 6, 6, 3, 6, 7, 6, 6, 7, 6, 7, 3, 5,
  6, 9, 6, 1, 4, 6, 3, 5, 5, 6, 7)

l <- length(data)
print(l)

count <- table(data)
print(count)

table <- as.data.frame(count)
print(table)

colnames(table) <- c("data", "absolute_freq")
print(table)

table$ralative_freq <- table$absolute_freq / l
print(table)

barplot(table$ralative_freq,
        names.arg = "skyblue",
        main = " Relative frequencies",
        xlab = "data values",
        ylab = "Relative frequency")

