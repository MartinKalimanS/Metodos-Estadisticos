# The grades of 50 students in a certain subject are
# 5 − 2 − 4 − 9 − 5 − 7 − 4 − 5 − 6 − 5 − 7 − 7 − 5 − 5 − 2 − 10 − 5 − 6 − 5 − 4 −
# 5 − 8 − 8 − 4 − 0 − 8 − 4 − 8 − 6 − 6 − 3 − 6 − 7 − 6 − 6 − 7 − 6 − 7 − 3 − 5 −
# 6 − 9 − 6 − 1 − 4 − 6 − 3 − 5 − 5 − 6 − 7.
# Construct the table of absolute and relative frequencies (both ordinary and cumulative) and
# create a bar chart to represent the relative frequencies graphically

datos <- c(5, 2, 4, 9, 5, 7, 4, 5, 6, 5, 7, 7, 5, 5, 2, 10, 5, 6, 5, 4, 
           5, 8, 8, 4, 0, 8, 4, 8, 6, 6, 3, 6, 7, 6, 6, 7, 6, 7, 3, 5, 
           6, 9, 6, 1, 4, 6, 3, 5, 5, 6, 7)

len <- length(datos)
print(paste("Cantidad total:", len))

# 1. Contar cuántas veces aparece cada número
conteo <- table(datos)

# 2. Convertir ese conteo en un data frame (esto genera automáticamente 2 columnas)
tabla_frecuencias <- as.data.frame(conteo)

# 3. Asignar los nombres correctos a las dos columnas
colnames(tabla_frecuencias) <- c("Numero", "Frecuencia_Absoluta")
print(tabla_frecuencias)

# 4. calcular las frecuencias relativas
# El operador $ agarra datos de una columna
# en la tabla de frecuencias crear una columna de frecuencias relativas 
# y en el calculo agarras los datos de columna de frecuencia absoluta y dividirla en N 

tabla_frecuencias$Frecuencia_Relativa <- tabla_frecuencias$Frecuencia_Absoluta / len
print(tabla_frecuencias)




