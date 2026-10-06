source("chapter_1/formulas_chapter1.R")
source("chapter_2/formulas_chapter2.R")
# 1
#Se anotó el número de goles por partido en 15 partidos de una liga: 
#0, 2, 1, 3, 2, 2, 4, 1, 0, 2, 3, 1, 2, 5, 2. 
#Haz el estudio completo de la variable. 
#¿Hay un valor atípico que influya mucho en la media?

data <- c(0, 2, 1, 3, 2, 2, 4, 1, 0, 2, 3, 1, 2, 5, 2)
describe_data(data)

###---------------------------------------------------------------------------------

#2 
# El tiempo de espera (en minutos) de 80 pacientes en urgencias se agrupó así:
# 0–10 (15 pacientes), 10–20 (30), 20–30 (20), 30–60 (10), 60–120 (5). 
#Calcula la media, la mediana, los cuartiles, la desviación típica y el coeficiente de variación. 
#Interpreta la forma de la distribución.

data <- c(15, 30, 20, 10, 5)
lower <- c(0, 10, 20, 30, 60)
upper <- c(10, 20, 30, 60, 120)
describe_intervals(lower = lower, upper = upper, data = data)

#--------------------------------------------------------------------------------------------------------
x <- c(1, 2, 3, 4, 5, 6)
y <- c(2,	3,	5,	8,	12,	19)
z <- c(3,	4,	5,	4,	2,	2)

pearson_correlation(x, y)
linear_regression(x, y, z)
