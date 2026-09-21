list <- c(21.8, 40.8, 25.1, 34.6, 39.1, 37.8, 38.4, 36.0, 34.4, 30.1,
          25.1, 37.4, 38.0, 25.1, 37.8, 29.4, 34.3, 27.2, 23.8, 29.9,
          26.8, 36.8, 25.0, 36.1, 24.9, 38.5, 23.8, 36.8, 31.7, 20.6,
          29.8, 24.3, 36.5, 39.0, 40.9, 27.4, 22.2, 28.2, 31.1, 31.5,
          38.9, 24.6, 22.3, 39.1, 25.9, 21.6, 24.1, 29.4, 22.1, 28.9,
          42.4, 38.5, 43.8, 25.9, 26.5, 33.8, 20.8, 37.5, 37.6, 38.4,
          27.4, 28.5, 31.5, 28.5, 25.1, 31.3, 22.3, 25.3, 22.6, 22.3,
          20.4, 31.1, 21.2, 28.7, 24.9, 23.2, 26.4, 20.6, 27.8, 26.9,
          30.2, 20.5, 31.1, 30.0, 26.7, 25.4, 25.1, 30.3, 28.8, 23.6,
          25.3, 26.6, 23.8, 28.5, 27.0, 23.9, 21.9, 31.9, 26.0, 23.5)

l <- length(list)
print(l)

## a) mean
m <- mean(list)

##b) median 
Me <- median(list)

##c) interquartile range
q1 <- quantile(list, 0.25)
q3 <- quantile(list, 0.75)
inter_quantile_range <- q3 - q1
print(inter_quantile_range)

## d) standard deviation
standard_deviation <- sd(list) * sqrt((l-1) / l)

## f)
CV <- standard_deviation / abs(m)
print(CV)

## g) Skewness
central_moment_of_order <- function(r, mu, datos) {
  mu_r <- sum((datos - mu)^r) / length(datos)
  return(mu_r)
}

mu_r <- central_moment_of_order(3, m, list)
print(mu_r)

g_1 <- mu_r / standard_deviation^3
print(g_1)

if(g_1 > 0) {
  print("Positive skew")
} else if(g_1 == 0) {
  print("symmetric")
} else {
  print("negative skew")
}

## g) Kurtosis
g_2 <- (central_moment_of_order(4, m, list) / standard_deviation^4) - 3
print (g_2)

if(g_2 < 0){
  print("Platykurtic")
} else if (g_2 == 0) {
  print("Mesokurtic")
} else {
  print(Leptokurtic)
}
  
  
  
  
  
  
  
  
