source("chapter_1/formulas_chapter1.R")
source("chapter_2/formulas_chapter2.R")
source("chapter_3/formulas_chapter3.R")

#### QUESTION 1
#The monthly spending (in €) of 100 customers of an online shop has been grouped as follows:

# Spending (€)	(0, 20]	(20, 40]	(40, 60]	(60, 100]	(100, 200]
# Customers	        8	     24	      30	        28	      10

# a) Find the modal interval, the mean, the median and the 90th percentile.
# b) Compute the variance, the standard deviation and the coefficient of variation.
# c) Compute Fisher’s skewness and the kurtosis coefficient, and interpret the shape of the distribution.

lower <- c(0,20,40,60,100)
upper <- c(20, 40, 60, 100, 200)

data <- c(8, 24, 30, 28, 10)

describe_intervals(n = data, lower = lower, upper = upper)


#### QUESTION 2
#Question 2 (10 pts)

#For 60 software projects, X is the number of developers (1, 2 or 3 teams of developers, coded as 1, 2, 3) and Y is the number of critical incidents reported (0 to 3). The joint frequencies are:
  
# X \ Y	0	1	2	3
# 1	8	5	2	0
# 2	6	10	8	3
# 3	1	4	7	6

# a) Obtain the marginal distributions of X and Y, with their means and variances.
# b) Are X and Y independent? Justify your answer.
# c) Obtain the distribution of Y for projects with X = 2, and its mean.
# d) Compute the covariance and the correlation coefficient.
# e) Find the regression line of Y on X and predict Y for X = 2.5. What percentage of the variability of Y does it explain?

data <- matrix(c(8, 5, 2, 0,
                6, 10, 8, 3, 
                1, 4, 7, 6), nrow = 3, byrow = TRUE)

analyze_table(data, x_values = 1:3, y_values = 0:3, given_x = 2, predict_x_at = 2.5)

#### QUESTION 3
#The two regression lines of a bivariate distribution (X, Y) of 25 observations are:
  
#  3x + 5y = 37 and 10x + 9y = 85

# It is known that Var(X) = 9.

# a) Identify which line is the regression of Y on X and which is the regression of X on Y. Justify your answer.
# b) Find the means of X and Y and the correlation coefficient.
# c) Find Var(Y) and Cov(X, Y).
# d) The observation (6, 3) is discovered to be an error and is removed. Recompute the means, variances, covariance, correlation coefficient and the new regression line of Y on X.

analyze_regression_lines("3x + 5y = 37", "10x + 9y = 85", var_x = 9, N = 25 ,  remove = c(6, 3))


#### QUESTION 4
#The number of daily requests Y (in thousands) received by a server during its first 7 weeks is:
  
#  Week x	1	2	3	4	5	6	7
#Requests y	3.1	4.0	5.6	7.9	11.8	17.0	24.9

#a) Fit the model y = a·e^(bx) and the model y = a·x² + b·x + c.
#b) For each model, compute the mean squared error and the coefficient of determination.
#c) Decide which model is more suitable and use it to predict the requests in week 8. Comment on the reliability of that prediction.

x <- 1:7
y <- c(3.1, 4.0, 5.6,	7.9,	11.8,	17.0,	24.9)
fit_model("y = a*e^(b*x)")

