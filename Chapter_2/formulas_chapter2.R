# =============================================================================
#  CHAPTER 2 - STATISTICAL MODELLING (bivariate data, regression, correlation)
#  One R function for every formula / concept in the Chapter 2 slides.
#  (Department of Applied Mathematics, University of Malaga, 2024-2025)
# =============================================================================
#
#  HOW TO USE THIS FILE
#  ---------------------
#  This file BUILDS ON Chapter 1 (it reuses arithmetic_mean(), variance() and
#  standard_deviation() from there instead of redefining them). Always source
#  Chapter 1 FIRST, then this file:
#
#      source("chapter1_descriptive_stats.R")
#      source("chapter2_statistical_modelling.R")
#      pearson_correlation(c(1,2,3), c(2,4,5))   # 0.9819805
#
#  If you forget to source Chapter 1 first, this file will stop with a clear
#  error message telling you to do so.
#
#  WHAT "BIVARIATE" DATA LOOKS LIKE HERE
#  ---------------------------------------
#  Almost every function in this file works with a variable X and a variable
#  Y observed TOGETHER, in pairs: (x1, y1), (x2, y2), .... You give them as
#  two vectors of the same length:
#      x <- c(1, 2, 3, 4, 5)
#      y <- c(1, 1, 2, 2, 4)
#  Just like in Chapter 1, you can optionally add a third vector `n` with how
#  many times each pair (x_i, y_i) repeats (its absolute frequency), instead
#  of listing every repeated observation:
#      x <- c(1, 2, 3)        # the different values of X that were paired
#      y <- c(1, 1, 2)        # the different values of Y that were paired
#      n <- c(4, 2, 1)        # pair (1,1) appeared 4 times, (2,1) twice, ...
#  If a joint (two-way) frequency table is what you have (rows = categories
#  of X, columns = categories of Y, cells = how many times that pair
#  appeared), use `table_to_pairs()` first to turn it into this (x, y, n)
#  form, which every other function expects.
#
#  GROUPED (CONTINUOUS) VARIABLES
#  --------------------------------
#  If X and/or Y are grouped into intervals, use the class mark (the
#  midpoint of each interval, from Chapter 1's `class_mark()`) as the value
#  that represents that interval, exactly like in Chapter 1.
#
#  INDEX (in the order of the slides)
#  ------------------------------------
#  1. JOINT FREQUENCY TABLES
#       joint_absolute_table, joint_relative_table, table_to_pairs,
#       marginal_absolute_x, marginal_absolute_y,
#       marginal_relative_x, marginal_relative_y,
#       conditional_relative_y_given_x, conditional_relative_x_given_y,
#       is_independent
#  2. CENTRE OF GRAVITY, MOMENTS AND COVARIANCE
#       bivariate_mean, moment_xy, ordinary_moment_xy, central_moment_xy,
#       covariance, covariance_shortcut
#  3. SIMPLE LINEAR REGRESSION AND CORRELATION
#       regression_y_on_x, regression_x_on_y, predict_linear,
#       pearson_correlation, r_squared_from_slopes
#  4. GOODNESS OF FIT (works for ANY model: linear, exponential, ...)
#       sse_fit, mse_fit, r_squared_fit, variance_explained_fit
#  5. NON-LINEAR FITS (turned into a straight line first)
#       fit_exponential, predict_exponential,
#       fit_hyperbolic, predict_hyperbolic,
#       fit_power, predict_power
#  6. MULTIPLE LINEAR REGRESSION
#       multiple_regression, predict_multiple, polynomial_regression
#  7. GRAPHS
#       plot_regression_lines
# =============================================================================

if (!exists("arithmetic_mean") || !exists("variance") || !exists("standard_deviation")) {
  stop("Chapter 2 needs Chapter 1's functions. Run this first:\n",
       '  source("chapter1_descriptive_stats.R")\n',
       "and only then source this Chapter 2 file.")
}

# -----------------------------------------------------------------------------
# print.stat_fit(x, ...)     [you never call this yourself]
# -----------------------------------------------------------------------------
# WHAT IT DOES : Every fitting function in this file (regression_y_on_x(),
#                regression_x_on_y(), fit_exponential(), fit_hyperbolic(),
#                fit_power()) returns a small list tagged as a "stat_fit".
#                This teaches R how to print that list in a readable way —
#                instead of just dumping $a and $b with no context, it shows
#                what was fitted and the equation with the numbers filled
#                in. It runs AUTOMATICALLY whenever you print one of those
#                results (by typing the model's name, or not assigning the
#                function's result to anything) — you never call it by hand.
# EXAMPLE      :
#   regression_y_on_x(c(1,2,3,4,5), c(1,1,2,2,4))
#   # Regression line R_Y/X (predicts Y from X)
#   #   y = 0.4000 + 0.7000 * x
#   #   intercept (a) = 0.4000
#   #   slope     (b) = 0.7000
# -----------------------------------------------------------------------------
print.stat_fit <- function(x, ...) {
  labels <- if (!is.null(x$labels)) x$labels else c("a", "b")
  cat(x$description, "\n")
  cat("  ", x$equation, "\n\n", sep = "")
  cat(sprintf("  %-14s= %.4f\n", labels[1], x$a))
  cat(sprintf("  %-14s= %.4f\n", labels[2], x$b))
  invisible(x)
}


# =============================================================================
# 1. JOINT (TWO-WAY) FREQUENCY TABLES
# =============================================================================

# -----------------------------------------------------------------------------
# joint_absolute_table(x, y)
# -----------------------------------------------------------------------------
# WHAT IT DOES : Builds the two-way table of absolute frequencies from two
#                RAW paired observation vectors: how many times each pair
#                of values occurred together. Rows = categories of X,
#                columns = categories of Y.
# PARAMETERS   :
#   x : vector with the X value of every observation.
#   y : vector with the Y value of every observation (same length as x;
#       x[i] and y[i] are observed TOGETHER on the same individual).
# RETURNS      : A matrix of counts, with row names = the values of X and
#                column names = the values of Y.
# EXAMPLE      :  (checks C and errors E on 20 software builds)
#   C <- c(0,1,1,0,1,0,1,1,1,1,1,0,1,0,1,1,2,0,1,2)
#   E <- c(0,1,1,1,1,1,1,1,0,0,1,1,1,0,0,0,0,1,1,0)
#   joint_absolute_table(C, E)
# -----------------------------------------------------------------------------
joint_absolute_table <- function(x, y) {
  as.matrix(table(x, y))
}

# -----------------------------------------------------------------------------
# joint_relative_table(table_abs)
# -----------------------------------------------------------------------------
# WHAT IT DOES : Turns a table of absolute frequencies into a table of
#                relative frequencies (each cell divided by the grand total),
#                so the whole table adds up to 1.
# PARAMETERS   :
#   table_abs : a matrix of absolute frequencies, e.g. from
#               joint_absolute_table() or typed in by hand.
# RETURNS      : A matrix the same shape as `table_abs`, with proportions.
# EXAMPLE      :
#   tab <- matrix(c(2,4,1,0,4,6, 0,0,3,1,0,0), nrow = 3, byrow = TRUE)
#   joint_relative_table(tab)
# -----------------------------------------------------------------------------
joint_relative_table <- function(table_abs) {
  table_abs / sum(table_abs)
}

# -----------------------------------------------------------------------------
# table_to_pairs(table_abs, x_values, y_values)
# -----------------------------------------------------------------------------
# WHAT IT DOES : Converts a two-way frequency table into the (x, y, n) long
#                format that every other function in this file expects: one
#                entry per non-empty cell, with the X value, the Y value,
#                and how many times that pair occurred. You give the actual
#                numeric value each row and each column represents (for
#                grouped/continuous variables, use the class marks).
# PARAMETERS   :
#   table_abs : matrix of absolute frequencies (rows = X, columns = Y).
#   x_values  : numeric vector with the value each ROW represents (its
#               category, or its class mark if X is grouped into intervals).
#               Must have as many entries as `table_abs` has rows.
#   y_values  : numeric vector with the value each COLUMN represents. Must
#               have as many entries as `table_abs` has columns.
# RETURNS      : A list with three vectors of the same length: $x, $y, $n.
# EXAMPLE      :  (height/weight table from the slides)
#   tab <- matrix(c(
#     2,0,0,0,0,0,
#     4,4,0,0,0,0,
#     1,6,3,1,0,0,
#     0,1,4,1,1,0,
#     0,0,0,0,1,1), nrow = 5, byrow = TRUE)
#   height_marks <- class_mark(c(150,160,170,180,190), c(160,170,180,190,200))
#   weight_marks <- class_mark(c(50,60,70,80,90,100), c(60,70,80,90,100,110))
#   pairs <- table_to_pairs(tab, height_marks, weight_marks)
#   bivariate_mean(pairs$x, pairs$y, pairs$n)
# -----------------------------------------------------------------------------
table_to_pairs <- function(table_abs, x_values, y_values) {
  if (nrow(table_abs) != length(x_values))
    stop("`x_values` must have one entry per ROW of `table_abs`.")
  if (ncol(table_abs) != length(y_values))
    stop("`y_values` must have one entry per COLUMN of `table_abs`.")
  list(x = rep(x_values, times = ncol(table_abs)),
       y = rep(y_values, each  = nrow(table_abs)),
       n = as.vector(table_abs))
}

# -----------------------------------------------------------------------------
# marginal_absolute_x(table_abs)
# marginal_absolute_y(table_abs)
# -----------------------------------------------------------------------------
# WHAT THEY DO : The marginal (one-variable) absolute frequency distribution
#                of X (adding up each ROW) or of Y (adding up each COLUMN),
#                ignoring the other variable.
# PARAMETERS   :
#   table_abs : matrix of absolute frequencies (rows = X, columns = Y).
# RETURNS      : Named numeric vector: one total per row (for X) or per
#                column (for Y).
# EXAMPLE      :
#   marginal_absolute_x(tab)   # how many observations at each X value
#   marginal_absolute_y(tab)   # how many observations at each Y value
# -----------------------------------------------------------------------------
marginal_absolute_x <- function(table_abs) {
  rowSums(table_abs)
}
marginal_absolute_y <- function(table_abs) {
  colSums(table_abs)
}

# -----------------------------------------------------------------------------
# marginal_relative_x(table_abs)
# marginal_relative_y(table_abs)
# -----------------------------------------------------------------------------
# WHAT THEY DO : Same as above, but as proportions of the grand total instead
#                of counts.
# PARAMETERS   :
#   table_abs : matrix of absolute frequencies.
# RETURNS      : Named numeric vector of proportions (adds up to 1).
# EXAMPLE      :
#   marginal_relative_x(tab)
# -----------------------------------------------------------------------------
marginal_relative_x <- function(table_abs) {
  marginal_absolute_x(table_abs) / sum(table_abs)
}
marginal_relative_y <- function(table_abs) {
  marginal_absolute_y(table_abs) / sum(table_abs)
}

# -----------------------------------------------------------------------------
# conditional_relative_y_given_x(table_abs, x_index)
# -----------------------------------------------------------------------------
# WHAT IT DOES : The distribution of Y when we only look at the individuals
#                that have a particular value of X (one ROW of the table),
#                as proportions of that row's own total (they add up to 1).
#                This is the distribution written "Y / x_i" in the slides.
# PARAMETERS   :
#   table_abs : matrix of absolute frequencies (rows = X, columns = Y).
#   x_index   : which row to look at: either the row NUMBER (1, 2, 3, ...)
#               or, if the table has row names, the row's name (as text).
# RETURNS      : Named numeric vector of proportions, one per value of Y.
# EXAMPLE      :
#   conditional_relative_y_given_x(tab, 1)     # distribution of Y given X = row 1
# -----------------------------------------------------------------------------
conditional_relative_y_given_x <- function(table_abs, x_index) {
  row <- table_abs[x_index, ]
  row / sum(row)
}

# -----------------------------------------------------------------------------
# conditional_relative_x_given_y(table_abs, y_index)
# -----------------------------------------------------------------------------
# WHAT IT DOES : The distribution of X when we only look at the individuals
#                that have a particular value of Y (one COLUMN of the
#                table), as proportions of that column's own total. This is
#                the distribution written "X / y_j" in the slides.
# PARAMETERS   :
#   table_abs : matrix of absolute frequencies (rows = X, columns = Y).
#   y_index   : which column to look at: either the column NUMBER, or,
#               if the table has column names, the column's name (as text).
# RETURNS      : Named numeric vector of proportions, one per value of X.
# EXAMPLE      :
#   conditional_relative_x_given_y(tab, 2)     # distribution of X given Y = column 2
# -----------------------------------------------------------------------------
conditional_relative_x_given_y <- function(table_abs, y_index) {
  col <- table_abs[, y_index]
  col / sum(col)
}

# -----------------------------------------------------------------------------
# is_independent(table_abs, tolerance = 1e-8)
# -----------------------------------------------------------------------------
# WHAT IT DOES : Checks whether X and Y are INDEPENDENT: this holds exactly
#                when every joint relative frequency equals the product of
#                its two marginal relative frequencies (f_ij = f_i. * f._j
#                for every cell). Because of rounding, you decide how close
#                counts as "equal" with `tolerance`.
# PARAMETERS   :
#   table_abs : matrix of absolute frequencies (rows = X, columns = Y).
#   tolerance : how far apart f_ij and f_i.*f._j may be and still count as
#               equal (default 1e-8, i.e. must match almost exactly).
# RETURNS      : TRUE or FALSE. Also prints the largest discrepancy found,
#                so you can see how close (or far) the table is from
#                independence even when the answer is FALSE.
# EXAMPLE      :
#   tab <- matrix(c(1,3,2,4, 3,9,6,12, 2,6,4,8), nrow = 3, byrow = TRUE)
#   is_independent(tab)   # TRUE: every row is proportional to every other
# -----------------------------------------------------------------------------
is_independent <- function(table_abs, tolerance = 1e-8) {
  N <- sum(table_abs)
  fij      <- table_abs / N
  expected <- outer(marginal_relative_x(table_abs), marginal_relative_y(table_abs))
  max_diff <- max(abs(fij - expected))
  cat("Largest difference between f_ij and (f_i. * f._j):", max_diff, "\n")
  max_diff <= tolerance
}


# =============================================================================
# 2. CENTRE OF GRAVITY, MOMENTS AND COVARIANCE
# =============================================================================

# -----------------------------------------------------------------------------
# bivariate_mean(x, y, n = NULL)
# -----------------------------------------------------------------------------
# WHAT IT DOES : The mean of X and the mean of Y together: the "centre of
#                gravity" (m10, m01) of the bivariate data. Every regression
#                line passes through this point.
# PARAMETERS   :
#   x : the X value of each pair (or the different X values, if using `n`).
#   y : the Y value of each pair (same length as x).
#   n : (optional) how many times each pair (x, y) appears (its frequency).
#       Leave empty for raw, one-row-per-observation data.
# RETURNS      : Named numeric vector c(mean_x = ..., mean_y = ...).
# EXAMPLE      :
#   bivariate_mean(c(1,2,3,4,5), c(1,1,2,2,4))    # mean_x = 3, mean_y = 2
# -----------------------------------------------------------------------------
bivariate_mean <- function(x, y, n = NULL) {
  c(mean_x = arithmetic_mean(x, n), mean_y = arithmetic_mean(y, n))
}

# -----------------------------------------------------------------------------
# moment_xy(x, y, n = NULL, r, s, a = 0, b = 0)
# -----------------------------------------------------------------------------
# WHAT IT DOES : The general bivariate moment of order (r, s) with respect to
#                a reference point (a, b): the average, over all the data, of
#                (X - a) raised to the power r, multiplied by (Y - b) raised
#                to the power s.
# FORMULA (for reference — matches the slides' notation):
#   M_rs(a, b) = sum( (x_i - a)^r * (y_j - b)^s * n_ij ) / N
# PARAMETERS   :
#   x, y : the X and Y value of each pair.
#   n    : (optional) how many times each pair appears.
#   r    : the power applied to (X - a).
#   s    : the power applied to (Y - b).
#   a    : the reference point for X (default 0).
#   b    : the reference point for Y (default 0).
# RETURNS      : One number.
# EXAMPLE      :  (communication-time example: M12 about the point (3, 2))
#   moment_xy(t_values, d_values, n_values, r = 1, s = 2, a = 3, b = 2)
# -----------------------------------------------------------------------------
moment_xy <- function(x, y, n = NULL, r, s, a = 0, b = 0) {
  n <- if (is.null(n)) rep(1, length(x)) else n
  sum(n * (x - a)^r * (y - b)^s) / sum(n)
}

# -----------------------------------------------------------------------------
# ordinary_moment_xy(x, y, n = NULL, r, s)
# -----------------------------------------------------------------------------
# WHAT IT DOES : The bivariate moment of order (r, s) about the point (0, 0).
#                m10 is the mean of X, m01 is the mean of Y, m00 is always 1.
# PARAMETERS   : same as moment_xy(), without `a`/`b` (they are fixed at 0).
# RETURNS      : One number.
# EXAMPLE      :
#   ordinary_moment_xy(c(1,2,3), c(2,4,6), r = 1, s = 1)
# -----------------------------------------------------------------------------
ordinary_moment_xy <- function(x, y, n = NULL, r, s) {
  moment_xy(x, y, n, r, s, a = 0, b = 0)
}

# -----------------------------------------------------------------------------
# central_moment_xy(x, y, n = NULL, r, s)
# -----------------------------------------------------------------------------
# WHAT IT DOES : The bivariate moment of order (r, s) about the point
#                (mean of X, mean of Y). mu_20 is the variance of X, mu_02
#                is the variance of Y, and mu_11 is the covariance of X
#                and Y.
# PARAMETERS   : same as moment_xy(), without `a`/`b` (they are computed
#                automatically as the means of x and y).
# RETURNS      : One number.
# EXAMPLE      :
#   x <- c(1,2,3,4,5); y <- c(1,1,2,2,4)
#   central_moment_xy(x, y, r = 2, s = 0)   # equals variance(x)
#   central_moment_xy(x, y, r = 1, s = 1)   # equals covariance(x, y)
# -----------------------------------------------------------------------------
central_moment_xy <- function(x, y, n = NULL, r, s) {
  mean_x <- arithmetic_mean(x, n)
  mean_y <- arithmetic_mean(y, n)
  moment_xy(x, y, n, r, s, a = mean_x, b = mean_y)
}

# -----------------------------------------------------------------------------
# covariance(x, y, n = NULL)
# -----------------------------------------------------------------------------
# WHAT IT DOES : The covariance of X and Y: it measures whether X and Y tend
#                to move together (positive), move in opposite directions
#                (negative), or show no linear tendency (close to zero).
#                It is the average of (X - mean of X) times (Y - mean of Y).
# FORMULA (for reference — matches the slides' notation):
#   Cov(X, Y) = mu_11 = sum( (x_i - x_bar)(y_j - y_bar) * n_ij ) / N
# PARAMETERS   :
#   x : the X value of each pair.
#   y : the Y value of each pair.
#   n : (optional) how many times each pair appears.
# RETURNS      : One number. Positive = direct relationship, negative =
#                inverse relationship, (close to) zero = uncorrelated.
# EXAMPLE      :
#   covariance(c(1,2,3,4,5), c(1,1,2,2,4))
# -----------------------------------------------------------------------------
covariance <- function(x, y, n = NULL) {
  central_moment_xy(x, y, n, r = 1, s = 1)
}

# -----------------------------------------------------------------------------
# covariance_shortcut(x, y, n = NULL)
# -----------------------------------------------------------------------------
# WHAT IT DOES : The same covariance, computed with the equivalent
#                "shortcut" formula from the slides (mean of the products
#                minus the product of the means). Gives the same result as
#                covariance(); useful to double-check a hand calculation.
# FORMULA (for reference — matches the slides' notation):
#   Cov(X, Y) = mean(X * Y) - mean(X) * mean(Y)
# PARAMETERS   : same as covariance().
# RETURNS      : One number (same result as covariance()).
# EXAMPLE      :
#   covariance_shortcut(c(1,2,3,4,5), c(1,1,2,2,4))
# -----------------------------------------------------------------------------
covariance_shortcut <- function(x, y, n = NULL) {
  ordinary_moment_xy(x, y, n, r = 1, s = 1) - arithmetic_mean(x, n) * arithmetic_mean(y, n)
}


# =============================================================================
# 3. SIMPLE LINEAR REGRESSION AND CORRELATION
# =============================================================================

# -----------------------------------------------------------------------------
# regression_y_on_x(x, y, n = NULL)
# -----------------------------------------------------------------------------
# WHAT IT DOES : Fits the straight line that best predicts Y FROM X, in the
#                least-squares sense (this line is written R_Y/X in the
#                slides): y = a + b * x. Use this when Y is the variable you
#                want to predict/explain and X is the predictor.
# FORMULA (for reference — matches the slides' notation):
#   b = Cov(X, Y) / Var(X)          a = mean(Y) - b * mean(X)
# PARAMETERS   :
#   x : the X value of each pair (the predictor).
#   y : the Y value of each pair (the variable to predict).
#   n : (optional) how many times each pair appears.
# RETURNS      : A list you can print directly to see a readable summary
#                (it prints as "Regression line R_Y/X ... y = a + b*x" with
#                a and b filled in). It has:
#                  $a         — the intercept.
#                  $b         — the slope: how much Y changes per one-unit
#                               increase in X. This is usually the number
#                               that matters for interpretation.
#                  $intercept — the same value as $a, under a clearer name.
#                  $slope     — the same value as $b, under a clearer name.
#                  $equation  — the fitted line as readable text, e.g.
#                               "y = -107.8202 + 1.0161 * x".
#                The line always passes through (mean of X, mean of Y).
# EXAMPLE      :
#   model <- regression_y_on_x(c(1,2,3,4,5), c(1,1,2,2,4))
#   model                      # prints a readable summary
#   model$slope                # 0.9 (just the number you usually want)
#   predict_linear(model$a, model$b, x_new = 6)
# -----------------------------------------------------------------------------
regression_y_on_x <- function(x, y, n = NULL) {
  b <- covariance(x, y, n) / variance(x, n)
  a <- arithmetic_mean(y, n) - b * arithmetic_mean(x, n)
  structure(list(
    a = a, b = b, intercept = a, slope = b,
    equation = sprintf("y = %.4f + %.4f * x", a, b),
    description = "Regression line R_Y/X (predicts Y from X)",
    labels = c("intercept (a)", "slope     (b)")
  ), class = "stat_fit")
}

# -----------------------------------------------------------------------------
# regression_x_on_y(x, y, n = NULL)
# -----------------------------------------------------------------------------
# WHAT IT DOES : Fits the straight line that best predicts X FROM Y, in the
#                least-squares sense (this line is written R_X/Y in the
#                slides): x = a + b * y. This is generally a DIFFERENT line
#                from regression_y_on_x() — solving that one for x does not
#                give you this one.
# FORMULA (for reference — matches the slides' notation):
#   b = Cov(X, Y) / Var(Y)          a = mean(X) - b * mean(Y)
# PARAMETERS   :
#   x : the X value of each pair (the variable to predict here).
#   y : the Y value of each pair (the predictor here).
#   n : (optional) how many times each pair appears.
# RETURNS      : A list you can print directly to see a readable summary
#                (it prints as "Regression line R_X/Y ... x = a + b*y" with
#                a and b filled in). It has:
#                  $a         — the intercept.
#                  $b         — the slope: how much X changes per one-unit
#                               increase in Y.
#                  $intercept — the same value as $a, under a clearer name.
#                  $slope     — the same value as $b, under a clearer name.
#                  $equation  — the fitted line as readable text, e.g.
#                               "x = 130.3504 + 0.6361 * y".
#                The line also passes through (mean of X, mean of Y).
# EXAMPLE      :
#   model <- regression_x_on_y(c(1,2,3,4,5), c(1,1,2,2,4))
#   model                      # prints a readable summary
#   predict_linear(model$a, model$b, x_new = 3)   # predicts X from Y = 3
# -----------------------------------------------------------------------------
regression_x_on_y <- function(x, y, n = NULL) {
  b <- covariance(x, y, n) / variance(y, n)
  a <- arithmetic_mean(x, n) - b * arithmetic_mean(y, n)
  structure(list(
    a = a, b = b, intercept = a, slope = b,
    equation = sprintf("x = %.4f + %.4f * y", a, b),
    description = "Regression line R_X/Y (predicts X from Y)",
    labels = c("intercept (a)", "slope     (b)")
  ), class = "stat_fit")
}

# -----------------------------------------------------------------------------
# predict_linear(a, b, x_new)
# -----------------------------------------------------------------------------
# WHAT IT DOES : Uses a straight-line model (from regression_y_on_x(),
#                regression_x_on_y(), or any a + b*value model) to predict
#                new values.
# FORMULA (for reference — matches the slides' notation):
#   predicted = a + b * x_new
# PARAMETERS   :
#   a     : the intercept of the line.
#   b     : the slope of the line.
#   x_new : the value(s) to plug into the line (one number or a vector).
# RETURNS      : The predicted value(s).
# EXAMPLE      :
#   model <- regression_y_on_x(c(1,2,3,4,5), c(1,1,2,2,4))
#   predict_linear(model$a, model$b, x_new = c(6, 7))
# -----------------------------------------------------------------------------
predict_linear <- function(a, b, x_new) {
  a + b * x_new
}

# -----------------------------------------------------------------------------
# pearson_correlation(x, y, n = NULL)
# -----------------------------------------------------------------------------
# WHAT IT DOES : Pearson's linear correlation coefficient (r): it measures
#                the strength AND direction of the straight-line relationship
#                between X and Y, on a scale that does not depend on units.
#                r = 0 means no linear relationship; r = 1 or r = -1 means a
#                perfect straight-line relationship (direct or inverse).
# FORMULA (for reference — matches the slides' notation):
#   r = Cov(X, Y) / (sd(X) * sd(Y))          (always between -1 and 1)
# PARAMETERS   :
#   x : the X value of each pair.
#   y : the Y value of each pair.
#   n : (optional) how many times each pair appears.
# RETURNS      : One number between -1 and 1.
# EXAMPLE      :
#   pearson_correlation(c(1,2,3,4,5), c(1,1,2,2,4))   # 0.9701425
# -----------------------------------------------------------------------------
pearson_correlation <- function(x, y, n = NULL) {
  covariance(x, y, n) / (standard_deviation(x, n) * standard_deviation(y, n))
}

# -----------------------------------------------------------------------------
# r_squared_from_slopes(slope_y_on_x, slope_x_on_y)
# -----------------------------------------------------------------------------
# WHAT IT DOES : A quick check / shortcut from the slides: the square of the
#                correlation coefficient (r^2) always equals the product of
#                the two regression lines' slopes (b from R_Y/X times b from
#                R_X/Y). Handy to verify your two regressions are consistent
#                with each other, without recomputing r from scratch.
# FORMULA (for reference — matches the slides' notation):
#   r^2 = b * b'
# PARAMETERS   :
#   slope_y_on_x : the slope (b) of the regression of Y on X.
#   slope_x_on_y : the slope (b') of the regression of X on Y.
# RETURNS      : One number: r^2 (should match pearson_correlation(...)^2).
# EXAMPLE      :
#   my <- regression_y_on_x(c(1,2,3,4,5), c(1,1,2,2,4))
#   mx <- regression_x_on_y(c(1,2,3,4,5), c(1,1,2,2,4))
#   r_squared_from_slopes(my$b, mx$b)
# -----------------------------------------------------------------------------
r_squared_from_slopes <- function(slope_y_on_x, slope_x_on_y) {
  slope_y_on_x * slope_x_on_y
}


# =============================================================================
# 4. GOODNESS OF FIT (works for ANY fitted model: linear, exponential, ...)
# =============================================================================

# -----------------------------------------------------------------------------
# sse_fit(actual, predicted, n = NULL)
# -----------------------------------------------------------------------------
# WHAT IT DOES : The Sum of Squared Errors (SSE) of a fitted model: how far
#                the model's predictions are from the real values, added up
#                after squaring each difference (so positive and negative
#                errors don't cancel out). Smaller is better.
# FORMULA (for reference — matches the slides' notation):
#   SSE = sum( (actual_i - predicted_i)^2 * n_i )
# PARAMETERS   :
#   actual    : the real observed values (e.g. the y you measured).
#   predicted : the values your model predicted for the same points.
#   n         : (optional) how many times each (actual, predicted) pair
#               repeats. Leave empty for raw data.
# RETURNS      : One number (0 or more). 0 means a perfect fit.
# EXAMPLE      :
#   sse_fit(actual = c(2,3,4,5), predicted = c(2.2,2.8,4.1,4.9))
# -----------------------------------------------------------------------------
sse_fit <- function(actual, predicted, n = NULL) {
  n <- if (is.null(n)) rep(1, length(actual)) else n
  sum(n * (actual - predicted)^2)
}

# -----------------------------------------------------------------------------
# mse_fit(actual, predicted, n = NULL)
# -----------------------------------------------------------------------------
# WHAT IT DOES : The Mean Squared Error (MSE) of a fitted model: the average
#                of the squared errors (the SSE divided by how many data
#                points there are). Smaller is better; 0 means a perfect fit.
# FORMULA (for reference — matches the slides' notation):
#   MSE = SSE / N
# PARAMETERS   : same as sse_fit().
# RETURNS      : One number (0 or more).
# EXAMPLE      :
#   mse_fit(actual = c(2,3,4,5), predicted = c(2.2,2.8,4.1,4.9))
# -----------------------------------------------------------------------------
mse_fit <- function(actual, predicted, n = NULL) {
  n <- if (is.null(n)) rep(1, length(actual)) else n
  sse_fit(actual, predicted, n) / sum(n)
}

# -----------------------------------------------------------------------------
# r_squared_fit(actual, predicted, n = NULL)
# -----------------------------------------------------------------------------
# WHAT IT DOES : The coefficient of determination (R^2): how much better the
#                model's predictions are than just always predicting the
#                mean. R^2 = 1 is a perfect fit, R^2 = 0 means the model
#                explains nothing (no better than the mean), and R^2 CAN be
#                negative if the model fits worse than a flat horizontal
#                line at the mean.
# FORMULA (for reference — matches the slides' notation):
#   R^2 = 1 - MSE / Var(actual)
# PARAMETERS   :
#   actual    : the real observed values.
#   predicted : the values your model predicted for the same points.
#   n         : (optional) how many times each pair repeats.
# RETURNS      : One number, at most 1 (can be negative for a bad model).
# EXAMPLE      :
#   r_squared_fit(actual = c(2,3,4,5), predicted = c(2.2,2.8,4.1,4.9))
# -----------------------------------------------------------------------------
r_squared_fit <- function(actual, predicted, n = NULL) {
  1 - mse_fit(actual, predicted, n) / variance(actual, n)
}

# -----------------------------------------------------------------------------
# variance_explained_fit(actual, predicted, n = NULL)
# -----------------------------------------------------------------------------
# WHAT IT DOES : The portion of the total variance of the real data that the
#                model "explains" (accounts for). Together with the MSE
#                (the unexplained portion), it adds up to the total
#                variance of the real data.
# FORMULA (for reference — matches the slides' notation):
#   Variance explained = R^2 * Var(actual)
#   Var(actual) = MSE + Variance explained
# PARAMETERS   : same as r_squared_fit().
# RETURNS      : One number. It is NOT itself a variance, so it can be
#                negative when the model fits worse than the mean.
# EXAMPLE      :
#   variance_explained_fit(actual = c(2,3,4,5), predicted = c(2.2,2.8,4.1,4.9))
# -----------------------------------------------------------------------------
variance_explained_fit <- function(actual, predicted, n = NULL) {
  r_squared_fit(actual, predicted, n) * variance(actual, n)
}


# =============================================================================
# 5. NON-LINEAR FITS (turned into a straight line first, then fitted)
# =============================================================================

# -----------------------------------------------------------------------------
# fit_exponential(x, y)
# -----------------------------------------------------------------------------
# WHAT IT DOES : Fits a curve of the form y = a * e^(b*x) to your data. It
#                works by taking the logarithm of y (which turns the curve
#                into a straight line: ln(y) = ln(a) + b*x), fitting a
#                regular straight line to (x, ln(y)), and converting the
#                result back.
# FORMULA (for reference — matches the slides' notation):
#   ln(y) = ln(a) + b*x    (a straight line in x and ln(y))
# PARAMETERS   :
#   x : vector of X values (raw data; every y value must be positive,
#       since the logarithm of a non-positive number does not exist).
#   y : vector of Y values.
# RETURNS      : A list you can print directly for a readable summary
#                ("Exponential fit y = a * e^(b*x) ... y = 5.8846 * e^(-0.1307*x)").
#                It has $a and $b (the parameters), plus $equation (the
#                fitted curve as readable text).
# EXAMPLE      :
#   x <- c(0, 1, 2, 3, 6); y <- c(7, 5, 4, 3.5, 3)
#   model <- fit_exponential(x, y)
#   model                                          # prints a readable summary
#   model$a; model$b                              # a = 5.8846, b = -0.1307
#   predicted <- predict_exponential(model$a, model$b, x)
#   r_squared_fit(y, predicted)                    # 0.81225
# -----------------------------------------------------------------------------
fit_exponential <- function(x, y) {
  Y <- log(y)
  b <- covariance(x, Y) / variance(x)
  A <- arithmetic_mean(Y) - b * arithmetic_mean(x)
  a <- exp(A)
  structure(list(
    a = a, b = b,
    equation = sprintf("y = %.4f * e^(%.4f * x)", a, b),
    description = "Exponential fit y = a * e^(b*x)",
    labels = c("a", "b")
  ), class = "stat_fit")
}

# -----------------------------------------------------------------------------
# predict_exponential(a, b, x_new)
# -----------------------------------------------------------------------------
# WHAT IT DOES : Predicts y values from a fitted exponential model
#                y = a * e^(b*x).
# PARAMETERS   :
#   a, b  : the parameters returned by fit_exponential().
#   x_new : the X value(s) to predict at.
# RETURNS      : The predicted y value(s).
# EXAMPLE      :
#   predict_exponential(5.8846, -0.1307, x_new = 4)
# -----------------------------------------------------------------------------
predict_exponential <- function(a, b, x_new) {
  a * exp(b * x_new)
}

# -----------------------------------------------------------------------------
# fit_hyperbolic(x, y)
# -----------------------------------------------------------------------------
# WHAT IT DOES : Fits a curve of the form y = 1 / (a + b*x) to your data. It
#                works by taking the reciprocal of y (which turns the curve
#                into a straight line: 1/y = a + b*x), fitting a regular
#                straight line to (x, 1/y), and reusing the result directly.
# FORMULA (for reference — matches the slides' notation):
#   1/y = a + b*x    (a straight line in x and 1/y)
# PARAMETERS   :
#   x : vector of X values (raw data).
#   y : vector of Y values (none may be 0, since 1/0 does not exist).
# RETURNS      : A list you can print directly for a readable summary. It
#                has $a and $b (the parameters of y = 1 / (a + b*x)), plus
#                $equation (the fitted curve as readable text).
# EXAMPLE      :
#   x <- c(0, 1, 2, 3, 6); y <- c(7, 5, 4, 3.5, 3)
#   model <- fit_hyperbolic(x, y)
#   model                                          # prints a readable summary
#   model$a; model$b                              # a = 0.169, b = 0.0306
#   # (the course slides print this as "b = -0.0306" and "y = 1/(0.169 -
#   # 0.0306x)", but solving their own normal equations actually gives
#   # b = +0.0306 — plugging that in is what reproduces their MSE and R^2)
#   predicted <- predict_hyperbolic(model$a, model$b, x)
#   r_squared_fit(y, predicted)                    # 0.85724
# -----------------------------------------------------------------------------
fit_hyperbolic <- function(x, y) {
  Y <- 1 / y
  b <- covariance(x, Y) / variance(x)
  a <- arithmetic_mean(Y) - b * arithmetic_mean(x)
  structure(list(
    a = a, b = b,
    equation = sprintf("y = 1 / (%.4f + %.4f * x)", a, b),
    description = "Hyperbolic fit y = 1 / (a + b*x)",
    labels = c("a", "b")
  ), class = "stat_fit")
}

# -----------------------------------------------------------------------------
# predict_hyperbolic(a, b, x_new)
# -----------------------------------------------------------------------------
# WHAT IT DOES : Predicts y values from a fitted hyperbolic model
#                y = 1 / (a + b*x).
# PARAMETERS   :
#   a, b  : the parameters returned by fit_hyperbolic().
#   x_new : the X value(s) to predict at.
# RETURNS      : The predicted y value(s).
# EXAMPLE      :
#   predict_hyperbolic(0.169, -0.0306, x_new = 4)
# -----------------------------------------------------------------------------
predict_hyperbolic <- function(a, b, x_new) {
  1 / (a + b * x_new)
}

# -----------------------------------------------------------------------------
# fit_power(x, y)
# -----------------------------------------------------------------------------
# WHAT IT DOES : Fits a curve of the form y = a * x^b (a power law) to your
#                data. It works the same way as the other two: taking
#                logarithms of BOTH x and y turns the curve into a straight
#                line (ln(y) = ln(a) + b*ln(x)), which is then fitted
#                normally. Not explicitly named in the slides' formula list,
#                but built the same way as fit_exponential() and
#                fit_hyperbolic(), and it shows up in the exercises.
# FORMULA (for reference):
#   ln(y) = ln(a) + b*ln(x)    (a straight line in ln(x) and ln(y))
# PARAMETERS   :
#   x : vector of X values (all strictly positive).
#   y : vector of Y values (all strictly positive).
# RETURNS      : A list you can print directly for a readable summary. It
#                has $a and $b (the parameters of y = a * x^b), plus
#                $equation (the fitted curve as readable text).
# EXAMPLE      :
#   x <- c(1,2,3,4,5); y <- c(0.5,2.0,4.5,8.0,12.5)
#   model <- fit_power(x, y)
#   model                                          # prints a readable summary
#   predicted <- predict_power(model$a, model$b, x)
#   r_squared_fit(y, predicted)
# -----------------------------------------------------------------------------
fit_power <- function(x, y) {
  X <- log(x); Y <- log(y)
  b <- covariance(X, Y) / variance(X)
  A <- arithmetic_mean(Y) - b * arithmetic_mean(X)
  a <- exp(A)
  structure(list(
    a = a, b = b,
    equation = sprintf("y = %.4f * x^%.4f", a, b),
    description = "Power fit y = a * x^b",
    labels = c("a", "b")
  ), class = "stat_fit")
}

# -----------------------------------------------------------------------------
# predict_power(a, b, x_new)
# -----------------------------------------------------------------------------
# WHAT IT DOES : Predicts y values from a fitted power-law model y = a * x^b.
# PARAMETERS   :
#   a, b  : the parameters returned by fit_power().
#   x_new : the X value(s) to predict at (should be positive).
# RETURNS      : The predicted y value(s).
# EXAMPLE      :
#   predict_power(0.5, 2, x_new = 6)
# -----------------------------------------------------------------------------
predict_power <- function(a, b, x_new) {
  a * x_new^b
}


# =============================================================================
# 6. MULTIPLE LINEAR REGRESSION (one response, several predictors)
# =============================================================================

# -----------------------------------------------------------------------------
# multiple_regression(predictors, response)
# -----------------------------------------------------------------------------
# WHAT IT DOES : Fits the best (least-squares) linear model that predicts a
#                response variable from SEVERAL predictor variables at once:
#                response = a0 + a1*predictor_1 + a2*predictor_2 + ...
#                This is the generalisation of regression_y_on_x() to more
#                than one predictor (it includes simple regression and
#                polynomial regression as special cases — see
#                polynomial_regression()).
# FORMULA (for reference — matches the slides' notation):
#   Design matrix M = [1, predictor columns].
#   Coefficients A = (M' M)^(-1) M' response
# PARAMETERS   :
#   predictors : a matrix or data.frame with one COLUMN per predictor
#                variable (do not include a column of 1s; it is added
#                automatically). One row per observation.
#   response   : vector with the value of the variable to predict, one per
#                observation (same number of rows as `predictors`).
# RETURNS      : A list with:
#                  $coefficients — named vector: intercept, then one slope
#                                  per predictor column, in order.
#                  $fitted       — the model's predicted value for every
#                                  observation used to fit it.
#                  $residuals    — response - fitted, for every observation.
# EXAMPLE      :  (fitting a plane z = a + b*x + c*y)
#   x <- c(0,1,0,-1,0,1,1,2); y <- c(0,1,1,1,3,2,2,0); z <- c(2,3,4,5,7,4,5,0)
#   model <- multiple_regression(cbind(x, y), z)
#   model$coefficients          # intercept, x, y
#   sse_fit(z, model$fitted)    # 0.6477
#   r_squared_fit(z, model$fitted)   # 0.9794
# -----------------------------------------------------------------------------
multiple_regression <- function(predictors, response) {
  X <- as.matrix(predictors)
  M <- cbind(Intercept = 1, X)
  A <- solve(t(M) %*% M) %*% t(M) %*% response
  coefs <- as.vector(A)
  names(coefs) <- colnames(M)
  fitted_values <- as.vector(M %*% A)
  list(coefficients = coefs,
       fitted    = fitted_values,
       residuals = response - fitted_values)
}

# -----------------------------------------------------------------------------
# predict_multiple(coefficients, new_predictors)
# -----------------------------------------------------------------------------
# WHAT IT DOES : Predicts the response for new observations from a model
#                fitted with multiple_regression().
# PARAMETERS   :
#   coefficients   : the $coefficients returned by multiple_regression()
#                    (intercept first, then one slope per predictor, in the
#                    same order the predictors were given).
#   new_predictors : a matrix or data.frame with the SAME predictor columns,
#                    in the same order, for the new observations to predict.
# RETURNS      : Vector with the predicted response, one per new observation.
# EXAMPLE      :
#   model <- multiple_regression(cbind(x, y), z)
#   predict_multiple(model$coefficients, cbind(x = 1, y = 1))
# -----------------------------------------------------------------------------
predict_multiple <- function(coefficients, new_predictors) {
  Xnew <- cbind(Intercept = 1, as.matrix(new_predictors))
  as.vector(Xnew %*% coefficients)
}

# -----------------------------------------------------------------------------
# polynomial_regression(x, y, degree)
# -----------------------------------------------------------------------------
# WHAT IT DOES : Fits a polynomial curve (e.g. a parabola for degree = 2:
#                y = a + b*x + c*x^2) to your data, using the least-squares
#                method. This is just multiple_regression() with the
#                predictors being the powers of x (x, x^2, x^3, ...), so it
#                reuses that function internally.
# PARAMETERS   :
#   x      : vector of X values.
#   y      : vector of Y values.
#   degree : the highest power of x to include (2 for a parabola, 3 for a
#            cubic curve, and so on).
# RETURNS      : Same as multiple_regression(): a list with $coefficients
#                (intercept, then the coefficient of x, x^2, ..., x^degree),
#                $fitted and $residuals.
# EXAMPLE      :
#   x <- c(1,2,3,4,5); y <- c(2,3,7,14,23)
#   model <- polynomial_regression(x, y, degree = 2)
#   model$coefficients        # intercept, x, x^2
#   r_squared_fit(y, model$fitted)
# -----------------------------------------------------------------------------
polynomial_regression <- function(x, y, degree) {
  preds <- sapply(1:degree, function(k) x^k)
  colnames(preds) <- paste0("x^", 1:degree)
  multiple_regression(preds, y)
}


# =============================================================================
# 7. GRAPHS
# =============================================================================

# -----------------------------------------------------------------------------
# plot_regression_lines(x, y, n = NULL, show_both = TRUE, main = "Scatter plot and regression line(s)")
# -----------------------------------------------------------------------------
# WHAT IT DOES : Draws the scatter plot of the data (one point per pair, its
#                size proportional to how often that pair repeats) together
#                with the regression line(s): R_Y/X (predicts Y from X, in
#                red) and, if `show_both = TRUE`, also R_X/Y (predicts X
#                from Y, drawn as it appears in the X-Y plane, in green
#                dashed). Both lines always cross at the centre of gravity
#                (mean of X, mean of Y), which the plot also marks.
# PARAMETERS   :
#   x         : the X value of each pair.
#   y         : the Y value of each pair.
#   n         : (optional) how many times each pair appears (its
#               frequency). Leave empty for raw, one-row-per-observation
#               data.
#   show_both : TRUE (default) draws both regression lines (R_Y/X and
#               R_X/Y). FALSE draws only R_Y/X.
#   main      : title of the plot.
# RETURNS      : Invisibly, a list with $R_Y_X and $R_X_Y — the two fitted
#                models (each with $a and $b), in case you want to inspect
#                or reuse them. Mainly used for the plot it draws.
# EXAMPLE      :  (height/weight table from the slides)
#   pares <- table_to_pairs(tab, x_marks, y_marks)
#   plot_regression_lines(pares$x, pares$y, pares$n,
#                          main = "Height vs Weight")
# -----------------------------------------------------------------------------
plot_regression_lines <- function(x, y, n = NULL, show_both = TRUE,
                                  main = "Scatter plot and regression line(s)") {
  n_size <- if (is.null(n)) rep(1, length(x)) else n
  model_yx <- regression_y_on_x(x, y, n)
  
  plot(x, y, cex = sqrt(n_size), pch = 19, col = "steelblue",
       xlab = "X", ylab = "Y", main = main)
  abline(a = model_yx$a, b = model_yx$b, col = "firebrick", lwd = 2)
  
  legend_labels <- c("data (size = frequency)", "R_Y/X")
  legend_colors <- c("steelblue", "firebrick")
  legend_pch    <- c(19, NA)
  legend_lty    <- c(NA, 1)
  
  model_xy <- NULL
  if (show_both) {
    model_xy <- regression_x_on_y(x, y, n)
    # R_X/Y is x = a' + b'*y; rewritten as y = (x - a')/b' to draw it in the X-Y plane
    abline(a = -model_xy$a / model_xy$b, b = 1 / model_xy$b,
           col = "darkgreen", lwd = 2, lty = 2)
    legend_labels <- c(legend_labels, "R_X/Y")
    legend_colors <- c(legend_colors, "darkgreen")
    legend_pch    <- c(legend_pch, NA)
    legend_lty    <- c(legend_lty, 2)
  }
  
  center <- bivariate_mean(x, y, n)
  points(center["mean_x"], center["mean_y"], pch = 4, cex = 1.5, lwd = 2, col = "black")
  
  legend("topleft", legend = legend_labels, col = legend_colors,
         pch = legend_pch, lty = legend_lty, bty = "n")
  
  invisible(list(R_Y_X = model_yx, R_X_Y = model_xy))
}