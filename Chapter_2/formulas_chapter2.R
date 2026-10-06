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
#      source("formulas_chapter1.R")
#      source("formulas_chapter2.R")
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
#  QUICK GUIDE: ONE CALL PER EXERCISE TYPE  (read this first!)
#  -----------------------------------------------------------
#   Two lists of numbers x, y                  -> linear_regression(x, y, n)
#   A two-way table (values or intervals)      -> analyze_table(tab, x_limits=, y_limits=)
#   Two regression lines given as equations    -> analyze_regression_lines("..", "..")
#   MSE of an equation that is already given  -> mse_of_model("y = 3 + 0.5*x")
#   Any formula typed as text ("y = a*x^2+b*x+c", "y = a*exp(b*x)",
#   "z = a + b*x + c*y")                       -> fit_model("...")
#  Each prints an English report with all decimals and draws the plot.
#  Text-typed formulas with several data variables use fit_model(); for
#  non-linear forms a*e^(bx), 1/(a+bx), a*x^b it linearises automatically.
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
#       sse_fit, mse_fit, r_squared_fit, variance_explained_fit,
#       mse_of_model   <- MSE of a model given as text, e.g. "y = 3 + 0.5*x"
#  5. NON-LINEAR FITS (turned into a straight line first)
#       fit_exponential, predict_exponential,
#       fit_hyperbolic, predict_hyperbolic,
#       fit_power, predict_power
#  6. MULTIPLE LINEAR REGRESSION
#       multiple_regression, predict_multiple, polynomial_regression,
#       fit_model, predict_model       <- fit ANY model written as text,
#                                         as in the exam, e.g.
#                                         "y = a*x^2 + b*x + c"
#  7. GRAPHS
#       plot_regression_lines
# =============================================================================

if (!exists("describe_data") && file.exists("formulas_chapter1.R")) {
  source("formulas_chapter1.R")      # loads Chapter 1 automatically if it is in the same folder
}
if (!exists("arithmetic_mean") || !exists("variance") || !exists("describe_data")) {
  stop("Chapter 2 needs Chapter 1's functions. Run this first:\n",
       '  source("formulas_chapter1.R")\n',
       "and only then source this Chapter 2 file.")
}

# -----------------------------------------------------------------------------
# FULL PRECISION (no rounding anywhere)
# -----------------------------------------------------------------------------
# No function in this file rounds its result. But R itself only DISPLAYS 7
# significant digits by default, so this line makes R display up to 15 for
# everything you print in this session (the same line as in Chapter 1).
options(digits = 15)

# INTERNAL HELPERS (you do not need to call these yourself): write numbers
# with ALL their digits (up to 15 significant), never rounded to 4 decimals.
# (full_digits() is defined in Chapter 1, which this file requires)
# the same, but with " + " or " - " in front (so we never print "+ -0.8")
signed_full <- function(v) {
  paste0(ifelse(v < 0, " - ", " + "), full_digits(abs(v)))
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
#   #   y = -0.0999999999999996 + 0.7 * x
#   #   intercept (a) = -0.0999999999999996
#   #   slope     (b) = 0.7
#   # (the intercept is -0.1 up to the tiny error of floating-point numbers)
# -----------------------------------------------------------------------------
print.stat_fit <- function(x, ...) {
  labels <- if (!is.null(x$labels)) x$labels else c("a", "b")
  cat(x$description, "\n")
  cat("  ", x$equation, "\n\n", sep = "")
  cat(sprintf("  %-14s= %s\n", labels[1], full_digits(x$a)))
  cat(sprintf("  %-14s= %s\n", labels[2], full_digits(x$b)))
  if (is.function(x$report)) x$report()
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
    equation = paste0("y = ", full_digits(a), signed_full(b), " * x"),
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
    equation = paste0("x = ", full_digits(a), signed_full(b), " * y"),
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


# -----------------------------------------------------------------------------
# mse_of_model(model, data = NULL, n = NULL)
# -----------------------------------------------------------------------------
# WHAT IT DOES : Calculates the MSE (mean squared error) of a model whose
#                numbers ARE ALREADY KNOWN, written as text like in the
#                statement. No fitting is done: it just plugs your data into
#                the formula and measures the error. It also gives SSE, RMSE
#                and R^2. Use it when the exercise GIVES you the equation
#                ("find the MSE of y = 3 + 0.5*x") or when you want to check
#                a model you fitted by hand.
#                (If the model has unknown letters a, b, c to find, use
#                fit_model() instead: it fits AND gives the MSE.)
# FORMULA (for reference — matches the slides' notation):
#   MSE = sum( n_i * (y_i - prediction_i)^2 ) / N
# PARAMETERS   :
#   model : the model as text with only numbers and your data variables,
#           e.g. "y = 3 + 0.5*x", "z = 2.2 - 1.05*x + 1.66*y",
#           "y = 5.88*exp(-0.13*x)" or "y = 5.88*e^(-0.13*x)" (e = Euler's
#           number). The variable before "=" is the real one.
#   data  : optional data.frame / list with the variables.
#   n     : optional frequencies of each observation.
# RETURNS      : Invisibly, a list with $mse, $sse, $rmse, $r_squared,
#                $predicted and $residuals. It also prints them.
# EXAMPLE      :
#   x <- c(1,2,3,4,5); y <- c(1,1,2,2,4)
#   mse_of_model("y = -0.1 + 0.7*x")      # MSE = 0.22
# -----------------------------------------------------------------------------
mse_of_model <- function(model, data = NULL, n = NULL) {
  caller <- parent.frame()
  env <- if (is.null(data)) caller else list2env(as.list(data), parent = caller)
  sides <- strsplit(model, "=", fixed = TRUE)[[1]]
  if (length(sides) != 2) stop('Write the model with exactly one "=", e.g. "y = 3 + 0.5*x".')
  resp_name <- trimws(sides[1])
  real <- get0(resp_name, envir = env, mode = "numeric")
  if (is.null(real)) stop("I can't find the variable `", resp_name, "` in your workspace or in `data`.")
  rhs <- parse(text = sides[2])[[1]]
  if (is.null(get0("e", envir = env, mode = "numeric")))     # e = Euler's number
    rhs <- do.call(substitute, list(rhs, list(e = exp(1))))
  unknown <- Filter(function(s) is.null(get0(s, envir = env, mode = "numeric")), setdiff(all.vars(rhs), "pi"))
  if (length(unknown) > 0)
    stop("The model still has unknown letters (", paste(unknown, collapse = ", "),
         "). Put numbers in them, or use fit_model() to find them.")
  predicted <- rep_len(as.numeric(eval(rhs, env)), length(real))
  if (!is.null(n) && length(n) != length(real)) stop("`n` must have the same length as the data.")
  sse <- sse_fit(real, predicted, n); mse <- mse_fit(real, predicted, n)
  report_title(paste0("MSE OF THE MODEL:  ", model))
  report_line("N (number of observations)", sum(freq_or_ones(real, n)))
  report_line("SSE (sum of squared errors)", sse)
  report_line("MSE (mean squared error = SSE / N)", mse)
  report_line(paste0("RMSE (typical error, in the units of ", resp_name, ")"), sqrt(mse))
  report_line(paste0("R^2 (1 - MSE / variance of ", resp_name, ")"), r_squared_fit(real, predicted, n))
  invisible(list(mse = mse, sse = sse, rmse = sqrt(mse),
                 r_squared = r_squared_fit(real, predicted, n),
                 predicted = predicted, residuals = real - predicted))
}


# -----------------------------------------------------------------------------
# INTERNAL: linearised_stats / report_linearised          (used by every fit that
# works through a change of variable: exponential, hyperbolic, power)
#   linearised_stats : fits the STRAIGHT LINE  Yt = A + B * Xt  (the transformed
#       data) and returns A, B, the correlation r of the transformed data, r^2
#       and the MSE of that straight line.
#   report_linearised: prints it all in plain English.
# -----------------------------------------------------------------------------
linearised_stats <- function(Xt, Yt, n = NULL) {
  B <- covariance(Xt, Yt, n) / variance(Xt, n)
  A <- arithmetic_mean(Yt, n) - B * arithmetic_mean(Xt, n)
  r <- covariance(Xt, Yt, n) / sqrt(variance(Xt, n) * variance(Yt, n))
  list(A = A, B = B, r = r, r2 = r^2,
       sse = sse_fit(Yt, A + B * Xt, n), mse = mse_fit(Yt, A + B * Xt, n))
}

report_linearised <- function(lin, line_text, A_text, B_text, back_text,
                              resp, sse, mse, r2_original) {
  report_section("Straight line after the change of variable")
  cat("  ", line_text, "\n", sep = "")
  report_line(A_text, lin$A)
  report_line(B_text, lin$B)
  for (b in back_text) report_line(b$label, b$value)
  report_line("r (correlation coefficient of the transformed data)", lin$r)
  report_line("MSE of the straight line (in transformed units)", lin$mse)
  report_section(paste0("Goodness of the curve on the ORIGINAL ", resp, " values"))
  report_line("SSE (sum of squared errors)", sse)
  report_line("MSE (mean squared error = SSE / N)", mse)
  report_line(paste0("RMSE (typical error, in the units of ", resp, ")"), sqrt(mse))
  report_line(paste0("Variance of ", resp, " (Vy, divides by N)"), mse / (1 - r2_original))
  report_line(paste0("R^2 OF THE COURSE = 1 - MSE / Vy"), r2_original)
  
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
fit_exponential <- function(x, y, n = NULL) {
  Xt <- x; Yt <- log(y)
  lin <- linearised_stats(Xt, Yt, n)
  a <- exp(lin$A)
  b <- lin$B
  fitted <- a * exp(b * x)
  sse <- sse_fit(y, fitted, n); mse <- mse_fit(y, fitted, n)
  structure(list(
    a = a, b = b, A = lin$A, B = lin$B, r = lin$r, r_squared_linear = lin$r2,
    mse_linear = lin$mse, sse = sse, mse = mse, rmse = sqrt(mse),
    r_squared = r_squared_fit(y, fitted, n), lin = lin,
    equation = paste0("y = ", full_digits(a), " * e^(", full_digits(b), " * x)"),
    description = "Exponential fit y = a * e^(b*x)",
    labels = c("a", "b"),
    report = function() report_linearised(lin, "ln(y) = A + B * x      with  A = ln(a)  and  B = b", "A = ln(a)  (intercept of the straight line)", "B = b  (slope of the straight line)",
                                          list(list(label = "a", value = a), list(label = "b", value = b)),
                                          "y", sse, mse, r_squared_fit(y, fitted, n))
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
fit_hyperbolic <- function(x, y, n = NULL) {
  Xt <- x; Yt <- 1 / y
  lin <- linearised_stats(Xt, Yt, n)
  a <- lin$A
  b <- lin$B
  fitted <- 1 / (a + b * x)
  sse <- sse_fit(y, fitted, n); mse <- mse_fit(y, fitted, n)
  structure(list(
    a = a, b = b, A = lin$A, B = lin$B, r = lin$r, r_squared_linear = lin$r2,
    mse_linear = lin$mse, sse = sse, mse = mse, rmse = sqrt(mse),
    r_squared = r_squared_fit(y, fitted, n), lin = lin,
    equation = paste0("y = 1 / (", full_digits(a), signed_full(b), " * x)"),
    description = "Hyperbolic fit y = 1 / (a + b*x)",
    labels = c("a", "b"),
    report = function() report_linearised(lin, "1/y = A + B * x      with  A = a  and  B = b", "A = a  (intercept of the straight line)", "B = b  (slope of the straight line)",
                                          list(list(label = "a", value = a), list(label = "b", value = b)),
                                          "y", sse, mse, r_squared_fit(y, fitted, n))
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
fit_power <- function(x, y, n = NULL) {
  Xt <- log(x); Yt <- log(y)
  lin <- linearised_stats(Xt, Yt, n)
  a <- exp(lin$A)
  b <- lin$B
  fitted <- a * x^b
  sse <- sse_fit(y, fitted, n); mse <- mse_fit(y, fitted, n)
  structure(list(
    a = a, b = b, A = lin$A, B = lin$B, r = lin$r, r_squared_linear = lin$r2,
    mse_linear = lin$mse, sse = sse, mse = mse, rmse = sqrt(mse),
    r_squared = r_squared_fit(y, fitted, n), lin = lin,
    equation = paste0("y = ", full_digits(a), " * x^", full_digits(b)),
    description = "Power fit y = a * x^b",
    labels = c("a", "b"),
    report = function() report_linearised(lin, "ln(y) = A + B * ln(x)      with  A = ln(a)  and  B = b", "A = ln(a)  (intercept of the straight line)", "B = b  (slope of the straight line)",
                                          list(list(label = "a", value = a), list(label = "b", value = b)),
                                          "y", sse, mse, r_squared_fit(y, fitted, n))
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

# -----------------------------------------------------------------------------
# join_signed_terms(values, labels, sep)        [you never call this yourself]
# -----------------------------------------------------------------------------
# WHAT IT DOES : Small helper used when printing a fit_model() result: it writes
#                "3.6591 a + 0.5309 b" with the right + and - signs.
# -----------------------------------------------------------------------------
join_signed_terms <- function(values, labels, sep = " ") {
  out <- ""
  for (i in seq_along(values)) {
    magnitude <- full_digits(abs(values[i]))
    term <- if (labels[i] == "") magnitude else paste0(magnitude, sep, labels[i])
    if (i == 1) {
      out <- paste0(if (values[i] < 0) "-" else "", term)
    } else {
      out <- paste0(out, if (values[i] < 0) " - " else " + ", term)
    }
  }
  out
}

# -----------------------------------------------------------------------------
# fit_model(model, data = NULL, n = NULL, plot = TRUE)   <- THE SIMPLEST ONE TO USE
# -----------------------------------------------------------------------------
# WHAT IT DOES : Fits ANY model that is linear in its unknown coefficients,
#                and you write the model AS TEXT, exactly as the exam gives
#                it. It does the whole exercise: builds and shows the normal
#                equations, solves them, and gives the coefficients, the
#                fitted values, the errors, SSE, MSE and R^2. The model is
#                written the way you read it in the statement, with your own
#                letters.
# HOW TO WRITE THE MODEL : put the model between quotes, with ONE "=" and an
#                explicit * between a coefficient and what it multiplies:
#     "y = a*x + b"                  straight line
#     "z = a + b*x + c*y"            plane (multiple regression)
#     "y = a*x^2 + b*x + c"          parabola
#     "y = a*sin(x) + b*cos(x)"      no constant term: just don't write one
#     "y = a + b*log(x)"             any function of x: sin, cos, log, exp...
#   * The unknown COEFFICIENTS are every letter in the model that is NOT one
#     of your data variables (a, b, c, ... normally). The DATA variables are
#     the ones you already created with the same length as the response
#     (x, y, z, ...). They are told apart automatically.
#   * Exponential, hyperbolic and power models with ONE variable are
#     recognised and fitted automatically by linearisation (same method as
#     fit_exponential(), fit_hyperbolic(), fit_power()): "y = a*exp(b*x)",
#     "y = a*b^x", "y = 1/(a + b*x)", "y = a*x^b". Any other model that is not
#     linear in its coefficients is refused with an explanation.
#   * The letter e means Euler's number: "y = a*e^(b*x)" works like
#     "y = a*exp(b*x)" (unless you have a data variable called e).
#   * The plot (data + fitted curve, or observed vs fitted if there are
#     several variables) is drawn automatically; plot = FALSE skips it.
# FORMULA (for reference — the normal equations, as in the slides):
#   M = one column per coefficient (what multiplies it, evaluated at the data)
#   (M' M) * coefficients = M' y       (with frequencies n: (M' W M) ... M' W y)
# PARAMETERS   :
#   model : the model as text, e.g. "y = a*x^2 + b*x + c" (see above). The
#           variable BEFORE the "=" is the one being predicted.
#   data  : optional data.frame or list with the variables (e.g. `pares`).
#           Leave it out to use the variables you already created.
#   plot  : FALSE to skip the plot.
#   n     : optional vector with how many times each observation repeats
#           (its absolute frequency), exactly as in the rest of this file.
# RETURNS      : An object that prints the normal equations (with YOUR letters
#                a, b, c...), the solution, the fitted equation with the
#                numbers filled in, and SSE / MSE / R^2. You can also use:
#                  $coefficients — named vector (a, b, c, ... in the order
#                                  they appear in the model).
#                  $fitted, $residuals, $sse, $mse, $r_squared (the slides'
#                  definition: 1 - MSE / variance of the response),
#                  $normal_lhs, $normal_rhs — the two sides of the normal
#                  equations.
# EXAMPLE      :
#   x <- c(0,1,0,-1,0,1,1,2); y <- c(0,1,1,1,3,2,2,0); z <- c(2,3,4,5,7,4,5,0)
#   fit_model("y = a*sin(x) + b*cos(x)")   # a = 0.6900588..., b = 1.5838855...
#   fit_model("y = a*x^2 + b*x + c")       # a = -0.5, b = 0.3333..., c = 1.5833...
#   fit_model("z = a + b*x + c*y")         # a = 2.2045..., b = -1.0568..., c = 1.6591...
# -----------------------------------------------------------------------------
fit_model <- function(model, data = NULL, n = NULL, plot = TRUE) {
  caller <- parent.frame()
  env <- if (is.null(data)) caller else list2env(as.list(data), parent = caller)
  sides <- strsplit(model, "=", fixed = TRUE)[[1]]
  if (length(sides) != 2) {
    stop('Write the model as text with exactly one "=", e.g. "y = a*x + b".')
  }
  response_name <- trimws(sides[1])
  rhs <- parse(text = sides[2])[[1]]
  if (is.null(get0("e", envir = env, mode = "numeric")))     # e = Euler's number
    rhs <- do.call(substitute, list(rhs, list(e = exp(1))))
  response <- get0(response_name, envir = env, mode = "numeric")
  if (is.null(response)) {
    stop("I can't find the variable `", response_name,
         "` (the one before the \"=\") in your workspace or in `data`.")
  }
  N <- length(response)
  
  # data variables = symbols that exist as numeric vectors of the same length
  # as the response; every other symbol is an unknown coefficient
  symbols <- setdiff(all.vars(rhs), "pi")
  is_data <- vapply(symbols, function(s) {
    v <- get0(s, envir = env, mode = "numeric")
    !is.null(v) && length(v) == N
  }, logical(1))
  params    <- symbols[!is_data]
  data_vars <- symbols[is_data]
  if (length(params) == 0) {
    stop("No unknown coefficients found. Use letters that are not data ",
         "variables for them, e.g. a, b, c.")
  }
  np <- length(params)
  
  # value of the right-hand side for given coefficient values
  value_of <- function(values) {
    e <- new.env(parent = env)
    for (j in 1:np) assign(params[j], values[j], envir = e)
    rep_len(as.numeric(eval(rhs, e)), N)
  }
  # exponential / hyperbolic / power models: recognised and fitted by
  # linearisation (like fit_exponential(), fit_hyperbolic(), fit_power())
  if (np == 2 && length(data_vars) == 1) {
    xv <- get(data_vars, envir = env)
    form <- recognize_nonlinear_form(value_of, xv)
    if (!is.null(form)) {
      fit <- fit_linearised_form(form, model, response_name, rhs, params,
                                 data_vars, env, xv, response, n)
      if (plot) plot_model_fit(fit)
      return(fit)
    }
  }
  offset <- value_of(rep(0, np))                # part with no coefficient
  M <- matrix(0, N, np)                         # one column per coefficient
  for (j in 1:np) {
    unit <- rep(0, np); unit[j] <- 1
    M[, j] <- value_of(unit) - offset
  }
  if (any(!is.finite(M)) || any(!is.finite(offset))) {
    stop("The model gives NA/Inf. Either it is not linear in the coefficients ",
         "(for example y = 1/(a + b*x): use fit_hyperbolic(); y = a*x^b: use ",
         "fit_power()), or a term fails for your data (for example log(0)).")
  }
  test <- 1.3 * (-1)^(1:np) * (1 + (1:np) / 7)  # is it really linear?
  if (!isTRUE(all.equal(value_of(test), as.vector(offset + M %*% test),
                        tolerance = 1e-8))) {
    stop("This model is not linear in the coefficients (",
         paste(params, collapse = ", "), "), so the normal equations can't ",
         "be used directly (only a*e^(b*x), a*b^x, 1/(a+b*x) and a*x^b are ",
         "recognised automatically).")
  }
  
  w <- if (is.null(n)) rep(1, N) else n
  normal_lhs <- t(M) %*% (w * M)
  normal_rhs <- as.vector(t(M) %*% (w * (response - offset)))
  dimnames(normal_lhs) <- list(params, params)
  solution <- tryCatch(solve(normal_lhs, normal_rhs), error = function(e) {
    stop("The normal equations have no single solution: two of your terms ",
         "are repeated or proportional (for example a*x + b*x).")
  })
  coefs <- setNames(as.vector(solution), params)
  fitted_values <- as.vector(offset + M %*% coefs)
  fit <- structure(list(
    model         = model,
    response_name = response_name,
    response      = response,
    weights       = n,
    note          = NULL,
    rhs           = rhs,
    params        = params,
    data_vars     = data_vars,
    env           = env,
    normal_lhs    = normal_lhs,
    normal_rhs    = normal_rhs,
    coefficients  = coefs,
    fitted        = fitted_values,
    residuals     = response - fitted_values,
    sse           = sse_fit(response, fitted_values, n),
    mse           = mse_fit(response, fitted_values, n),
    rmse          = sqrt(mse_fit(response, fitted_values, n)),
    r_squared     = r_squared_fit(response, fitted_values, n)
  ), class = "model_fit")
  if (plot) plot_model_fit(fit)
  fit
}

# -----------------------------------------------------------------------------
# predict_model(fit, new_data)
# -----------------------------------------------------------------------------
# WHAT IT DOES : Predicts the response for NEW values, using a model fitted
#                with fit_model().
# PARAMETERS   :
#   fit      : the result of fit_model().
#   new_data : a data.frame (or list) with the data variables of the model,
#              e.g. data.frame(x = c(3, 4)).
# RETURNS      : Vector with the predicted response, one per new value.
# EXAMPLE      :
#   x <- c(0,1,0,-1,0,1,1,2); y <- c(0,1,1,1,3,2,2,0)
#   model <- fit_model("y = a*x^2 + b*x + c")
#   predict_model(model, data.frame(x = c(3, 4)))
# -----------------------------------------------------------------------------
predict_model <- function(fit, new_data) {
  missing_vars <- setdiff(fit$data_vars, names(new_data))
  if (length(missing_vars) > 0) {
    stop("`new_data` needs the variable(s): ", paste(missing_vars, collapse = ", "))
  }
  e <- list2env(as.list(new_data), parent = fit$env)
  for (j in seq_along(fit$params)) assign(fit$params[j], fit$coefficients[[j]], envir = e)
  as.numeric(eval(fit$rhs, e))
}

# -----------------------------------------------------------------------------
# print.model_fit(x, ...)                  [you never call this yourself]
# -----------------------------------------------------------------------------
# WHAT IT DOES : Teaches R how to display the result of fit_model(): the
#                normal equations (with your own letters), their solution,
#                the fitted equation with the numbers filled in, and the
#                goodness of fit. It runs AUTOMATICALLY when you print a fit.
# -----------------------------------------------------------------------------
print.model_fit <- function(x, ...) {
  report_title(paste0("LEAST-SQUARES FIT:  ", x$model))
  if (!is.null(x$note)) cat("  ", x$note, "\n", sep = "")
  if (!is.null(x$normal_lhs)) {
    report_section("Normal equations (they give the coefficients)")
    for (i in seq_along(x$params)) {
      cat("  ", full_digits(x$normal_rhs[i]), " = ",
          join_signed_terms(x$normal_lhs[i, ], x$params), "\n", sep = "")
    }
  }
  report_section("Coefficients")
  for (i in seq_along(x$params)) {
    cat(sprintf("  %-4s= %s\n", x$params[i], full_digits(x$coefficients[i])))
  }
  filled <- do.call(substitute, list(x$rhs, as.list(x$coefficients)))
  filled <- paste(deparse(filled, width.cutoff = 500), collapse = "")
  filled <- gsub("+ -", "- ", filled, fixed = TRUE)
  report_section("Fitted model")
  cat("  ", x$response_name, " = ", filled, "\n", sep = "")
  if (!is.null(x$lin)) {
    t <- x$lin_texts
    A_lab <- switch(t$form, hyperbolic = paste0("A = ", t$P1), paste0("A = ln(", t$P1, ")"))
    B_lab <- switch(t$form, exponential_base = paste0("B = ln(", t$P2, ")"), paste0("B = ", t$P2))
    line_txt <- switch(t$form,
                       exponential      = paste0("ln(", t$resp, ") = A + B * ", t$xname),
                       exponential_base = paste0("ln(", t$resp, ") = A + B * ", t$xname),
                       hyperbolic       = paste0("1/", t$resp, " = A + B * ", t$xname),
                       power            = paste0("ln(", t$resp, ") = A + B * ln(", t$xname, ")"))
    report_section("Straight line after the change of variable")
    cat("  ", line_txt, "\n", sep = "")
    report_line(paste0(A_lab, "  (intercept of the straight line)"), x$lin$A)
    report_line(paste0(B_lab, "  (slope of the straight line)"), x$lin$B)
    for (k in seq_along(x$params)) report_line(x$params[k], x$coefficients[k])
    report_line("r (correlation coefficient of the transformed data)", x$lin$r)
    report_line("MSE of the straight line (in transformed units)", x$lin$mse)
  }
  report_section(if (!is.null(x$lin)) paste0("Goodness of the curve on the ORIGINAL ", x$response_name, " values") else "Goodness of fit")
  report_line("SSE (sum of squared errors)", x$sse)
  report_line("MSE (mean squared error = SSE / N)", x$mse)
  report_line("RMSE (typical error, in the units of the response)", x$rmse)
  if (!is.null(x$lin)) report_line(paste0("Variance of ", x$response_name, " (Vy, divides by N)"), variance(x$response, x$weights))
  report_line(paste0(if (!is.null(x$lin)) "R^2 OF THE COURSE = " else "R^2 = ", "1 - MSE / variance of ", x$response_name), x$r_squared)
  cat("  (R^2 close to 1 = the model explains almost all the variation)\n")
  invisible(x)
}

# -----------------------------------------------------------------------------
# INTERNAL: recognize_nonlinear_form / fit_linearised_form / plot_model_fit
# (called by fit_model(); you do not call them yourself)
#   recognize_nonlinear_form : checks numerically whether the typed model is
#       a*exp(b*x), a*b^x, 1/(a + b*x) or a*x^b (with any letters).
#   fit_linearised_form      : fits it as a straight line after transforming
#       (ln y, 1/y, ln x), with the SAME formulas as fit_exponential(), etc.,
#       but taking frequencies into account. SSE/MSE/R^2 are computed on the
#       ORIGINAL y scale.
#   plot_model_fit           : draws data + fitted curve (one variable) or
#       observed vs fitted values (several variables).
# -----------------------------------------------------------------------------
recognize_nonlinear_form <- function(value_of, xv) {
  forms <- list(
    exponential      = function(p, x) p[1] * exp(p[2] * x),
    exponential_base = function(p, x) p[1] * p[2]^x,
    hyperbolic       = function(p, x) 1 / (p[1] + p[2] * x),
    power            = function(p, x) p[1] * x^p[2])
  tests <- list(c(1.3, 0.7), c(0.6, 1.1), c(2.1, 0.4))
  for (nm in names(forms)) for (swap in c(FALSE, TRUE)) {
    ok <- all(vapply(tests, function(p) {
      got  <- suppressWarnings(tryCatch(value_of(p), error = function(e) NA_real_))
      want <- suppressWarnings(forms[[nm]](if (swap) rev(p) else p, xv))
      all(is.finite(got)) && all(is.finite(want)) &&
        isTRUE(all.equal(as.numeric(got), as.numeric(want), tolerance = 1e-9))
    }, logical(1)))
    if (ok) return(list(form = nm, swap = swap))
  }
  NULL
}

fit_linearised_form <- function(form, model, response_name, rhs, params,
                                data_vars, env, xv, response, n) {
  f <- form$form
  if (f != "hyperbolic" && any(response <= 0))
    stop("This model needs every `", response_name, "` value to be positive (it uses a logarithm).")
  if (f == "hyperbolic" && any(response == 0))
    stop("This model uses 1/", response_name, ", so no `", response_name, "` value can be 0.")
  if (f == "power" && any(xv <= 0))
    stop("This model needs every `", data_vars, "` value to be positive (it uses a logarithm).")
  idx <- if (form$swap) c(2, 1) else c(1, 2)       # which coefficient is the form's 1st / 2nd
  P1 <- params[idx[1]]; P2 <- params[idx[2]]
  Xt <- if (f == "power") log(xv) else xv
  Yt <- if (f == "hyperbolic") 1 / response else log(response)
  lin <- linearised_stats(Xt, Yt, n)
  A <- lin$A; B <- lin$B
  p <- switch(f, exponential = c(exp(A), B), exponential_base = c(exp(A), exp(B)),
              hyperbolic = c(A, B), power = c(exp(A), B))
  note <- switch(f,
                 exponential      = paste0("Fitted as a straight line:  ln(", response_name, ") = ln(", P1, ") + ", P2, "*", data_vars),
                 exponential_base = paste0("Fitted as a straight line:  ln(", response_name, ") = ln(", P1, ") + ln(", P2, ")*", data_vars),
                 hyperbolic       = paste0("Fitted as a straight line:  1/", response_name, " = ", P1, " + ", P2, "*", data_vars),
                 power            = paste0("Fitted as a straight line:  ln(", response_name, ") = ln(", P1, ") + ", P2, "*ln(", data_vars, ")"))
  coefs <- numeric(2); coefs[idx] <- p; names(coefs) <- params
  e <- list2env(as.list(setNames(list(xv), data_vars)), parent = env)
  for (j in 1:2) assign(params[j], coefs[[j]], envir = e)
  fitted_values <- as.numeric(eval(rhs, e))
  structure(list(
    model = model, response_name = response_name, response = response, weights = n,
    note = paste0(note, "\n  (SSE, MSE and R^2 below are measured on the ORIGINAL ",
                  response_name, " values, not on the transformed ones)"),
    rhs = rhs, params = params, data_vars = data_vars, env = env,
    normal_lhs = NULL, normal_rhs = NULL, coefficients = coefs,
    fitted = fitted_values, residuals = response - fitted_values,
    sse = sse_fit(response, fitted_values, n), mse = mse_fit(response, fitted_values, n),
    rmse = sqrt(mse_fit(response, fitted_values, n)),
    r_squared = r_squared_fit(response, fitted_values, n),
    lin = lin, lin_texts = list(P1 = P1, P2 = P2, form = f, resp = response_name, xname = data_vars)
  ), class = "model_fit")
}

plot_model_fit <- function(fit) {
  size <- if (is.null(fit$weights)) rep(1, length(fit$response)) else fit$weights
  if (length(fit$data_vars) == 1) {
    v <- fit$data_vars
    xv <- get(v, envir = fit$env)
    grid <- seq(min(xv), max(xv), length.out = 300)
    curve_y <- predict_model(fit, setNames(list(grid), v))
    plot(xv, fit$response, pch = 19, col = "steelblue", cex = sqrt(size),
         xlab = v, ylab = fit$response_name,
         main = paste0("Fit: ", fit$model),
         ylim = range(c(fit$response, curve_y), finite = TRUE))
    lines(grid, curve_y, col = "firebrick", lwd = 2)
    legend("topleft", legend = c("data (size = frequency)", "fitted model"),
           col = c("steelblue", "firebrick"), pch = c(19, NA), lty = c(NA, 1), bty = "n")
  } else if (length(fit$data_vars) > 1) {
    plot(fit$fitted, fit$response, pch = 19, col = "steelblue", cex = sqrt(size),
         xlab = paste0("fitted ", fit$response_name),
         ylab = paste0("observed ", fit$response_name),
         main = paste0("Observed vs fitted: ", fit$model))
    abline(0, 1, col = "firebrick", lwd = 2)
  }
  invisible(NULL)
}


# =============================================================================
# 7. GRAPHS
# =============================================================================

# -----------------------------------------------------------------------------
# plot_regression_lines(x, y, n = NULL, show_both = TRUE, main = ..., x_name = "X", y_name = "Y")
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
#   x_name, y_name : names shown on the axes and in the legend.
# RETURNS      : Invisibly, a list with $R_Y_X and $R_X_Y — the two fitted
#                models (each with $a and $b), in case you want to inspect
#                or reuse them. Mainly used for the plot it draws.
# EXAMPLE      :  (height/weight table from the slides)
#   pares <- table_to_pairs(tab, x_marks, y_marks)
#   plot_regression_lines(pares$x, pares$y, pares$n,
#                          main = "Height vs Weight")
# -----------------------------------------------------------------------------
plot_regression_lines <- function(x, y, n = NULL, show_both = TRUE,
                                  main = "Scatter plot and regression line(s)",
                                  x_name = "X", y_name = "Y") {
  n_size <- if (is.null(n)) rep(1, length(x)) else n
  model_yx <- regression_y_on_x(x, y, n)
  
  plot(x, y, cex = sqrt(n_size), pch = 19, col = "steelblue",
       xlab = x_name, ylab = y_name, main = main)
  abline(a = model_yx$a, b = model_yx$b, col = "firebrick", lwd = 2)
  
  legend_labels <- c("data (size = frequency)", paste0(y_name, " on ", x_name))
  legend_colors <- c("steelblue", "firebrick")
  legend_pch    <- c(19, NA)
  legend_lty    <- c(NA, 1)
  
  model_xy <- NULL
  if (show_both) {
    model_xy <- regression_x_on_y(x, y, n)
    # R_X/Y is x = a' + b'*y; rewritten as y = (x - a')/b' to draw it in the X-Y plane
    abline(a = -model_xy$a / model_xy$b, b = 1 / model_xy$b,
           col = "darkgreen", lwd = 2, lty = 2)
    legend_labels <- c(legend_labels, paste0(x_name, " on ", y_name))
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


# =============================================================================
# 0. MAIN FUNCTIONS (ONE CALL = FULL ANSWER)
# =============================================================================
# These are the functions to use in the exam. Each one prints a complete
# report in plain English, uses the real names of your variables, shows every
# decimal and draws the plot.
#
#   Two lists of numbers x, y (raw or with frequencies) -> linear_regression()
#   A two-way table (X by Y), values or intervals       -> analyze_table()
#   Two regression lines given as equations            -> analyze_regression_lines()
#   Any formula written as text, e.g. "y = a*x^2+b*x+c" -> fit_model()
# =============================================================================

# INTERNAL: plain-English reading of a correlation coefficient
describe_correlation <- function(r) {
  a <- abs(r)
  strength <- if (a >= 1 - 1e-12) "perfect" else if (a >= 0.9) "very strong"
  else if (a >= 0.7) "strong" else if (a >= 0.4) "moderate"
  else if (a >= 0.2) "weak" else "very weak or none"
  direction <- if (a < 1e-12) "" else if (r > 0) "positive (when one grows, the other tends to grow)"
  else "negative (when one grows, the other tends to decrease)"
  paste0(strength, if (nzchar(direction)) paste0(", ", direction) else " (no linear relationship)")
}

# INTERNAL: prints one regression line with its coefficients and errors
report_regression_line <- function(title, resp, pred, a, b, actual, fitted, n,
                                   course_name, slope_formula, direction) {
  report_section(title)
  cat("  Course notation: ", course_name, "  (", direction, ")\n", sep = "")
  cat("  Use it when you KNOW ", pred, " and want to ESTIMATE ", resp, ".\n", sep = "")
  cat("  Equation:  ", resp, " = a + b * ", pred, "\n", sep = "")
  cat("  Filled in: ", resp, " = ", full_digits(a), signed_full(b), " * ", pred, "\n", sep = "")
  report_line(paste0("a  (intercept: value of ", resp, " when ", pred, " = 0)"), a)
  report_line(paste0("b  (slope: change in ", resp, " for each +1 in ", pred, ")"), b)
  cat("     (", slope_formula, ")\n", sep = "")
  sse <- sse_fit(actual, fitted, n); mse <- mse_fit(actual, fitted, n)
  cat("  Errors of this line (real ", resp, " minus predicted ", resp, "):\n", sep = "")
  report_line("SSE (sum of squared errors)", sse)
  report_line("MSE (mean squared error = SSE / N)", mse)
  report_line(paste0("RMSE (typical error, in the units of ", resp, ")"), sqrt(mse))
}

# -----------------------------------------------------------------------------
# linear_regression(x, y, n = NULL, x_name = NULL, y_name = NULL,
#                   predict_y_at = NULL, predict_x_at = NULL, plot = TRUE)
# -----------------------------------------------------------------------------
# WHAT IT DOES : Everything about the straight-line relationship between two
#                variables, in ONE call: both regression lines (y on x and
#                x on y) with their a and b, the covariance, the correlation
#                coefficient r with a plain-English reading, R^2, the errors
#                (SSE, MSE, RMSE) of each line, optional predictions, and the
#                scatter plot with both lines. The names of your variables are
#                used everywhere (e.g. "weight = a + b * height").
# PARAMETERS   :
#   x, y         : the two variables, same length. One value per observation,
#                  or the different pairs together with their frequencies `n`.
#   n            : (optional) how many times each pair appears.
#   x_name,y_name: (optional) names to show; by default the names of the
#                  objects you passed (for pares$x it shows "x").
#   predict_y_at : (optional) value(s) of x at which to predict y.
#   predict_x_at : (optional) value(s) of y at which to predict x.
#   plot         : FALSE to skip the plot.
# RETURNS      : Invisibly, a list with y_on_x, x_on_y (each with $a, $b),
#                covariance, r, r_squared, N.
# EXAMPLE      :
#   height <- c(150,160,170,180); weight <- c(50,58,69,80)
#   linear_regression(height, weight, predict_y_at = 175)
#   # table data:  p <- table_to_pairs(tab, x_marks, y_marks)
#   #              linear_regression(p$x, p$y, p$n, x_name="height", y_name="weight")
# -----------------------------------------------------------------------------
linear_regression <- function(x, y, n = NULL, x_name = NULL, y_name = NULL,
                              predict_y_at = NULL, predict_x_at = NULL, plot = TRUE) {
  xn <- if (is.null(x_name)) variable_label(deparse(substitute(x)), "X") else x_name
  yn <- if (is.null(y_name)) variable_label(deparse(substitute(y)), "Y") else y_name
  if (length(x) != length(y)) stop("`x` and `y` must have the same length.")
  if (!is.null(n) && length(n) != length(x)) stop("`n` must have the same length as `x` and `y`.")
  w <- freq_or_ones(x, n)
  N <- sum(w)
  vx <- variance(x, n); vy <- variance(y, n)
  if (vx == 0 || vy == 0) stop("One of the variables is constant: there is no regression line.")
  cv <- covariance(x, y, n)
  r <- cv / sqrt(vx * vy)
  yx <- regression_y_on_x(x, y, n); xy <- regression_x_on_y(x, y, n)
  
  report_title(paste0("LINEAR REGRESSION:  ", yn, " and ", xn))
  report_line("Number of observations (N)", N)
  cat("\n  There are TWO different regression lines, because each one minimizes the errors\n",
      "  in a different direction (line 1: errors in ", yn, "; line 2: errors in ", xn, ").\n",
      "  Solving line 1 for ", xn, " does NOT give line 2. Both cross at the point\n",
      "  (mean of ", xn, ", mean of ", yn, ") = (", full_digits(arithmetic_mean(x, n)), ", ",
      full_digits(arithmetic_mean(y, n)), ").\n", sep = "")
  report_regression_line(paste0("Regression line 1: predicts ", yn, " from ", xn, "  (", yn, " on ", xn, ")"),
                         yn, xn, yx$a, yx$b, y, yx$a + yx$b * x, n,
                         "R_Y/X, the regression of Y on X",
                         paste0("b = Cov(", xn, ",", yn, ") / Var(", xn, "); a = mean ", yn, " - b * mean ", xn),
                         paste0("Y = ", yn, ", X = ", xn))
  report_regression_line(paste0("Regression line 2: predicts ", xn, " from ", yn, "  (", xn, " on ", yn, ")"),
                         xn, yn, xy$a, xy$b, x, xy$a + xy$b * y, n,
                         "R_X/Y, the regression of X on Y",
                         paste0("b' = Cov(", xn, ",", yn, ") / Var(", yn, "); a' = mean ", xn, " - b' * mean ", yn),
                         paste0("X = ", xn, ", Y = ", yn))
  report_section("Strength of the linear relationship")
  report_line(paste0("Covariance of ", xn, " and ", yn), cv)
  report_line("Correlation coefficient r (between -1 and 1)", r)
  cat(sprintf("  %-62s %s\n", "", paste("->", describe_correlation(r))))
  cat("     (r = Cov / (sd of ", xn, " * sd of ", yn, "); its sign is the sign of both slopes)\n", sep = "")
  report_line("R^2 = r^2 (share of the variation explained by the line)", r^2)
  report_line("Check: b (line 1) * b' (line 2) = r^2", yx$b * xy$b)
  cat("  (R^2 is the same for both lines. The errors (SSE, MSE, RMSE) of the two lines\n",
      "   are NOT comparable: line 1 is measured in ", yn, " units, line 2 in ", xn, " units.)\n", sep = "")
  if (!is.null(predict_y_at)) {
    report_section(paste0("Predictions of ", yn))
    for (v in predict_y_at) report_line(paste0("When ", xn, " = ", full_digits(v), ", predicted ", yn),
                                        yx$a + yx$b * v)
  }
  if (!is.null(predict_x_at)) {
    report_section(paste0("Predictions of ", xn))
    for (v in predict_x_at) report_line(paste0("When ", yn, " = ", full_digits(v), ", predicted ", xn),
                                        xy$a + xy$b * v)
  }
  if (plot) plot_regression_lines(x, y, n, TRUE,
                                  main = paste0(yn, " vs ", xn, " with regression lines"),
                                  x_name = xn, y_name = yn)
  invisible(list(y_on_x = yx, x_on_y = xy, covariance = cv, r = r,
                 r_squared = r^2, N = N))
}

# -----------------------------------------------------------------------------
# analyze_table(table, x_values = NULL, y_values = NULL,
#               x_limits = NULL, y_limits = NULL, rows_are = "X",
#               x_name = "X", y_name = "Y", given_x = NULL, given_y = NULL,
#               plot = TRUE)
# -----------------------------------------------------------------------------
# WHAT IT DOES : Complete analysis of a two-way frequency table in ONE call:
#                joint absolute and relative tables with their totals,
#                marginal distributions of X and of Y (mean, variance, sd),
#                independence check, optional conditional distributions,
#                covariance, correlation, both regression lines, and the plot.
# PARAMETERS   :
#   table    : matrix of absolute frequencies.
#   x_values, y_values : the value of X for each row of the table and the
#              value of Y for each column (class marks if grouped).
#              If omitted they are taken from the row/column names.
#   x_limits, y_limits : instead of values, give the interval BOUNDARIES
#              (one more than rows/columns), e.g. c(150,160,170,180); the
#              class marks are computed for you.
#   rows_are : "X" (default) if the rows of your table are X and the columns Y.
#              "Y" if the rows are Y (typical "Y/X" tables): it is transposed.
#   x_name, y_name : names to show.
#   given_x  : (optional) value(s) of X: shows the distribution of Y given X.
#   given_y  : (optional) value(s) of Y: shows the distribution of X given Y.
# RETURNS      : Invisibly, a list with the (x, y, n) pairs, the results of
#                the regression (see linear_regression) and `independent`.
# EXAMPLE      :
#   tab <- matrix(c(2,4,1, 0,4,6, 0,3,1), nrow = 3, byrow = TRUE)
#   analyze_table(tab, x_limits = c(150,160,170,180), y_limits = c(50,60,70,80),
#                 x_name = "height", y_name = "weight", given_x = 165)
# -----------------------------------------------------------------------------
analyze_table <- function(table, x_values = NULL, y_values = NULL,
                          x_limits = NULL, y_limits = NULL, rows_are = "X",
                          x_name = "X", y_name = "Y", given_x = NULL,
                          given_y = NULL, plot = TRUE) {
  tab <- as.matrix(table)
  if (toupper(rows_are) == "Y") {
    tab <- t(tab)
  } else if (toupper(rows_are) != "X") stop('`rows_are` must be "X" or "Y".')
  # values / marks of each variable
  get_marks <- function(values, limits, k, names_vec, what) {
    if (!is.null(limits)) {
      if (length(limits) != k + 1)
        stop("Your table has ", nrow(tab), " rows and ", ncol(tab), " columns (rows are ", toupper(rows_are),
             "). So `", what, "_limits` must have ", k + 1, " boundaries (one more than the number of ",
             if (what == "x") "rows" else "columns", ") but you gave ", length(limits),
             ". Check that `tab` is the table you mean and, if its rows are Y, use rows_are = \"Y\".")
      return(list(v = class_mark(limits[-(k + 1)], limits[-1]),
                  lab = interval_labels(limits[-(k + 1)], limits[-1], TRUE)))
    }
    if (is.null(values)) {
      values <- suppressWarnings(as.numeric(names_vec))
      if (is.null(names_vec) || anyNA(values))
        stop("Give `", what, "_values` (one per ", if (what == "x") "row" else "column",
             ") or `", what, "_limits`.")
    }
    if (length(values) != k) stop("`", what, "_values` must have ", k, " values.")
    list(v = values, lab = full_digits(values))
  }
  # if the user gave values/limits for X and Y in the ORIGINAL orientation, they
  # already refer to X and Y, so nothing else to swap
  mx <- get_marks(x_values, x_limits, nrow(tab), rownames(tab), "x")
  my <- get_marks(y_values, y_limits, ncol(tab), colnames(tab), "y")
  xv <- mx$v; yv <- my$v
  N <- sum(tab)
  rel <- tab / N
  ni <- rowSums(tab); nj <- colSums(tab)
  
  report_title(paste0("TWO-WAY TABLE ANALYSIS:  ", x_name, " (rows) and ", y_name, " (columns)"))
  show_tab <- function(m) {
    full <- rbind(cbind(m, Total = rowSums(m)), Total = c(colSums(m), sum(m)))
    dimnames(full) <- list(c(mx$lab, "Total"), c(my$lab, "Total"))
    print(full)
  }
  report_section(paste0("Joint absolute frequencies (rows = ", x_name, ", columns = ", y_name, ")"))
  show_tab(tab)
  report_section("Joint relative frequencies (absolute / N)")
  show_tab(rel)
  
  marg <- function(vals, freq, nm) {
    report_section(paste0("Marginal distribution of ", nm))
    report_line(paste0("Mean of ", nm), arithmetic_mean(vals, freq))
    report_line(paste0("Variance of ", nm, " (divides by N)"), variance(vals, freq))
    report_line(paste0("Standard deviation of ", nm), standard_deviation(vals, freq))
    report_line(paste0("Sample variance of ", nm, " (divides by N - 1)"), sample_quasi_variance(vals, freq))
  }
  marg(xv, ni, x_name); marg(yv, nj, y_name)
  
  report_section("Independence")
  expected <- outer(ni / N, nj / N)
  gap <- max(abs(rel - expected))
  indep <- gap <= 1e-8
  report_line("Largest gap between f_ij and (f_i. * f_.j)", gap)
  cat("  ->", if (indep) paste0(x_name, " and ", y_name, " are INDEPENDENT (f_ij = f_i. * f_.j in every cell)\n")
      else paste0(x_name, " and ", y_name, " are NOT independent (the relative frequencies are not the product of the marginals)\n"))
  
  cond <- function(given, vals_given, lab_given, vals_other, lab_other, freq_fun, gname, oname) {
    for (g in given) {
      k <- which(abs(vals_given - g) < 1e-9)
      if (length(k) == 0) {
        k <- which(lab_given == as.character(g))
        if (length(k) == 0) stop("The value ", g, " of ", gname, " is not in the table.")
      }
      f <- freq_fun(k[1])
      report_section(paste0("Distribution of ", oname, " given ", gname, " = ", g))
      df <- data.frame(value = lab_other, absolute = f, relative = f / sum(f))
      names(df)[1] <- oname
      print(df, row.names = FALSE)
      report_line(paste0("Mean of ", oname, " given ", gname, " = ", g), arithmetic_mean(vals_other, f))
      report_line(paste0("Variance of ", oname, " given ", gname, " = ", g), variance(vals_other, f))
    }
  }
  if (!is.null(given_x)) cond(given_x, xv, mx$lab, yv, my$lab, function(k) tab[k, ], x_name, y_name)
  if (!is.null(given_y)) cond(given_y, yv, my$lab, xv, mx$lab, function(k) tab[, k], y_name, x_name)
  
  pairs <- table_to_pairs(tab, xv, yv)
  res <- linear_regression(pairs$x, pairs$y, pairs$n, x_name = x_name, y_name = y_name, plot = plot)
  invisible(c(list(pairs = pairs, independent = indep, N = N), res))
}

# -----------------------------------------------------------------------------
# analyze_regression_lines(line1, line2, vars = c("x", "y"),
#      var_x = NULL, var_y = NULL, sd_x = NULL, sd_y = NULL, N = NULL,
#      remove = NULL, add = NULL, predict_y_at = NULL, predict_x_at = NULL,
#      line1_is = NULL)
# -----------------------------------------------------------------------------
# WHAT IT DOES : For exercises where you are GIVEN the two regression lines
#                as equations. It finds which one is y-on-x and which is
#                x-on-y, the point where they cross (the means), the
#                correlation coefficient, and, if you give a variance or sd,
#                the other variances and the covariance. With N and
#                remove/add it recomputes everything after taking out or
#                adding an observation.
# PARAMETERS   :
#   line1, line2 : the equations as text, e.g. "2*x - 3*y + 1 = 0" or
#                  "y = 3 + 0.5*x" ("3x" without * also works).
#   vars         : the two letters used in the equations: c(X name, Y name).
#   var_x, var_y, sd_x, sd_y : (optional) a known variance / standard
#                  deviation (divided by N). One is enough.
#   N            : (optional) number of observations (needed for remove/add).
#   remove, add  : (optional) c(x, y) of one observation to remove / add.
#   predict_y_at, predict_x_at : (optional) predictions.
#   line1_is     : "Y/X" or "X/Y" if you already know which line line1 is.
# RETURNS      : Invisibly, a list with the results.
# EXAMPLE      :
#   analyze_regression_lines("y = 0.5 + 0.8*x", "x = 1 + 0.5*y")
#   analyze_regression_lines("3*x + 2*y = 20", "x + 4*y = 15", sd_x = 2,
#                            N = 10, remove = c(4, 4))
# -----------------------------------------------------------------------------
analyze_regression_lines <- function(line1, line2, vars = c("x", "y"),
                                     var_x = NULL, var_y = NULL, sd_x = NULL, sd_y = NULL,
                                     N = NULL, remove = NULL, add = NULL,
                                     predict_y_at = NULL, predict_x_at = NULL,
                                     line1_is = NULL) {
  xn <- vars[1]; yn <- vars[2]
  parse_line <- function(txt) {
    sides <- strsplit(txt, "=", fixed = TRUE)[[1]]
    if (length(sides) != 2) stop('Write each line with one "=", e.g. "3*x + 2*y = 20".')
    fix <- function(s) gsub("([0-9.])\\s*([A-Za-z_])", "\\1*\\2", s)
    f <- function(xx, yy) {
      env <- setNames(list(xx, yy), vars)
      eval(parse(text = fix(sides[1])), env) - eval(parse(text = fix(sides[2])), env)
    }
    c0 <- f(0, 0)
    c(A = f(1, 0) - c0, B = f(0, 1) - c0, C = c0)     # A*x + B*y + C = 0
  }
  L1 <- parse_line(line1); L2 <- parse_line(line2)
  tol <- 1e-12
  # slope of y on x for a line, and of x on y
  yx_ok <- function(L) abs(L["B"]) > tol
  xy_ok <- function(L) abs(L["A"]) > tol
  candidate <- function(Lyx, Lxy) {
    if (!yx_ok(Lyx) || !xy_ok(Lxy)) return(NULL)
    b  <- -Lyx["A"] / Lyx["B"]
    bp <- -Lxy["B"] / Lxy["A"]
    r2 <- b * bp
    if (r2 >= -tol && r2 <= 1 + 1e-9) list(b = unname(b), bp = unname(bp), r2 = max(r2, 0)) else NULL
  }
  c1 <- candidate(L1, L2)   # line1 = y on x
  c2 <- candidate(L2, L1)   # line2 = y on x
  if (!is.null(line1_is)) {
    if (toupper(line1_is) == "Y/X") c2 <- NULL else c1 <- NULL
  }
  if (is.null(c1) && is.null(c2))
    stop("These two lines cannot be regression lines of the same data (the product of the two slopes b*b' must be between 0 and 1).")
  if (!is.null(c1) && !is.null(c2) && abs(c1$r2 - 1) > 1e-9)
    stop("Both assignments are possible; tell me which one is y on x with line1_is = \"Y/X\" or \"X/Y\".")
  if (!is.null(c1)) { Lyx <- L1; Lxy <- L2; cc <- c1; yx_name <- "line 1"; xy_name <- "line 2" }
  else { Lyx <- L2; Lxy <- L1; cc <- c2; yx_name <- "line 2"; xy_name <- "line 1" }
  b <- cc$b; bp <- cc$bp
  a  <- -unname(Lyx["C"]) / unname(Lyx["B"])         # y = a + b x
  ap <- -unname(Lxy["C"]) / unname(Lxy["A"])         # x = a' + b' y
  r <- sign(b) * sqrt(cc$r2)
  # intersection = (mean x, mean y)
  M <- rbind(c(Lyx["A"], Lyx["B"]), c(Lxy["A"], Lxy["B"]))
  cen <- solve(M, -c(Lyx["C"], Lxy["C"]))
  mx <- unname(cen[1]); my <- unname(cen[2])
  
  report_title("REGRESSION LINES ANALYSIS (given equations)")
  report_section("Which line is which")
  cat(sprintf("  %s is the regression of %s on %s:  %s = %s%s * %s\n", yx_name, yn, xn, yn,
              full_digits(a), signed_full(b), xn))
  cat(sprintf("  %s is the regression of %s on %s:  %s = %s%s * %s\n", xy_name, xn, yn, xn,
              full_digits(ap), signed_full(bp), yn))
  cat("  (rule: the two slopes satisfy b * b' = r^2 <= 1, which identifies each line)\n")
  report_section("Results")
  report_line(paste0("Slope of ", yn, " on ", xn, " (b)"), b)
  report_line(paste0("Slope of ", xn, " on ", yn, " (b')"), bp)
  report_line("R^2 = b * b'", cc$r2)
  report_line("Correlation coefficient r (sign = sign of the slopes)", r)
  if (cc$r2 > 0) cat(sprintf("  %-62s %s\n", "", paste("->", describe_correlation(r))))
  report_line(paste0("Mean of ", xn, " (the two lines cross at the means)"), mx)
  report_line(paste0("Mean of ", yn), my)
  
  # variances
  vx <- vy <- cv <- NULL
  if (!is.null(sd_x)) var_x <- sd_x^2
  if (!is.null(sd_y)) var_y <- sd_y^2
  if (!is.null(var_x)) { vx <- var_x; cv <- b * vx; vy <- if (abs(bp) > tol) cv / bp else NA }
  else if (!is.null(var_y)) { vy <- var_y; cv <- bp * vy; vx <- if (abs(b) > tol) cv / b else NA }
  if (!is.null(vx)) {
    report_section("Variances and covariance (from the data you gave)")
    report_line(paste0("Variance of ", xn), vx)
    report_line(paste0("Variance of ", yn), vy)
    report_line(paste0("Standard deviation of ", xn), sqrt(vx))
    report_line(paste0("Standard deviation of ", yn), sqrt(vy))
    report_line("Covariance (b * variance of x = b' * variance of y)", cv)
  } else if (cc$r2 > 0) {
    report_section("Ratio of spreads")
    report_line(paste0("sd(", yn, ") / sd(", xn, ") = b / r"), b / r)
    cat("  (give var_x, var_y, sd_x or sd_y to get the variances and covariance)\n")
  }
  
  if (!is.null(predict_y_at)) {
    report_section(paste0("Predictions of ", yn))
    for (v in predict_y_at) report_line(paste0("When ", xn, " = ", full_digits(v), ", predicted ", yn), a + b * v)
  }
  if (!is.null(predict_x_at)) {
    report_section(paste0("Predictions of ", xn))
    for (v in predict_x_at) report_line(paste0("When ", yn, " = ", full_digits(v), ", predicted ", xn), ap + bp * v)
  }
  
  out <- list(y_on_x = c(a = a, b = b), x_on_y = c(a = ap, b = bp), r = r,
              r_squared = cc$r2, mean_x = mx, mean_y = my,
              var_x = vx, var_y = vy, covariance = cv)
  
  # remove / add an observation
  if (!is.null(remove) || !is.null(add)) {
    if (is.null(N) || is.null(vx) || is.na(vx) || is.na(vy))
      stop("To remove/add an observation I need N and a variance (var_x, var_y, sd_x or sd_y).")
    Sx  <- N * mx;  Sy <- N * my
    Sxx <- N * (vx + mx^2); Syy <- N * (vy + my^2); Sxy <- N * (cv + mx * my)
    N2 <- N
    if (!is.null(remove)) {
      Sx <- Sx - remove[1]; Sy <- Sy - remove[2]; Sxx <- Sxx - remove[1]^2
      Syy <- Syy - remove[2]^2; Sxy <- Sxy - remove[1] * remove[2]; N2 <- N2 - 1
    }
    if (!is.null(add)) {
      Sx <- Sx + add[1]; Sy <- Sy + add[2]; Sxx <- Sxx + add[1]^2
      Syy <- Syy + add[2]^2; Sxy <- Sxy + add[1] * add[2]; N2 <- N2 + 1
    }
    mx2 <- Sx / N2; my2 <- Sy / N2
    vx2 <- Sxx / N2 - mx2^2; vy2 <- Syy / N2 - my2^2; cv2 <- Sxy / N2 - mx2 * my2
    b2 <- cv2 / vx2; bp2 <- cv2 / vy2
    report_section(paste0("After ", if (!is.null(remove)) paste0("removing (", paste(full_digits(remove), collapse = ", "), ")") else "",
                          if (!is.null(remove) && !is.null(add)) " and " else "",
                          if (!is.null(add)) paste0("adding (", paste(full_digits(add), collapse = ", "), ")") else ""))
    report_line("New number of observations", N2)
    report_line(paste0("New mean of ", xn), mx2)
    report_line(paste0("New mean of ", yn), my2)
    report_line(paste0("New variance of ", xn), vx2)
    report_line(paste0("New variance of ", yn), vy2)
    report_line("New covariance", cv2)
    report_line("New correlation coefficient r", cv2 / sqrt(vx2 * vy2))
    cat("  ", yn, " = ", full_digits(my2 - b2 * mx2), signed_full(b2), " * ", xn, "\n", sep = "")
    cat("  ", xn, " = ", full_digits(mx2 - bp2 * my2), signed_full(bp2), " * ", yn, "\n", sep = "")
    out$after <- list(N = N2, mean_x = mx2, mean_y = my2, var_x = vx2, var_y = vy2,
                      covariance = cv2, r = cv2 / sqrt(vx2 * vy2),
                      y_on_x = c(a = my2 - b2 * mx2, b = b2),
                      x_on_y = c(a = mx2 - bp2 * my2, b = bp2))
  }
  invisible(out)
}