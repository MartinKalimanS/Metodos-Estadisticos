#SOURCES
#source("chapter_1/formulas_chapter1.R")
#source("chapter_2/formulas_chapter2.R")
#source("chapter_3/formulas_chapter3.R")
# =============================================================================
#  CHAPTER 3 - TIME SERIES AND INDEX NUMBERS
#  One R function for every kind of exercise in the Chapter 3 slides.
#  (Department of Applied Mathematics, University of Malaga, 2024-2025)
# =============================================================================
#
#  HOW TO USE THIS FILE
#  ---------------------
#  1. Source Chapter 1 FIRST (this file reuses its printing helpers), then this
#     file. If formulas_chapter1.R is in the same folder, it is loaded for you:
#
#         source("formulas_chapter1.R")
#         source("formulas_chapter3.R")
#
#  2. Type the call that matches your exercise (see the cheat sheet below).
#     Every main function prints a complete report in plain English with ALL
#     the decimals, and draws the plot. Only base R is used.
#
#  CHEAT SHEET: EXACTLY WHAT TO TYPE FOR EACH CASE  (read this first!)
#  ===================================================================
#
#  TIME SERIES (the data changes over time)
#  ----------------------------------------
#  "Calculate the moving averages of order k"
#       moving_average(c(6, 6.2, 5.1, 4.9, 5.2, 5.8, 7), k = 3)
#       moving_average(x, k = 4, labels = 1959:1965)      # k even: extremes x 0.5
#
#  "Find the trend using least squares / regression line of the series"
#       trend_line(x)                         # t = 1, 2, 3, ...
#       trend_line(x, labels = 1959:1965, forecast = 2)   # + 2 periods ahead
#
#  Quarterly / monthly data table (rows = years, columns = seasons):
#  "Calculate the trend-cycle", "seasonal indices", "corrected seasonal
#  indices", "random component", "deseasonalised series", "regression line of
#  the deseasonalised series", "true trend", "predict the next periods"
#  -> ALL OF THEM with ONE call:
#       tab <- matrix(c(178.2,153.2,185.9,163.6,  196.3,156.9,197.9,166.4, ...),
#                     ncol = 4, byrow = TRUE,
#                     dimnames = list(1980:1984, c("Spring","Summer","Autumn","Winter")))
#       decompose_series(tab)                             # multiplicative X = T.E.C.A
#       decompose_series(tab, model = "additive")         # additive X = T+E+C+A
#       decompose_series(tab, forecast = 4)               # + predict next 4 periods
#
#  The same, but the data is a single list of numbers:
#       decompose_series(x, period = 4, start = c(1980, 1))
#       (period = 4 quarterly, 12 monthly;  start = c(first year, first season))
#
#  "Does the series have a cycle of period 3?" / "find the cycle"
#       autocorrelation(x, lag = 3)           # r between X(t) and X(t+3)
#       autocorrelation(x)                    # tries all the lags and finds the best
#
#  INDEX NUMBERS
#  -------------
#  "Calculate the simple index of 1980 with base 1970"
#       simple_index(148, 123)                # value in the period, value in the base
#  "Calculate all the elementary indices with base 2000"
#       index_numbers(c(1254, 1345, 1408, 1451), base = 2000, labels = 2000:2003)
#  "Index I(a/b) and I(b/c) are known, find I(a/c)"       chain_index(i_ab, i_bc)
#  "Index I(b/a) is known, find I(a/b)"                   invert_index(i_ba)
#
#  "Real value of a nominal amount" (deflation)
#       deflate(1000, 1.35)                   # CPI 135%: 1000 of the year t -> base year
#       deflate(1000, 135, percent = TRUE)    # the same, typing the CPI as a percentage
#       deflate(c(100,110,120), c(100,104,109), labels = 2020:2022)   # a whole series
#  "The salary grew 50% and the CPI doubled: how much did it really grow?"
#       real_change(nominal_change = 50, cpi_change = 100)   # inputs in %
#
#  TYPICAL MISTAKES: write quarterly tables ROW BY ROW (byrow = TRUE) |
#  use the right `period` (4 quarters, 12 months) | in deflate(), the CPI is
#  I(t/base): to bring a value of the base year to year t use invert_index().
#
#  INDEX (in order of the slides)
#  -------------------------------
#  0. MAIN FUNCTIONS (one call = full answer)
#       moving_average, trend_line, decompose_series, autocorrelation,
#       index_numbers, simple_index, deflate, real_change
#  1. INDEX NUMBER PROPERTIES
#       invert_index, chain_index
# =============================================================================

if (!exists("report_title") || !exists("full_digits")) {
  if (file.exists("formulas_chapter1.R")) {
    source("formulas_chapter1.R")
  } else {
    stop("Chapter 3 needs Chapter 1's functions. Run this first:\n",
         '  source("formulas_chapter1.R")\n',
         "and only then source this Chapter 3 file.")
  }
}
options(digits = 15)


# =============================================================================
# 0. MAIN FUNCTIONS (ONE CALL = FULL ANSWER)
# =============================================================================

# -----------------------------------------------------------------------------
# INTERNAL HELPERS (you do not call these yourself)
#   centred_moving_average : the moving average of order k of a vector.
#       k odd  -> plain average of k values, placed at the middle one.
#       k even -> average of k+1 values where the two EXTREMES count 0.5
#                 (this centres it), divided by k.
#   series_layout          : turns a vector (+ period, start) or a table
#                            (rows = years, columns = seasons) into the layout
#                            that decompose_series() works with.
#   as_table               : puts a vector in the years x seasons table.
#   show_table             : prints a table with "-" where there is no value.
# -----------------------------------------------------------------------------
centred_moving_average <- function(x, k) {
  n <- length(x)
  if (k < 2) stop("The order k of the moving average must be 2 or more.")
  if (k >= n) stop("The order k (", k, ") must be smaller than the number of data (", n, ").")
  w <- if (k %% 2 == 1) rep(1, k) / k else c(0.5, rep(1, k - 1), 0.5) / k
  half <- length(w) %/% 2
  out <- rep(NA_real_, n)
  for (i in (half + 1):(n - half)) out[i] <- sum(w * x[(i - half):(i + half)])
  out
}

default_season_names <- function(period) {
  if (period == 4)  return(c("Q1", "Q2", "Q3", "Q4"))
  if (period == 12) return(month.abb)
  if (period == 7)  return(c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))
  paste0("S", seq_len(period))
}

series_layout <- function(x, period, start, season_names) {
  if (is.matrix(x) || is.data.frame(x)) {
    m <- as.matrix(x)
    period <- ncol(m)
    values <- as.vector(t(m))
    rn <- rownames(m)
    yr <- suppressWarnings(as.numeric(rn))
    first_year <- if (!is.null(rn) && !anyNA(yr)) yr[1] else 1
    start_season <- 1
    if (is.null(season_names)) season_names <- if (!is.null(colnames(m))) colnames(m) else default_season_names(period)
  } else {
    if (is.null(period)) stop("Give the `period` (4 for quarterly data, 12 for monthly data) or a table with one column per season.")
    values <- as.numeric(x)
    first_year <- if (is.null(start)) 1 else start[1]
    start_season <- if (is.null(start) || length(start) < 2) 1 else start[2]
    if (is.null(season_names)) season_names <- default_season_names(period)
  }
  if (length(season_names) != period) stop("`season_names` must have ", period, " names.")
  n <- length(values)
  idx0 <- (start_season - 1) + (0:(n - 1))
  list(values = values, n = n, period = period, first_year = first_year,
       start_season = start_season, season_names = season_names,
       row = idx0 %/% period + 1, col = idx0 %% period + 1,
       season = idx0 %% period + 1,
       year = first_year + idx0 %/% period,
       n_rows = max(idx0 %/% period) + 1)
}

as_table <- function(v, L) {
  m <- matrix(NA_real_, L$n_rows, L$period,
              dimnames = list(L$first_year + 0:(L$n_rows - 1), L$season_names))
  for (i in seq_len(L$n)) m[L$row[i], L$col[i]] <- v[i]
  m
}

show_table <- function(m) print(m, na.print = "-")

# -----------------------------------------------------------------------------
# moving_average(x, k, labels = NULL, name = NULL, plot = TRUE)
# -----------------------------------------------------------------------------
# WHAT IT DOES : CASE: "Calculate the moving averages of order k" or "find the
#                trend using moving averages". Gives the moving average of
#                order k of a series, centred (placed at the middle period),
#                with ALL the decimals, and draws the series with its moving
#                average. If k is ODD each value is the plain mean of k
#                consecutive data. If k is EVEN the two extreme values count
#                half (x 0.5) so the result is centred (as in the slides).
#                It also shows how the first value was computed.
# FORMULA (for reference — matches the slides' notation):
#   k odd :  Y_t = (X_{t-h} + ... + X_{t+h}) / k             with h = (k-1)/2
#   k even:  Y_t = (0.5 X_{t-h} + X_{t-h+1} + ... + X_{t+h-1} + 0.5 X_{t+h}) / k
#            with h = k/2
# PARAMETERS   :
#   x      : the series, in time order (one value per period).
#   k      : order of the moving average (3, 4, 5, 12 ...). Use the period of
#            the seasonality if you know it (4 quarterly, 12 monthly).
#   labels : (optional) names of the periods, e.g. 1959:1965. Default 1, 2, 3...
#   name   : (optional) name of the variable for the report.
#   plot   : FALSE to skip the plot.
# RETURNS      : Invisibly, a data.frame with the period, X and the moving
#                average (NA at the ends, where it cannot be computed).
# EXAMPLE      :
#   x <- c(6, 6.2, 5.1, 4.9, 5.2, 5.8, 7)
#   moving_average(x, k = 3, labels = 1959:1965)   # 5.7667 5.4000 5.0667 5.3000 6.0000
#   moving_average(x, k = 4, labels = 1959:1965)   # 5.45 5.30 5.49
# -----------------------------------------------------------------------------
moving_average <- function(x, k, labels = NULL, name = NULL, plot = TRUE) {
  nm <- if (is.null(name)) variable_label(deparse(substitute(x)), "X") else name
  x <- as.numeric(x)
  n <- length(x)
  if (is.null(labels)) labels <- seq_len(n)
  if (length(labels) != n) stop("`labels` must have one entry per value of the series.")
  ma <- centred_moving_average(x, k)
  report_title(paste0("MOVING AVERAGE OF ORDER ", k, ":  ", nm))
  report_line("Number of observations (N)", n)
  report_line("Values that can be computed", sum(!is.na(ma)))
  cat("  Order ", k, if (k %% 2 == 1) " is ODD: each value is the mean of the k values around it.\n"
      else " is EVEN: the average uses k + 1 values and the two extremes count x 0.5 (to centre it).\n", sep = "")
  cat("  The first ", k %/% 2, " and the last ", k %/% 2, " periods are lost (no moving average there).\n", sep = "")
  report_section("Moving average")
  tab <- data.frame(period = labels, X = x, moving_average = ma)
  print(tab, row.names = FALSE, na.print = "-")
  first <- which(!is.na(ma))[1]
  half <- if (k %% 2 == 1) k %/% 2 else k / 2
  w <- if (k %% 2 == 1) rep(1, k) else c(0.5, rep(1, k - 1), 0.5)
  terms <- paste0(ifelse(w == 1, "", paste0(full_digits(w), " * ")), full_digits(x[(first - half):(first + half)]))
  report_section("How the first value was computed")
  cat("  (", paste(terms, collapse = " + "), ") / ", k, " = ", full_digits(ma[first]),
      "   -> assigned to period ", labels[first], "\n", sep = "")
  if (plot) {
    plot(seq_len(n), x, type = "b", pch = 19, col = "steelblue", xaxt = "n",
         xlab = "period", ylab = nm, main = paste0("Moving average of order ", k))
    axis(1, at = seq_len(n), labels = labels)
    lines(seq_len(n), ma, type = "b", pch = 15, col = "firebrick", lwd = 2)
    legend("topleft", legend = c(nm, paste0("moving average (", k, ")")),
           col = c("steelblue", "firebrick"), pch = c(19, 15), lty = 1, bty = "n")
  }
  invisible(tab)
}

# -----------------------------------------------------------------------------
# trend_line(x, labels = NULL, forecast = 0, name = NULL, plot = TRUE)
# -----------------------------------------------------------------------------
# WHAT IT DOES : CASE: "Find the trend of the series by least squares", "fit
#                the regression line of the series" or "predict the next
#                values with the trend" (series WITHOUT seasonality, or when
#                they just ask for the straight-line trend). Numbers the
#                periods t = 1, 2, 3, ... and fits X = a + b * t. Gives a, b,
#                r, R^2, the errors, the trend value of each period, the
#                prediction for the next periods and the plot.
#                (If the series has seasons and the exercise asks for the TRUE
#                trend, use decompose_series() instead: it fits the line to
#                the deseasonalised series.)
# FORMULA (for reference — matches the slides' notation):
#   b = Cov(t, X) / Var(t)        a = mean(X) - b * mean(t)
# PARAMETERS   :
#   x        : the series, in time order.
#   labels   : (optional) names of the periods (years, ...). Default 1, 2, 3...
#   forecast : (optional) how many periods AFTER the last one to predict.
#   name     : (optional) name of the variable.
#   plot     : FALSE to skip the plot.
# RETURNS      : Invisibly, a list with a, b, r, r_squared, mse, trend
#                (fitted values) and forecast.
# EXAMPLE      :
#   trend_line(c(6, 6.2, 5.1, 4.9, 5.2, 5.8, 7), labels = 1959:1965, forecast = 2)
# -----------------------------------------------------------------------------
trend_line <- function(x, labels = NULL, forecast = 0, name = NULL, plot = TRUE) {
  nm <- if (is.null(name)) variable_label(deparse(substitute(x)), "X") else name
  x <- as.numeric(x); n <- length(x); t <- seq_len(n)
  if (is.null(labels)) labels <- t
  if (length(labels) != n) stop("`labels` must have one entry per value of the series.")
  vt <- mean(t^2) - mean(t)^2
  cv <- mean(t * x) - mean(t) * mean(x)
  b <- cv / vt; a <- mean(x) - b * mean(t)
  vx <- mean(x^2) - mean(x)^2
  r <- if (vx > 0) cv / sqrt(vt * vx) else NA
  fit <- a + b * t
  mse <- mean((x - fit)^2)
  report_title(paste0("TREND OF THE SERIES (least squares):  ", nm))
  cat("  The periods are numbered t = 1, 2, ..., ", n, ".  Trend line:  ", nm, " = a + b * t\n", sep = "")
  report_line("N (number of periods)", n)
  report_section("Trend line")
  cat("  ", nm, " = ", full_digits(a), signed_full_t(b), " * t\n", sep = "")
  report_line("a  (intercept: trend value at t = 0)", a)
  report_line(paste0("b  (slope: change of ", nm, " per period)"), b)
  cat("  ->", if (b > 0) "the trend is INCREASING" else if (b < 0) "the trend is DECREASING" else "there is no trend", "\n")
  report_section("Quality of the line")
  report_line("r (correlation of t and the series)", r)
  report_line("R^2 = r^2", r^2)
  report_line("MSE (mean squared error)", mse)
  report_line("RMSE", sqrt(mse))
  report_section("Trend value of each period")
  print(data.frame(period = labels, t = t, X = x, trend = fit), row.names = FALSE)
  fc <- NULL
  if (forecast > 0) {
    tf <- n + seq_len(forecast)
    flabels <- if (is.numeric(labels) && n > 1 && all(diff(labels) == diff(labels)[1]))
      labels[n] + diff(labels)[1] * seq_len(forecast) else paste0("t=", tf)
    fc <- data.frame(period = flabels, t = tf, prediction = a + b * tf)
    report_section("Prediction with the trend line")
    print(fc, row.names = FALSE)
  }
  if (plot) {
    tt <- c(t, if (!is.null(fc)) fc$t)
    plot(t, x, type = "b", pch = 19, col = "steelblue", xlim = range(tt),
         ylim = range(c(x, fit, fc$prediction)), xlab = "t", ylab = nm,
         main = paste0("Trend line of ", nm))
    abline(a = a, b = b, col = "firebrick", lwd = 2)
    if (!is.null(fc)) points(fc$t, fc$prediction, pch = 17, col = "darkgreen", cex = 1.3)
    legend("topleft", legend = c(nm, "trend line", if (!is.null(fc)) "prediction"),
           col = c("steelblue", "firebrick", if (!is.null(fc)) "darkgreen"),
           pch = c(19, NA, if (!is.null(fc)) 17), lty = c(1, 1, if (!is.null(fc)) NA), bty = "n")
  }
  invisible(list(a = a, b = b, r = r, r_squared = r^2, mse = mse, trend = fit, forecast = fc))
}

# INTERNAL: " + 3.2" / " - 3.2" in front of a number (so we never print "+ -3.2")
signed_full_t <- function(v) paste0(ifelse(v < 0, " - ", " + "), full_digits(abs(v)))

# -----------------------------------------------------------------------------
# decompose_series(x, period = NULL, start = NULL, model = "multiplicative",
#                  season_names = NULL, forecast = 0, name = NULL, plot = TRUE)
# -----------------------------------------------------------------------------
# WHAT IT DOES : CASE: any exercise with a QUARTERLY / MONTHLY (seasonal)
#                series: "calculate the trend-cycle", "the seasonal variation
#                indices (corrected or not)", "the random component", "the
#                deseasonalised series", "the regression line / true trend",
#                "predict the next periods". ONE call gives everything:
#                  1. Trend-cycle (T.C) by moving averages of order = period.
#                  2. X / (T.C)  [or X - (T.C) if additive]  = E.A table.
#                  3. Seasonal indices (average of each season), their sum
#                     (should be 400 % for quarterly data) -> UNCORRECTED.
#                  4. CORRECTED (normalised) seasonal indices, with the
#                     interpretation of each season.
#                  5. Random component A.
#                  6. Deseasonalised series X / E.
#                  7. Regression line of the deseasonalised series
#                     (t = 1, 2, 3...): the TRUE (corrected) trend.
#                  8. Prediction of future periods = trend x seasonal index.
#                  9. The plots.
# FORMULA (for reference — matches the slides' notation):
#   Multiplicative X = T.E.C.A :  E.A = X/(T.C);  uncorrected index = mean of
#       each season;  corrected I' = I * period / sum(I);  A = (X/(T.C)) / I';
#       deseasonalised = X / I';  prediction = (a + b t) * I'.
#   Additive X = T+E+C+A :  E+A = X-(T.C);  corrected I' = I - mean(I)
#       (they must add 0);  A = (X-(T.C)) - I';  deseasonalised = X - I';
#       prediction = (a + b t) + I'.
# PARAMETERS   :
#   x            : EITHER a table/matrix (rows = years, columns = seasons;
#                  type it row by row with byrow = TRUE and put the years as
#                  row names) OR a plain vector in time order.
#   period       : only if x is a vector: 4 (quarterly), 12 (monthly)...
#   start        : only if x is a vector: c(first year, first season),
#                  e.g. c(1980, 1). Default c(1, 1).
#   model        : "multiplicative" (default, X = T.E.C.A) or "additive".
#   season_names : (optional) names of the seasons. Default: the column names
#                  of the table, or Q1..Q4 / Jan..Dec.
#   forecast     : (optional) number of future periods to predict.
#   name         : (optional) name of the variable.
#   plot         : FALSE to skip the plots.
# RETURNS      : Invisibly, a list with trend_cycle, ratio (E.A), raw_indices,
#                indices (corrected), random, deseasonalised, a, b,
#                true_trend and forecast.
# EXAMPLE      :  (the "Muja company" quarterly production, 1980-1984)
#   tab <- matrix(c(178.2,153.2,185.9,163.6, 196.3,156.9,197.9,166.4,
#                   197.3,159.7,202.6,175.6, 209.5,169.5,202.4,179.8,
#                   200.0,168.6,216.1,178.5), ncol = 4, byrow = TRUE,
#                 dimnames = list(1980:1984, c("Spring","Summer","Autumn","Winter")))
#   decompose_series(tab)                 # corrected indices 109.1606 %, 88.2746 %, ...
#   decompose_series(tab, forecast = 4)   # + production of 1985
# -----------------------------------------------------------------------------
decompose_series <- function(x, period = NULL, start = NULL, model = "multiplicative",
                             season_names = NULL, forecast = 0, name = NULL, plot = TRUE) {
  nm <- if (is.null(name)) variable_label(deparse(substitute(x)), "X") else name
  model <- match.arg(model, c("multiplicative", "additive"))
  mult <- model == "multiplicative"
  L <- series_layout(x, period, start, season_names)
  p <- L$period; n <- L$n; v <- L$values
  if (p < 2) stop("The period must be 2 or more.")
  if (n < 2 * p) stop("At least two full periods (", 2 * p, " data) are needed.")
  tc  <- centred_moving_average(v, p)                       # trend-cycle
  ea  <- if (mult) v / tc else v - tc                       # E.A
  raw <- vapply(seq_len(p), function(s) mean(ea[L$season == s], na.rm = TRUE), numeric(1))
  idx <- if (mult) raw * p / sum(raw) else raw - mean(raw)  # corrected
  names(raw) <- names(idx) <- L$season_names
  I   <- idx[L$season]
  a_c <- if (mult) ea / I else ea - I                       # random component
  des <- if (mult) v / I else v - I                         # deseasonalised
  t   <- seq_len(n)
  vt  <- mean(t^2) - mean(t)^2
  b   <- (mean(t * des) - mean(t) * mean(des)) / vt
  a   <- mean(des) - b * mean(t)
  tt  <- a + b * t                                          # true trend
  r   <- (mean(t * des) - mean(t) * mean(des)) / sqrt(vt * (mean(des^2) - mean(des)^2))
  unit <- if (mult) 100 else 1
  
  report_title(paste0("TIME SERIES DECOMPOSITION (", model, "):  ", nm))
  report_line("Number of observations (N)", n)
  report_line("Period of the seasonality", p)
  cat("  Model: ", if (mult) "X = T . E . C . A  (multiplicative)" else "X = T + E + C + A  (additive)", "\n", sep = "")
  
  report_section(paste0("1. Trend-cycle T.C  (moving average of order ", p, if (p %% 2 == 0) ", extremes x 0.5)" else ")"))
  show_table(as_table(tc, L))
  cat("  (The first and last ", p %/% 2, " periods are lost.)\n", sep = "")
  
  report_section(paste0("2. ", if (mult) "E.A = X / (T.C)" else "E+A = X - (T.C)", "   (seasonal and random components)"))
  show_table(as_table(ea, L))
  
  report_section(paste0("3. Seasonal variation indices, UNCORRECTED (mean of each season", if (mult) ", in %)" else ")"))
  print(raw * unit)
  report_line(paste0("Sum of the indices (should be ", if (mult) paste0(p * 100, " %") else "0", ")"), sum(raw) * unit)
  cat("  -> They do not add up exactly, so they must be corrected.\n")
  
  report_section(paste0("4. Seasonal variation indices, CORRECTED (normalised", if (mult) ", in %)" else ")"))
  print(idx * unit)
  report_line("Sum of the corrected indices", sum(idx) * unit)
  cat("  Interpretation:\n")
  for (s in seq_len(p)) {
    if (mult) {
      d <- idx[s] * 100 - 100
      cat(sprintf("   - %s: index %s %%  ->  %s %% %s than the annual average\n", L$season_names[s],
                  full_digits(idx[s] * 100), full_digits(abs(d)), if (d >= 0) "HIGHER" else "LOWER"))
    } else {
      cat(sprintf("   - %s: index %s  ->  %s %s the annual average\n", L$season_names[s],
                  full_digits(idx[s]), full_digits(abs(idx[s])), if (idx[s] >= 0) "ABOVE" else "BELOW"))
    }
  }
  
  report_section(paste0("5. Random component A  (", if (mult) "(X/(T.C)) / corrected index; ideally close to 1" else "(X-(T.C)) - corrected index; ideally close to 0", ")"))
  show_table(as_table(a_c, L))
  
  report_section(paste0("6. Deseasonalised series  ", if (mult) "X / E = T.C.A" else "X - E = T+C+A"))
  show_table(as_table(des, L))
  
  report_section("7. Regression line of the deseasonalised series (t = 1, 2, ..., N)  = TRUE TREND")
  cat("  deseasonalised X = a + b * t\n  deseasonalised X = ", full_digits(a), signed_full_t(b), " * t\n", sep = "")
  report_line("a  (intercept)", a)
  report_line("b  (slope: change of the trend per period)", b)
  report_line("r (correlation of t and the deseasonalised series)", r)
  report_line("R^2 = r^2", r^2)
  cat("  -> the trend is ", if (b > 0) "INCREASING" else if (b < 0) "DECREASING" else "flat", "\n", sep = "")
  report_section("   True (corrected) trend T of each period = a + b * t")
  show_table(as_table(tt, L))
  
  fc <- NULL
  if (forecast > 0) {
    tf <- n + seq_len(forecast)
    idx0 <- (L$start_season - 1) + (tf - 1)
    sf <- idx0 %% p + 1
    yf <- L$first_year + idx0 %/% p
    pred <- if (mult) (a + b * tf) * idx[sf] else (a + b * tf) + idx[sf]
    fc <- data.frame(year = yf, season = L$season_names[sf], t = tf,
                     trend = a + b * tf, seasonal_index = unname(idx[sf]), prediction = pred)
    report_section(paste0("8. Prediction = trend ", if (mult) "x" else "+", " corrected seasonal index"))
    print(fc, row.names = FALSE)
  }
  
  if (plot) {
    op <- par(mfrow = c(2, 2)); on.exit(par(op), add = TRUE)
    plot(t, v, type = "l", col = "steelblue", xlab = "t", ylab = nm, main = "Series and trend-cycle")
    lines(t, tc, col = "firebrick", lwd = 2)
    legend("topleft", legend = c(nm, "trend-cycle"), col = c("steelblue", "firebrick"), lty = 1, bty = "n", cex = 0.8)
    barplot(idx * unit, col = "steelblue", main = "Corrected seasonal indices", las = 2)
    abline(h = if (mult) 100 else 0, lty = 2)
    plot(t, a_c, type = "b", pch = 19, col = "gray30", xlab = "t", ylab = "A", main = "Random component")
    abline(h = if (mult) 1 else 0, lty = 2)
    plot(t, des, type = "l", col = "steelblue", xlab = "t", ylab = nm,
         ylim = range(c(des, tt, if (!is.null(fc)) fc$prediction)), main = "Deseasonalised series and true trend")
    abline(a = a, b = b, col = "firebrick", lwd = 2)
    if (!is.null(fc)) points(fc$t, fc$prediction, pch = 17, col = "darkgreen")
  }
  invisible(list(trend_cycle = as_table(tc, L), ratio = as_table(ea, L), raw_indices = raw,
                 indices = idx, random = as_table(a_c, L), deseasonalised = as_table(des, L),
                 a = a, b = b, r = r, true_trend = as_table(tt, L), forecast = fc))
}

# -----------------------------------------------------------------------------
# autocorrelation(x, lag = NULL, threshold = 0.95, plot = TRUE)
# -----------------------------------------------------------------------------
# WHAT IT DOES : CASE: "Determine if the time series has a cycle of period T"
#                or "find the cycle of the series". Correlates the series X(t)
#                with the same series shifted `lag` periods, X(t+lag). If
#                r is close to 1, the series repeats itself every `lag`
#                periods: there is a cycle of that period.
#                - With `lag`: shows the table of pairs, r, and the verdict.
#                - Without `lag`: tries every lag (1, 2, 3, ...) and tells you
#                  the best one.
# FORMULA (for reference — matches the slides' notation):
#   r = correlation coefficient between (X_1..X_{N-lag}) and (X_{1+lag}..X_N)
# PARAMETERS   :
#   x         : the series, in time order.
#   lag       : (optional) the period to test, e.g. 3. Leave empty to scan.
#   threshold : r from which a cycle is declared (default 0.95, "r close to 1").
#   plot      : FALSE to skip the plot.
# RETURNS      : Invisibly, r (one lag) or a data.frame lag / pairs / r (scan).
# EXAMPLE      :
#   x <- c(1.2, 1.4, 2.0, 1.23, 1.37, 2.07, 1.25, 1.47, 2.09)
#   autocorrelation(x, lag = 3)     # r = 0.9936708...  -> cycle of period 3
#   autocorrelation(x)              # tries all the lags (needs at least 7 data)
# -----------------------------------------------------------------------------
autocorrelation <- function(x, lag = NULL, threshold = 0.95, plot = TRUE) {
  nm <- variable_label(deparse(substitute(x)), "X")
  x <- as.numeric(x); n <- length(x)
  r_at <- function(l) cor(x[1:(n - l)], x[(1 + l):n])
  if (!is.null(lag)) {
    if (lag < 1 || n - lag < 3) stop("The lag must be between 1 and ", n - 3, " (at least 3 pairs are needed).")
    r <- r_at(lag)
    report_title(paste0("AUTOCORRELATION WITH LAG ", lag, ":  ", nm))
    report_section(paste0("Series X(t) and the same series shifted ", lag, " periods X(t+", lag, ")"))
    pair_tab <- rbind(`X(t)` = x, `X(t-lag)` = c(rep(NA, lag), x[1:(n - lag)]))
    rownames(pair_tab)[2] <- paste0("X(t-", lag, ")")
    colnames(pair_tab) <- paste0("t=", 1:n)
    show_table(pair_tab)
    report_line("Number of pairs used", n - lag)
    report_line(paste0("r = correlation of X(t) and X(t-", lag, ")"), r)
    cat("  ->", if (r >= threshold) paste0("r is close to 1: the series HAS A CYCLE of period ", lag)
        else paste0("r is not close to 1: there is NO clear cycle of period ", lag), "\n")
    if (plot) {
      plot(x[1:(n - lag)], x[(1 + lag):n], pch = 19, col = "steelblue",
           xlab = "X(t)", ylab = paste0("X(t+", lag, ")"), main = paste0("Autocorrelation, lag ", lag, ",  r = ", signif(r, 6)))
      abline(lm(x[(1 + lag):n] ~ x[1:(n - lag)]), col = "firebrick", lwd = 2)
    }
    return(invisible(r))
  }
  if (n < 7) stop("At least 7 data are needed to scan the lags; give a `lag` instead.")
  lags <- 1:(n - 4)                      # at least 4 pairs per lag
  rs <- vapply(lags, r_at, numeric(1))
  report_title(paste0("AUTOCORRELATION FOR EVERY LAG:  ", nm))
  tab <- data.frame(lag = lags, pairs = n - lags, r = rs,
                    cycle = ifelse(rs >= threshold, "<- r close to 1", ""))
  print(tab, row.names = FALSE)
  hit <- which(rs >= threshold)
  report_section("Conclusion")
  if (length(hit) > 0) {
    best <- hit[1]
    report_line("Smallest lag with r close to 1 (the period of the cycle)", lags[best])
    report_line("its r", rs[best])
    cat("  -> the series HAS A CYCLE of period ", lags[best],
        " (multiples of it, such as ", 2 * lags[best], ", also correlate)\n", sep = "")
  } else {
    best <- which.max(rs)
    report_line("Lag with the highest r", lags[best])
    report_line("its r", rs[best])
    cat("  -> no lag has r close to 1: NO clear cycle\n")
  }
  if (plot) {
    barplot(rs, names.arg = lags, col = ifelse(rs >= threshold, "firebrick", "steelblue"),
            xlab = "lag", ylab = "r", main = "Autocorrelation by lag", ylim = c(min(0, rs), 1))
    abline(h = threshold, lty = 2)
  }
  invisible(tab)
}


# =============================================================================
# INDEX NUMBERS
# =============================================================================

# -----------------------------------------------------------------------------
# simple_index(value, base_value, percent = FALSE)
# -----------------------------------------------------------------------------
# WHAT IT DOES : CASE: "Calculate the simple (elementary) index of period t
#                with base period t0" / "percentage of variation of X between
#                two periods". Gives the index and the percentage of variation.
# FORMULA (for reference — matches the slides' notation):
#   X_{t/t0} = X_t / X_t0           variation (%) = (X_{t/t0} - 1) * 100
# PARAMETERS   :
#   value      : value of the variable in the period t (the one you compare).
#   base_value : value in the base period t0.
# RETURNS      : Invisibly, the index (as a ratio; 1.2033 = 120.33 %).
# EXAMPLE      :
#   simple_index(148, 123)     # 1.2033 -> increase of 20.33 % with respect to the base
# -----------------------------------------------------------------------------
simple_index <- function(value, base_value) {
  idx <- value / base_value
  report_title("SIMPLE INDEX NUMBER")
  report_line("Value in the period t", value)
  report_line("Value in the base period t0", base_value)
  report_line("Index X(t/t0) = value / base value", idx)
  report_line("Index in percentage", idx * 100)
  report_line("Variation with respect to the base period (%)", (idx - 1) * 100)
  cat("  ->", if (idx > 1) paste0("an INCREASE of ", full_digits((idx - 1) * 100), " % with the base period")
      else if (idx < 1) paste0("a DECREASE of ", full_digits((1 - idx) * 100), " % with the base period")
      else "no change", "\n")
  invisible(idx)
}

# -----------------------------------------------------------------------------
# index_numbers(values, base = 1, labels = NULL, name = NULL, plot = TRUE)
# -----------------------------------------------------------------------------
# WHAT IT DOES : CASE: "Calculate all the elementary indices taking year X as
#                the base period" or "express the series as an index". Gives
#                the index of EVERY period with respect to the base, as a
#                ratio and as a percentage, plus the percentage of variation.
# FORMULA (for reference — matches the slides' notation):
#   C_{t/base} = X_t / X_base
# PARAMETERS   :
#   values : the value of the variable in each period.
#   base   : the base period. Give its LABEL (e.g. 2000) if you use `labels`,
#            otherwise its POSITION (1 = the first period). Default: the first.
#   labels : (optional) names of the periods, e.g. 2000:2003.
#   name   : (optional) name of the variable.
#   plot   : FALSE to skip the plot.
# RETURNS      : Invisibly, a data.frame with period, value, index, percentage
#                and variation.
# EXAMPLE      :
#   index_numbers(c(1254, 1345, 1408, 1451), base = 2000, labels = 2000:2003)
#   # 1, 1.0726, 1.1228, 1.1571
# -----------------------------------------------------------------------------
index_numbers <- function(values, base = 1, labels = NULL, name = NULL, plot = TRUE) {
  nm <- if (is.null(name)) variable_label(deparse(substitute(values)), "X") else name
  n <- length(values)
  if (is.null(labels)) labels <- seq_len(n)
  if (length(labels) != n) stop("`labels` must have one entry per value.")
  pos <- if (!is.null(labels) && base %in% labels) which(labels == base)[1] else base
  if (is.na(pos) || pos < 1 || pos > n) stop("The base period is not in the data.")
  idx <- values / values[pos]
  report_title(paste0("INDEX NUMBERS:  ", nm, "  (base period = ", labels[pos], ")"))
  report_line("Value of the base period", values[pos])
  tab <- data.frame(period = labels, value = values, index = idx,
                    percentage = idx * 100, variation_percent = (idx - 1) * 100)
  print(tab, row.names = FALSE)
  cat("  (index = value / value of the base period; the base period has index 1 = 100 %;\n",
      "   variation = (index - 1) * 100 = percentage of increase (+) or decrease (-))\n", sep = "")
  if (plot) {
    barplot(idx * 100, names.arg = labels, col = ifelse(seq_len(n) == pos, "firebrick", "steelblue"),
            ylab = "index (%)", main = paste0("Index of ", nm, ", base ", labels[pos]))
    abline(h = 100, lty = 2)
  }
  invisible(tab)
}

# =============================================================================
# 1. INDEX NUMBER PROPERTIES
# =============================================================================

# -----------------------------------------------------------------------------
# invert_index(index, percent = FALSE)
# -----------------------------------------------------------------------------
# WHAT IT DOES : CASE: "I know I(b/a); find I(a/b)" (change the base to the
#                other period). Property of inversion.
# FORMULA (for reference — matches the slides' notation):
#   X_{a/b} = 1 / X_{b/a}
# PARAMETERS   :
#   index   : the known index (1.35, or 135 if percent = TRUE).
#   percent : TRUE if you type it as a percentage.
# RETURNS      : The inverse index, in the same units you typed it.
# EXAMPLE      :
#   invert_index(1.35)             # 0.7407407...
#   invert_index(135, percent = TRUE)   # 74.07407...
# -----------------------------------------------------------------------------
invert_index <- function(index, percent = FALSE) {
  out <- if (percent) 100 * 100 / index else 1 / index
  report_title("INVERSE INDEX  X(a/b) = 1 / X(b/a)")
  report_line("Known index X(b/a)", index)
  report_line("Inverse index X(a/b)", out)
  invisible(out)
}

# -----------------------------------------------------------------------------
# chain_index(..., percent = FALSE)
# -----------------------------------------------------------------------------
# WHAT IT DOES : CASE: "I know I(a/b) and I(b/c); find I(a/c)" (change of base
#                / cyclic and modified cyclic properties). Multiplies the
#                indices. Put them in the order a/b, b/c, c/d...
#                To use an index in the other direction, apply invert_index()
#                first.
# FORMULA (for reference — matches the slides' notation):
#   X_{a/b} * X_{b/c} * ... * X_{y/z} = X_{a/z}
# PARAMETERS   :
#   ...     : the indices to chain (ratios, or percentages if percent = TRUE).
#   percent : TRUE if you type them as percentages (e.g. 1000 for 1000 %).
# RETURNS      : The chained index, in the same units you typed.
# EXAMPLE      :
#   chain_index(1.2, 1.5)                          # 1.8
#   chain_index(1000, 1/0.75*100, percent = TRUE)  # I(06/69) = 13.33 -> 1333.33 %
# -----------------------------------------------------------------------------
chain_index <- function(..., percent = FALSE) {
  v <- c(...)
  out <- if (percent) prod(v / 100) * 100 else prod(v)
  report_title("CHAINED INDEX  X(a/b) * X(b/c) * ... = X(a/z)")
  for (i in seq_along(v)) report_line(paste0("Index ", i), v[i])
  report_line("Chained index X(a/z)", out)
  invisible(out)
}

# -----------------------------------------------------------------------------
# deflate(nominal, cpi, labels = NULL, percent = FALSE)
# -----------------------------------------------------------------------------
# WHAT IT DOES : CASE: "Calculate the REAL value (constant money) of a nominal
#                amount", "convert to the money of the base year", "deflate
#                the series with the CPI". Divides the nominal value by the
#                price index I(t/t0): you get the equivalent amount in the
#                money of the base period t0. Works for one amount or for a
#                whole series.
# FORMULA (for reference — matches the slides' notation):
#   real value (in money of t0) = nominal value of t / CPI(t/t0)
# PARAMETERS   :
#   nominal : the nominal (current) value(s), in the money of period t.
#   cpi     : the price index I(t/t0) of that period WITH RESPECT TO the base
#             period t0 (one number, or one per value).
#             Example: "the CPI of 1998 with respect to 1990 is 135 %"
#             -> cpi = 1.35 (or 135 with percent = TRUE).
#             To bring a value of the BASE year to the money of year t (the
#             opposite direction) use cpi = invert_index(1.35).
#   labels  : (optional) names of the periods (years).
#   percent : TRUE if the CPI is typed as a percentage (135).
# RETURNS      : Invisibly, the real value(s).
# EXAMPLE      :
#   deflate(1000, 1.35)                     # 740.74: 1000 of 1998 = 740.74 of 1990
#   deflate(1000, 1/1.35)                   # 1350: 1000 of 1990 = 1350 of 1998
#   deflate(c(100, 110, 120), c(100, 104, 109), labels = 2020:2022, percent = TRUE)
# -----------------------------------------------------------------------------
deflate <- function(nominal, cpi, labels = NULL, percent = FALSE) {
  cpi_ratio <- if (percent) cpi / 100 else cpi
  real <- nominal / cpi_ratio
  report_title("DEFLATION: REAL VALUE = NOMINAL VALUE / CPI")
  if (length(nominal) == 1 && length(cpi_ratio) == 1) {
    report_line("Nominal value", nominal)
    report_line("CPI (as a ratio, CPI = 1 means no change in prices)", cpi_ratio)
    report_line("Real value (in money of the base period)", real)
    cat("  ->", if (cpi_ratio > 1) "prices rose, so the real value is SMALLER than the nominal one"
        else if (cpi_ratio < 1) "prices fell, so the real value is LARGER than the nominal one"
        else "no inflation", "\n")
  } else {
    if (is.null(labels)) labels <- seq_along(real)
    print(data.frame(period = labels, nominal = nominal, cpi = cpi_ratio, real = real), row.names = FALSE)
  }
  invisible(real)
}

# -----------------------------------------------------------------------------
# real_change(nominal_change, cpi_change, percent = TRUE)
# -----------------------------------------------------------------------------
# WHAT IT DOES : CASE: "The salary (or price) grew X % and the CPI grew Y % in
#                the same period: how much did it really change?". Gives the
#                real value of 100 units and the real percentage of change
#                (purchasing power).
# FORMULA (for reference — matches the slides' notation):
#   real value = (100 + nominal change) / CPI(t/t0)         CPI = 1 + CPI change
# PARAMETERS   :
#   nominal_change : growth of the salary / amount (50 means +50 %).
#   cpi_change     : growth of the CPI in the same period (100 means it doubled).
#   percent        : TRUE (default) if both are typed as percentages. FALSE if
#                    you type them as ratios (0.5 = +50 %).
# RETURNS      : Invisibly, the real change as a ratio (0.75 = -25 %).
# EXAMPLE      :
#   real_change(50, 100)     # salary +50 %, CPI doubled -> real 75 -> -25 %
# -----------------------------------------------------------------------------
real_change <- function(nominal_change, cpi_change, percent = TRUE) {
  nom <- if (percent) nominal_change / 100 else nominal_change
  cpi <- if (percent) cpi_change / 100 else cpi_change
  ratio <- (1 + nom) / (1 + cpi)
  report_title("REAL CHANGE (PURCHASING POWER)")
  report_line("Nominal value of 100 units at the end of the period", 100 * (1 + nom))
  report_line("CPI at the end with respect to the start", 1 + cpi)
  report_line("Real value of that amount in money of the start (100 -> ?)", 100 * ratio)
  report_line("Real change (%)", (ratio - 1) * 100)
  cat("  ->", if (ratio < 1) paste0("the purchasing power DECREASED by ", full_digits((1 - ratio) * 100), " %")
      else if (ratio > 1) paste0("the purchasing power INCREASED by ", full_digits((ratio - 1) * 100), " %")
      else "the purchasing power did not change", "\n")
  invisible(ratio)
}