# =============================================================================
#  CHAPTER 3 - TIME SERIES AND INDEX NUMBERS
#  One R function for every kind of exercise in the Chapter 3 slides.
#  (Department of Applied Mathematics, University of Malaga, 2024-2025)
# =============================================================================
#
#  HOW TO USE THIS FILE
#  ---------------------
#  1. Source Chapter 1 FIRST (this file reuses its printing helpers), then this
#     file (if you forget Chapter 1, it tries to find it in the usual folders):
#
#         source("chapter_1/formulas_chapter1.R")
#         source("chapter_3/formulas_chapter3.R")
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
#  "Find the components T, E, C and A" / "apply the multiplicative hypothesis"
#  (SHORT version: only the 4 components and X = T.E.C.A for each period)
#       time_series_components(tab)                       # or (x, period = 4, start = c(1980, 1))
#       time_series_components(tab, model = "additive")   # X = T+E+C+A
#       time_series_components(tab, forecast = 4)         # + prediction T*E
#
#  "Solve the exercise STEP BY STEP like in the slides" (T.C, E.A, indices,
#  corrected indices, A, deseasonalised, regression line, true trend, C, prediction)
#       time_series_steps(tab)                            # or (x, period = 4, start = c(1980, 1))
#       time_series_steps(tab, forecast = 4)              # + step 10: prediction
#       time_series_steps(tab, model = "additive")
#
#  WHICH `period` DO I USE?  (period = how many data there are in ONE cycle = columns of the table)
#       quarterly data  (trimestral, Q1..Q4)         period = 4
#       four-month data (cuatrimestral)              period = 3
#       half-year data  (semestral)                  period = 2
#       bimonthly data  (bimestral)                  period = 6
#       monthly data                                 period = 12
#       daily data, weekly cycle (Mon..Sun)          period = 7
#       hourly data, daily cycle                     period = 24
#       (a "trimestre" has 3 months -> 4 per year -> period 4;
#        a "cuatrimestre" has 4 months -> 3 per year -> period 3)
#       start = c(first year, first season), e.g. quarterly data from Q3 of 1990: start = c(1990, 3)
#
#  "Calculate the seasonal variation indices / corrected indices of the following
#  table E.A (X / T.C)"  -> the exercise GIVES the table E.A (not the series X)
#       Copy the table row by row between quotes, "-" where it has a dash:
#       seasonal_indices("- - 1.0778 0.9337
#                         1.1079 0.8764 1.1025 0.9246
#                         1.0906 0.8743 1.0932 0.9337
#                         1.1069 0.8932 1.0703 0.9573
#                         1.0559 0.8829 - -",
#                        season_names = c("Spring","Summer","Autumn","Winter"))
#       (period = 4 columns by default; monthly table: period = 12)
#
#  The same, but the data is a single list of numbers:
#       decompose_series(x, period = 4, start = c(1980, 1))
#       (period = 4 quarterly, 12 monthly;  start = c(first year, first season))
#
#  "Does the series have a cycle of period 3?" / "find the cycle"
#       autocorrelation(x, lag = 3)           # r between X(t) and X(t+3)
#       autocorrelation(x)                    # tries all the lags and finds the best
#       WHICH lag?  = the period of the cycle they ask about:
#         "period 3" -> 3 | quarterly yearly pattern -> 4 | monthly yearly -> 12
#         semester -> 2 | daily, weekly pattern -> 7 | hourly, daily pattern -> 24
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
#  "The CPI of 1998 with respect to 1990 is 135 %: real value in 1990 of 1000 of 1998, and in 1998 of 1000 of 1990"
#       cpi_conversion(cpi = 135, amount = 1000, year = 1998, base = 1990)   # BOTH directions
#       real_change(nominal_change = 50, cpi_change = 100)   # inputs in %
#       real_salary(salary_pct = 50, cpi_pct = 100)          # THE EASIEST: the whole exercise
#       real_salary(salary_pct = 50, cpi_pct = 100, years = 10)   # + average yearly change
#       real_salary(salary = c(1000,1200,1500), cpi = c(100,110,130), labels = 1970:1972)  # a table
#       (CPI "doubled" = 100, "tripled" = 200, "grew 30 %" = 30, "fell 10 %" = -10)
#
#  TYPICAL MISTAKES: write quarterly tables ROW BY ROW (byrow = TRUE) |
#  use the right `period` (4 quarters, 12 months) | in deflate(), the CPI is
#  I(t/base): to bring a value of the base year to year t use invert_index().
#
#  INDEX (in order of the slides)
#  -------------------------------
#  0. MAIN FUNCTIONS (one call = full answer)
#       moving_average, trend_line, decompose_series, time_series_components,
#       time_series_steps, seasonal_indices,
#       autocorrelation,
#       index_numbers, simple_index, deflate, real_change, real_salary, cpi_conversion
#  1. INDEX NUMBER PROPERTIES
#       invert_index, chain_index
# =============================================================================

if (!exists("report_title") || !exists("full_digits")) {
  ch1 <- Filter(file.exists, c("formulas_chapter1.R", "chapter_1/formulas_chapter1.R", "../chapter_1/formulas_chapter1.R"))
  if (length(ch1) > 0) {
    source(ch1[1])
  } else {
    stop("Chapter 3 needs Chapter 1's functions. Run this first:\n",
         '  source("formulas_chapter1.R")\n',
         "and only then source this Chapter 3 file.")
  }
}

# Box helpers (they live in formulas_chapter1.R; defined here too so this file never fails
# if an older Chapter 1 file is loaded).
if (!exists("answer_box")) answer_box <- function(lines) {
  w <- max(nchar(lines)) + 4
  bar <- strrep("*", w)
  cat("\n  ", bar, "\n", sep = "")
  for (l in lines) cat("  * ", formatC(l, width = -(w - 4), flag = "-"), " *\n", sep = "")
  cat("  ", bar, "\n\n", sep = "")
}
if (!exists("box_line")) box_line <- function(label, value) paste0(label, " = ", paste(full_digits(value), collapse = ", "))
if (!exists("final_box")) final_box <- function(title, lines) answer_box(c(paste0(">>> ", title), "", lines))
options(digits = 15)


# =============================================================================
# 0. MAIN FUNCTIONS (ONE CALL = FULL ANSWER)
# =============================================================================

# -----------------------------------------------------------------------------
# INTERNAL HELPERS (you do not call these yourself)
#   series_layout          : turns a vector (+ period, start) or a table
#                            (rows = years, columns = seasons) into the layout
#                            that decompose_series() works with.
#   as_table               : puts a vector in the years x seasons table.
#   show_table             : prints a table with "-" where there is no value.
# -----------------------------------------------------------------------------
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

# INTERNAL helpers to build the lines of the final boxes (answer_box / final_box / box_line
# live in formulas_chapter1.R).
box_trend <- function(a, b, r2 = NA, nm = "X", var = "t") c(
  paste0(nm, " = ", full_digits(a), signed_full_t(b), " * ", var),
  box_line("INTERCEPT a", a),
  box_line("SLOPE b", b),
  if (!is.na(r2)) box_line("R^2", r2),
  paste0("TREND: ", if (abs(b) < 1e-12) "FLAT" else if (b > 0) "GOES UP" else "GOES DOWN"))
box_seasons <- function(season_names, idx, mult, what = "E'") {
  unit <- if (mult) 100 else 1
  vapply(seq_along(idx), function(k) paste0(what, " ", season_names[k], " = ", full_digits(unname(idx[k]) * unit), if (mult) " %" else ""), character(1))
}
box_forecast <- function(f) {
  if (is.null(f)) return(NULL)
  c("", "PREDICTION:", vapply(seq_len(nrow(f)), function(i)
    paste0("  ", f$year[i], " ", f$season[i], " (t = ", f$t[i], ") = ", full_digits(f$prediction[i])), character(1)))
}

# INTERNAL: says in plain words if a trend line goes UP, DOWN or is FLAT, how much it
# changes, and how strong the line is (R^2). Used by every function that gives a trend.
describe_trend <- function(a, b, n, r2 = NA) {
  first <- a + b; last <- a + b * n
  dir <- if (abs(b) < 1e-12) "FLAT (horizontal): there is no trend, the trend is centred around a constant"
  else if (b > 0) "GOES UP  (increasing trend)" else "GOES DOWN  (decreasing trend)"
  cat("  TREND DIRECTION : ", dir, "\n", sep = "")
  if (abs(b) >= 1e-12) {
    cat("  CHANGE          : ", if (b > 0) "+" else "-", full_digits(abs(b)), " per period;  from t = 1 (", full_digits(first),
        ") to t = ", n, " (", full_digits(last), "): ", if (b > 0) "+" else "-", full_digits(abs(last - first)),
        if (first != 0) paste0("  (", full_digits(100 * (last - first) / abs(first)), " % of the first value)") else "", "\n", sep = "")
  }
  if (!is.na(r2)) {
    cat("  STRENGTH        : R^2 = ", full_digits(r2), "  -> ",
        if (r2 >= 0.7) "the line follows the data well: the direction is CLEAR"
        else if (r2 >= 0.3) "MODERATE: the direction is visible but the data move a lot around the line"
        else "WEAK: the line explains little, the trend is almost flat compared with the ups and downs of the data", "\n", sep = "")
  }
}

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
  if (k < 2) stop("The order k of the moving average must be 2 or more.")
  if (k >= length(x)) stop("The order k (", k, ") must be smaller than the number of data (", length(x), ").")
  w_ma <- if (k %% 2 == 1) rep(1, k) / k else c(0.5, rep(1, k - 1), 0.5) / k   # odd: plain mean; even: extremes x0.5
  half_ma <- length(w_ma) %/% 2
  ma <- rep(NA_real_, length(x))
  for (i_ma in (half_ma + 1):(length(x) - half_ma)) ma[i_ma] <- sum(w_ma * x[(i_ma - half_ma):(i_ma + half_ma)])
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
  ok_ma <- which(!is.na(ma))
  shown <- head(ok_ma, 24)
  final_box(paste0("MOVING AVERAGE OF ORDER ", k, ":  ", nm), c(
    vapply(shown, function(i) paste0("MA(", labels[i], ") = ", full_digits(ma[i])), character(1)),
    if (length(ok_ma) > length(shown)) paste0("... and ", length(ok_ma) - length(shown), " more (all of them are in the table above)"),
    "",
    paste0("Periods lost at each end: ", k %/% 2)))
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
  describe_trend(a, b, n, r^2)
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
  final_box(paste0("TREND LINE:  ", nm), c(
    box_trend(a, b, r^2, nm, "t"),
    box_line("r", r),
    box_line("MSE", mse),
    if (!is.null(fc)) c("", "PREDICTION:", paste0("  ", fc$period, " (t = ", fc$t, ") = ", vapply(fc$prediction, full_digits, character(1))))))
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
#   period       : only if x is a vector: HOW MANY DATA THERE ARE IN ONE SEASONAL CYCLE
#                  (usually in one year) = the number of columns of the table:
#                    quarterly data  (trimestral, Q1..Q4)        -> period = 4
#                    four-month data (cuatrimestral)             -> period = 3
#                    half-year data  (semestral)                 -> period = 2
#                    bimonthly data  (bimestral, every 2 months) -> period = 6
#                    monthly data                                -> period = 12
#                    daily data with a weekly cycle (Mon..Sun)   -> period = 7
#                    hourly data with a daily cycle              -> period = 24
#                  Careful: in Spanish a "trimestre" lasts 3 months, so a year
#                  has 4 of them (period 4); a "cuatrimestre" lasts 4 months, so
#                  a year has 3 of them (period 3). The period is NOT the length
#                  of each season: it is how many seasons fit in the cycle.
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
  if (p >= length(v)) stop("The order k (", p, ") must be smaller than the number of data (", length(v), ").")
  w_tc <- if (p %% 2 == 1) rep(1, p) / p else c(0.5, rep(1, p - 1), 0.5) / p   # odd: plain mean; even: extremes x0.5
  half_tc <- length(w_tc) %/% 2
  tc <- rep(NA_real_, length(v))
  for (i_tc in (half_tc + 1):(length(v) - half_tc)) tc[i_tc] <- sum(w_tc * v[(i_tc - half_tc):(i_tc + half_tc)])
  # tc = trend-cycle (centred moving average of order p)
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
  if (abs(sum(raw) * unit - (if (mult) p * 100 else 0)) < 1e-9) cat("  -> They already add up exactly, so the correction does not change them.\n")
  else cat("  -> They do not add up exactly, so they must be corrected.\n")
  
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
  describe_trend(a, b, n, r^2)
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
  
  final_box(paste0("FINAL RESULTS (", model, "):  ", nm), c(
    "CORRECTED SEASONAL INDICES:", box_seasons(L$season_names, idx, mult),
    "",
    "TRUE TREND (regression line of the deseasonalised series):",
    box_trend(a, b, r^2, "T", "t"),
    box_forecast(fc)))
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
# time_series_components(x, period = NULL, start = NULL, model = "multiplicative",
#                        season_names = NULL, forecast = 0, name = NULL, plot = TRUE)
# -----------------------------------------------------------------------------
# WHAT IT DOES : CASE: "Find the components T, E, C and A of the series" /
#                "apply the multiplicative (or additive) hypothesis". SHORT
#                version of decompose_series(): it gives ONLY the four
#                components and checks that they rebuild the series.
#                  T = trend            (line a + b*t of the deseasonalised series)
#                  E = seasonal index   (one per season, corrected)
#                  C = cycle            (C = TC / T,  TC = moving average of order = period)
#                  A = random component (A = X / (T * E * C))
#                Then it rebuilds X = T * E * C * A (multiplicative) or
#                X = T + E + C + A (additive) period by period.
#                The first and last period/2 periods have no C and no A (the
#                moving average is lost there).
# PARAMETERS   : the same as decompose_series().
#   period       : how many data there are in ONE seasonal cycle (see the list in
#                  decompose_series): quarterly = 4, four-month = 3, half-year = 2,
#                  bimonthly = 6, monthly = 12, daily with weekly cycle = 7.
#   x, period, start, season_names : the series (vector + period, or a table
#                                    with years as rows and seasons as columns).
#   model        : "multiplicative" (default) or "additive".
#   forecast     : (optional) number of future periods to predict. The
#                  prediction uses T and E only (C and A are taken as neutral).
# RETURNS      : Invisibly, a data.frame with t, X, TC, T, E, C, A and rebuilt X.
# EXAMPLE      :
#   time_series_components(x, period = 4, start = c(1980, 1))
#   time_series_components(tab, forecast = 4)
# -----------------------------------------------------------------------------
time_series_components <- function(x, period = NULL, start = NULL, model = "multiplicative",
                                   season_names = NULL, forecast = 0, name = NULL, plot = TRUE) {
  nm <- if (is.null(name)) variable_label(deparse(substitute(x)), "X") else name
  model <- match.arg(model, c("multiplicative", "additive"))
  mult <- model == "multiplicative"
  L <- series_layout(x, period, start, season_names)
  n <- L$n; p <- L$period; v <- L$values
  invisible(capture.output(res <- decompose_series(x, period = period, start = start, model = model,
                                                   season_names = season_names, forecast = forecast,
                                                   name = nm, plot = FALSE)))
  pick <- function(m) m[cbind(L$row, L$col)]
  tc  <- pick(res$trend_cycle)
  tr  <- pick(res$true_trend)
  E   <- unname(res$indices[L$season])
  C   <- if (mult) tc / tr else tc - tr
  A   <- if (mult) v / (tr * E * C) else v - tr - E - C
  rebuilt <- if (mult) tr * E * C * A else tr + E + C + A
  lab <- paste(L$year, L$season_names[L$season])
  out <- data.frame(period = lab, t = seq_len(n), X = v, TC = tc, T = tr, E = E, C = C, A = A,
                    rebuilt = rebuilt, stringsAsFactors = FALSE)
  
  bar <- strrep("=", 72)
  report_title(paste0("TIME SERIES COMPONENTS (", model, " hypothesis):  ", nm))
  cat("  ", if (mult) "X = T . E . C . A" else "X = T + E + C + A", "      (N = ", n, ", period = ", p, ")\n", sep = "")
  cat("\n  T  TREND       T = a + b * t  (regression line of the deseasonalised series)\n")
  cat("     T = ", full_digits(res$a), signed_full_t(res$b), " * t\n", sep = "")
  describe_trend(res$a, res$b, n, res$r^2)
  cat("  E  SEASONAL    one corrected index per season",
      if (mult) " (multiplicative: they average 1)" else " (additive: they add up to 0)", "\n", sep = "")
  for (s in seq_len(p))
    cat(sprintf("       %-8s E = %s\n", L$season_names[s], full_digits(unname(res$indices[s]))))
  cat("  C  CYCLE       C = ", if (mult) "TC / T" else "TC - T",
      "   where TC = moving average of order ", p, "\n", sep = "")
  cat("  A  RANDOM      A = ", if (mult) "X / (T * E * C)" else "X - T - E - C", "\n", sep = "")
  
  cat("\n", strrep("-", 72), "\n COMPONENTS OF EACH PERIOD\n", strrep("-", 72), "\n", sep = "")
  fmt <- function(z) ifelse(is.na(z), "-", vapply(z, full_digits, character(1)))
  m <- cbind(X = fmt(out$X), T = fmt(out$T), E = fmt(out$E), C = fmt(out$C), A = fmt(out$A),
             rebuilt = fmt(out$rebuilt))
  rownames(m) <- lab
  colnames(m)[6] <- if (mult) "T*E*C*A" else "T+E+C+A"
  print(m, quote = FALSE, right = TRUE)
  ok <- !is.na(rebuilt)
  cat("\n  Check: ", if (mult) "T*E*C*A" else "T+E+C+A", " rebuilds X in the ", sum(ok),
      " periods where C and A exist (largest difference = ",
      full_digits(max(abs(rebuilt[ok] - v[ok]))), ").\n", sep = "")
  cat("  The other ", sum(!ok), " periods (the first and last ", p %/% 2,
      ") have no C and no A, because the moving average is lost there.\n", sep = "")
  
  if (!is.null(res$forecast)) {
    cat("\n", strrep("-", 72), "\n PREDICTION  (", if (mult) "T * E" else "T + E",
        ": C and A are taken as neutral)\n", strrep("-", 72), "\n", sep = "")
    f <- res$forecast
    pm <- cbind(T = fmt(f$trend), E = fmt(f$seasonal_index), prediction = fmt(f$prediction))
    rownames(pm) <- paste(f$year, f$season)
    print(pm, quote = FALSE, right = TRUE)
  }
  
  final_box(paste0("COMPONENTS ", if (mult) "T . E . C . A" else "T + E + C + A", ":  ", nm), c(
    "T  TREND:", box_trend(res$a, res$b, res$r^2, "T", "t"),
    "",
    "E  SEASONAL INDICES (corrected):", box_seasons(L$season_names, res$indices, mult, "E"),
    "",
    paste0("C  CYCLE:  C = ", if (mult) "TC / T" else "TC - T", "   (see the table of each period)"),
    paste0("A  RANDOM: A = ", if (mult) "X / (T * E * C)" else "X - T - E - C", "   (see the table of each period)"),
    box_forecast(res$forecast)))
  if (plot) {
    xr <- c(1, n + max(forecast, 0))
    plot(out$t, out$X, type = "b", pch = 19, col = "steelblue", xlim = xr, xlab = "t", ylab = nm,
         main = "Series, trend-cycle and trend")
    lines(out$t, out$TC, col = "darkgreen", lwd = 2)
    tt <- seq(xr[1], xr[2]); lines(tt, res$a + res$b * tt, col = "firebrick", lwd = 2)
    legend("topleft", legend = c(nm, "trend-cycle TC", "trend T"),
           col = c("steelblue", "darkgreen", "firebrick"), lty = 1, bty = "n", cex = 0.8)
  }
  invisible(out)
}

# -----------------------------------------------------------------------------
# time_series_steps(x, period = NULL, start = NULL, model = "multiplicative",
#                   season_names = NULL, forecast = 0, name = NULL, plot = TRUE)
# -----------------------------------------------------------------------------
# WHAT IT DOES : CASE: "solve the whole exercise like in the slides". It does
#                the complete decomposition STEP BY STEP, in the same order as
#                the slides of Chapter 3. Each step says WHAT is done, the
#                FORMULA, a WORKED EXAMPLE with your first numbers, and the
#                RESULT table:
#                  STEP 1  Trend-cycle T.C        (centred moving average, order = period)
#                  STEP 2  Remove T.C             E.A = X / (T.C)
#                  STEP 3  Seasonal indices       (mean of each season) + interpretation
#                  STEP 4  Corrected indices      (normalised so they add up to 400 %)
#                  STEP 5  Random component A     A = (E.A) / corrected index
#                  STEP 6  Deseasonalised series  X / E
#                  STEP 7  Regression line        deseasonalised X = a + b * t
#                  STEP 8  True trend T           T = a + b * t of every period
#                  STEP 9  Cycle C and check      C = TC / T,  X = T.E.C.A
#                  STEP 10 Prediction (if forecast > 0)   trend x corrected index
#                For the SHORT answers use time_series_components() (only
#                T, E, C, A) or decompose_series() (all the tables, compact).
# PARAMETERS   : the same as decompose_series().
#   period       : how many data there are in ONE seasonal cycle (see the list in
#                  decompose_series): quarterly = 4, four-month = 3, half-year = 2,
#                  bimonthly = 6, monthly = 12, daily with weekly cycle = 7.
#   x, period, start, season_names : the series (vector + period, or a table
#                                    with years as rows and seasons as columns).
#   model        : "multiplicative" (default) or "additive".
#   forecast     : (optional) number of future periods to predict.
#   plot         : FALSE to skip the plots.
# RETURNS      : Invisibly, the same list as decompose_series().
# EXAMPLE      :
#   time_series_steps(x, period = 4, start = c(1980, 1), forecast = 4)
#   time_series_steps(tab)                       # table years x seasons
# -----------------------------------------------------------------------------
time_series_steps <- function(x, period = NULL, start = NULL, model = "multiplicative",
                              season_names = NULL, forecast = 0, name = NULL, plot = TRUE) {
  nm <- if (is.null(name)) variable_label(deparse(substitute(x)), "X") else name
  model <- match.arg(model, c("multiplicative", "additive"))
  mult <- model == "multiplicative"
  old_width <- options(width = max(getOption("width"), 150)); on.exit(options(old_width), add = TRUE)  # tables do not wrap
  L <- series_layout(x, period, start, season_names)
  n <- L$n; p <- L$period; v <- L$values
  invisible(capture.output(res <- decompose_series(x, period = period, start = start, model = model,
                                                   season_names = season_names, forecast = forecast,
                                                   name = nm, plot = plot)))
  pick <- function(m) m[cbind(L$row, L$col)]
  tc <- pick(res$trend_cycle); ea <- pick(res$ratio); des <- pick(res$deseasonalised)
  tr <- pick(res$true_trend);  ac <- pick(res$random)
  idx <- res$indices; raw <- res$raw_indices
  sn <- L$season_names
  fm <- function(z) paste(vapply(z, full_digits, character(1)), collapse = ", ")
  op <- if (mult) "/" else "-"
  unit <- if (mult) 100 else 1
  step <- function(num, title, what, formula = NULL) {
    cat("\n", strrep("=", 72), "\n STEP ", num, ":  ", title, "\n", strrep("=", 72), "\n", sep = "")
    cat("  WHAT WE DO : ", what, "\n", sep = "")
    if (!is.null(formula)) cat("  FORMULA    : ", formula, "\n", sep = "")
  }
  
  report_title(paste0("TIME SERIES, STEP BY STEP (", model, "):  ", nm))
  cat("  Model: ", if (mult) "X = T . E . C . A" else "X = T + E + C + A",
      "     T = trend, E = seasonal, C = cycle, A = random\n", sep = "")
  report_line("Number of observations (N)", n)
  report_line("Period of the seasonality", p)
  cat("  The data, one row per year and one column per season:\n")
  show_table(as_table(v, L))
  
  # STEP 1
  step(1, paste0("T.C = MOVING AVERAGE of order ", p, " of X     [trend-cycle]"),
       paste0("Smooth the series with a centred moving average of order ", p,
              " (the period of the seasonality), which removes E and A."),
       if (p %% 2 == 0) paste0("TC_t = (0.5 X_(t-", p %/% 2, ") + X_(t-", p %/% 2 - 1, ") + ... + X_(t+",
                               p %/% 2 - 1, ") + 0.5 X_(t+", p %/% 2, ")) / ", p, "     (extremes x 0.5)")
       else paste0("TC_t = (X_(t-", (p - 1) %/% 2, ") + ... + X_(t+", (p - 1) %/% 2, ")) / ", p))
  i0 <- which(!is.na(tc))[1]; h <- p %/% 2
  if (p %% 2 == 0) {
    terms <- c(paste0("0.5*", full_digits(v[i0 - h])), vapply(v[(i0 - h + 1):(i0 + h - 1)], full_digits, character(1)),
               paste0("0.5*", full_digits(v[i0 + h])))
  } else terms <- vapply(v[(i0 - h):(i0 + h)], full_digits, character(1))
  cat("  EXAMPLE    : first value (", L$year[i0], " ", sn[L$season[i0]], "):\n",
      "               (", paste(terms, collapse = " + "), ") / ", p, " = ", full_digits(tc[i0]), "\n", sep = "")
  cat("  >>> ANSWER  T.C =\n"); show_table(as_table(tc, L))
  cat("  (The first and last ", h, " periods are lost: there are not enough data around them.)\n", sep = "")
  tcv <- tc[!is.na(tc)]
  cat("  TREND-CYCLE DIRECTION : from the first (", full_digits(tcv[1]), ") to the last (", full_digits(tcv[length(tcv)]),
      ") value of T.C it ", if (tcv[length(tcv)] > tcv[1]) "GOES UP" else if (tcv[length(tcv)] < tcv[1]) "GOES DOWN" else "stays FLAT",
      " (", if (tcv[length(tcv)] >= tcv[1]) "+" else "-", full_digits(abs(tcv[length(tcv)] - tcv[1])), ")\n", sep = "")
  
  # STEP 2
  step(2, paste0("E.A = ", if (mult) "X / (T.C)" else "X - (T.C)", "     [remove the trend-cycle]"),
       paste0(if (mult) "Divide the series by T.C (X / T.C)" else "Subtract T.C from the series (X - T.C)", ": only the seasonal and random components remain."),
       if (mult) "X / (T.C) = (T.E.C.A) / (T.C) = E.A" else "X - (T+C) = E + A")
  cat("  EXAMPLE    : ", L$year[i0], " ", sn[L$season[i0]], ":  ", full_digits(v[i0]), " ", op, " ",
      full_digits(tc[i0]), " = ", full_digits(ea[i0]), "\n", sep = "")
  cat("  >>> ANSWER  E.A =\n"); show_table(as_table(ea, L))
  
  # STEP 3
  step(3, "E (uncorrected) = MEAN of E.A in each season     [seasonal indices]",
       "Average each season over the years: this removes the random component A.",
       "I_season = mean of the E.A values of that season")
  s1 <- ea[L$season == 1 & !is.na(ea)]
  cat("  EXAMPLE    : ", sn[1], ":  (", paste(vapply(s1, full_digits, character(1)), collapse = " + "), ") / ",
      length(s1), " = ", full_digits(raw[1]), if (mult) paste0("  =>  ", full_digits(raw[1] * 100), " %") else "", "\n", sep = "")
  cat("  TABLE      : the table E.A of step 2 with the mean of each column added at the bottom:\n")
  m3 <- rbind(as_table(ea, L), `MEAN=index` = raw)
  show_table(m3)
  cat("  The indices", if (mult) " (in %)" else "", ":\n", sep = "")
  for (k in seq_len(p)) cat(sprintf("     %-10s %s%s\n", sn[k], full_digits(raw[k] * unit), if (mult) " %" else ""))
  report_line(paste0("Sum of the indices (should be ", if (mult) paste0(p * 100, " %") else "0", ")"), sum(raw) * unit)
  if (abs(sum(raw) * unit - (if (mult) p * 100 else 0)) < 1e-9) {
    cat("  -> The indices already add up exactly: step 4 will not change them (correction factor 1).\n")
  } else cat("  -> The indices do not add up exactly, so they must be CORRECTED (step 4).\n")
  cat("  >>> ANSWER  E (uncorrected seasonal indices) =\n")
  for (k in seq_len(p)) cat(sprintf("        %-10s %s%s\n", sn[k], full_digits(raw[k]),
                                    if (mult) paste0("   (", full_digits(raw[k] * 100), " %)") else ""))
  
  # STEP 4
  step(4, paste0("E' = ", if (mult) paste0(p * 100, " / (sum of E in %) * E") else "E - mean(E)", "     [corrected / normalised indices]"),
       paste0("Adjust the indices so that they add up exactly to ", if (mult) paste0(p * 100, " %") else "0", "."),
       if (mult) paste0("I' = ", p * 100, " / (sum of the indices in %) * I") else "I' = I - mean(I)")
  if (mult) {
    cat("  EXAMPLE    : ", sn[1], ":  ", p * 100, " / ", full_digits(sum(raw) * 100), " * ", full_digits(raw[1] * 100),
        " = ", full_digits(idx[1] * 100), " %\n", sep = "")
  } else cat("  EXAMPLE    : ", sn[1], ":  ", full_digits(raw[1]), " - (", full_digits(mean(raw)), ") = ",
             full_digits(idx[1]), "\n", sep = "")
  if (mult) cat("  CORRECTION FACTOR : ", p * 100, " / ", full_digits(sum(raw) * 100), " = ", full_digits(p * 100 / (sum(raw) * 100)),
                if (abs(sum(raw) * 100 - p * 100) < 1e-9) "   (the indices already added up: nothing changes)" else "", "\n", sep = "")
  cat("  TABLE      : the indices before and after the correction", if (mult) " (in %)" else "", ":\n", sep = "")
  m4 <- rbind(`uncorrected (step 3)` = raw * unit, `CORRECTED I'` = idx * unit)
  colnames(m4) <- sn
  print(m4)
  report_line("Sum of the corrected indices", sum(idx) * unit)
  cat("  NORMALISED TABLE  (the corrected index", if (mult) " in %" else "", " placed in every period; it is the E used in steps 5 and 6):\n", sep = "")
  show_table(as_table(unname(idx[L$season]) * unit, L))
  cat("  INTERPRETATION:\n")
  for (k in seq_len(p)) {
    if (mult) {
      d <- idx[k] * 100 - 100
      cat(sprintf("     %-10s %s %% %s than the annual average\n", sn[k], full_digits(abs(d)), if (d >= 0) "HIGHER" else "LOWER"))
    } else cat(sprintf("     %-10s %s %s the annual average\n", sn[k], full_digits(abs(idx[k])), if (idx[k] >= 0) "ABOVE" else "BELOW"))
  }
  cat("  >>> ANSWER  E' (corrected seasonal indices) =\n")
  for (k in seq_len(p)) cat(sprintf("        %-10s %s%s\n", sn[k], full_digits(idx[k]),
                                    if (mult) paste0("   (", full_digits(idx[k] * 100), " %)") else ""))
  
  # STEP 5
  step(5, paste0("A = ", if (mult) "(E.A) / E'" else "(E+A) - E'", "     [random component]"),
       paste0("Remove the corrected seasonal index from E.A: what is left is A (it should be close to ", if (mult) "1" else "0", ")."),
       if (mult) "A = (E.A) / I'" else "A = (E+A) - I'")
  cat("  EXAMPLE    : ", L$year[i0], " ", sn[L$season[i0]], ":  ", full_digits(ea[i0]), " ", op, " ",
      full_digits(idx[L$season[i0]]), " = ", full_digits(ac[i0]), "\n", sep = "")
  cat("  >>> ANSWER  A =\n"); show_table(as_table(ac, L))
  
  # STEP 6
  step(6, paste0(if (mult) "X / E' = T.C.A" else "X - E' = T+C+A", "     [deseasonalised series]"),
       paste0("Remove the seasonal component from the ORIGINAL series (", if (mult) "divide" else "subtract", " by I')."),
       if (mult) "X / E  = T.C.A" else "X - E = T+C+A")
  cat("  EXAMPLE    : ", L$year[1], " ", sn[L$season[1]], ":  ", full_digits(v[1]), " ", op, " ",
      full_digits(idx[L$season[1]]), " = ", full_digits(des[1]), "\n", sep = "")
  cat("  >>> ANSWER  X", op, "E' =\n", sep = ""); show_table(as_table(des, L))
  
  # STEP 7
  tt <- seq_len(n)
  step(7, paste0(if (mult) "X / E'" else "X - E'", " = a + b*t     [regression line of the deseasonalised series]"),
       paste0("Number the periods t = 1, ..., ", n, " and fit the least-squares line to the deseasonalised series."),
       "deseasonalised X = a + b * t,     b = Cov(t, X/E) / Var(t),     a = mean(X/E) - b * mean(t)")
  vt <- mean(tt^2) - mean(tt)^2; cvt <- mean(tt * des) - mean(tt) * mean(des)
  cat("  NUMBERS    : mean(t) = ", full_digits(mean(tt)), ",  mean(X/E) = ", full_digits(mean(des)),
      ",  Var(t) = ", full_digits(vt), ",  Cov(t, X/E) = ", full_digits(cvt), "\n", sep = "")
  cat("               SLOPE b = ", full_digits(cvt), " / ", full_digits(vt), " = ", full_digits(res$b), "\n", sep = "")
  cat("               INTERCEPT a = ", full_digits(mean(des)), " - (", full_digits(res$b), ") * ", full_digits(mean(tt)),
      " = ", full_digits(res$a), "\n", sep = "")
  answer_box(c(paste0(">>> ANSWER  STEP 7:  REGRESSION LINE"),
               "",
               paste0("X", op, "E' = ", full_digits(res$a), signed_full_t(res$b), " * t"),
               "",
               paste0("INTERCEPT  a = ", full_digits(res$a), "   (value of the line at t = 0)"),
               paste0("SLOPE      b = ", full_digits(res$b), "   (change per period)")))
  report_line("INTERCEPT  a", res$a)
  report_line("SLOPE      b", res$b)
  report_line("r (correlation of t and X/E)", res$r)
  report_line("R^2 = r^2", res$r^2)
  describe_trend(res$a, res$b, n, res$r^2)
  
  # STEP 8
  step(8, "T = a + b*t     [true (corrected) trend]",
       "Evaluate the regression line at the t of every period.", "T_t = a + b * t")
  cat("  EXAMPLE    : t = 1 (", L$year[1], " ", sn[L$season[1]], "):  ", full_digits(res$a), signed_full_t(res$b),
      " * 1 = ", full_digits(tr[1]), "\n", sep = "")
  cat("  >>> ANSWER  T =\n"); show_table(as_table(tr, L))
  describe_trend(res$a, res$b, n, res$r^2)
  
  # STEP 9
  cyc <- if (mult) tc / tr else tc - tr
  rebuilt <- if (mult) tr * idx[L$season] * cyc * ac else tr + idx[L$season] + cyc + ac
  step(9, paste0("C = ", if (mult) "(T.C) / T" else "(T.C) - T", ";   X = ", if (mult) "T.E.C.A" else "T+E+C+A", "     [cycle and check]"),
       "The trend-cycle T.C contains the trend T and the cycle C: separate them, then rebuild X.",
       if (mult) "C = (T.C) / T,     X = T . E . C . A" else "C = (T.C) - T,     X = T + E + C + A")
  cat("  EXAMPLE    : ", L$year[i0], " ", sn[L$season[i0]], ":  C = ", full_digits(tc[i0]), " ", op, " ",
      full_digits(tr[i0]), " = ", full_digits(cyc[i0]), "\n", sep = "")
  cat("               ", if (mult) "T.E.C.A" else "T+E+C+A", " = ", full_digits(tr[i0]), if (mult) " * " else " + ",
      full_digits(idx[L$season[i0]]), if (mult) " * " else " + ", full_digits(cyc[i0]), if (mult) " * " else " + ",
      full_digits(ac[i0]), " = ", full_digits(rebuilt[i0]), "   (X = ", full_digits(v[i0]), ")\n", sep = "")
  cat("  >>> ANSWER  C =\n"); show_table(as_table(cyc, L))
  ok <- !is.na(rebuilt)
  cat("  CHECK      : ", if (mult) "T.E.C.A" else "T+E+C+A", " gives back X in the ", sum(ok),
      " periods where C and A exist (largest difference ", full_digits(max(abs(rebuilt[ok] - v[ok]))), ").\n", sep = "")
  
  # STEP 10
  if (!is.null(res$forecast)) {
    f <- res$forecast
    step(10, paste0("X_t = (a + b*t) ", if (mult) "*" else "+", " E'     [prediction]"),
         paste0("Predict the next periods: trend of the future period ", if (mult) "x" else "+", " corrected seasonal index."),
         paste0("X_t = (a + b * t) ", if (mult) "*" else "+", " I'"))
    f1 <- f[1, ]
    cat("  EXAMPLE    : ", f1$year, " ", f1$season, " (t = ", f1$t, "):  (", full_digits(res$a), signed_full_t(res$b),
        " * ", f1$t, ") ", if (mult) "*" else "+", " ", full_digits(f1$seasonal_index), " = ",
        full_digits(f1$trend), if (mult) " * " else " + ", full_digits(f1$seasonal_index), " = ", full_digits(f1$prediction), "\n", sep = "")
    cat("  >>> ANSWER  X_t (prediction) =\n"); print(f, row.names = FALSE)
  }
  
  # ---------------------------------------------------------------- SUMMARY
  cat("\n", strrep("#", 72), "\n  FINAL SUMMARY OF ALL THE VALUES  (in the order they were calculated)\n",
      strrep("#", 72), "\n", sep = "")
  cat("\n  A) PER SEASON  (steps 3 and 4)\n")
  sm <- cbind(uncorrected = vapply(raw * unit, full_digits, character(1)),
              corrected = vapply(idx * unit, full_digits, character(1)),
              meaning = vapply(seq_len(p), function(k) {
                d <- if (mult) idx[k] * 100 - 100 else idx[k]
                paste0(full_digits(abs(d)), if (mult) " % " else " ", if (d >= 0) "above" else "below", " the average")
              }, character(1)))
  rownames(sm) <- sn
  colnames(sm)[1:2] <- paste0(colnames(sm)[1:2], if (mult) " (%)" else "")
  print(sm, quote = FALSE, right = TRUE)
  cat("  sum of the uncorrected = ", full_digits(sum(raw) * unit), "   sum of the corrected = ",
      full_digits(sum(idx) * unit), "\n", sep = "")
  
  cat("\n  B) TREND LINE  (step 7)\n")
  cat("     X = a + b * t  =  ", full_digits(res$a), signed_full_t(res$b), " * t\n", sep = "")
  kv_t <- function(k, val, note) cat(sprintf("     %-4s = %-22s %s\n", k, full_digits(val), note))
  kv_t("a", res$a, "INTERCEPT of the trend line (its value at t = 0)")
  kv_t("b", res$b, paste0("slope: the trend ", if (res$b > 0) "grows" else "falls", " by this much each period"))
  kv_t("r", res$r, "correlation of t and the deseasonalised series")
  kv_t("R^2", res$r^2, "share of the deseasonalised series explained by the line")
  
  cat("\n  C) PER PERIOD, in the order of the steps  ('-' = not available: the moving average is lost)\n")
  fmt <- function(z) ifelse(is.na(z), "-", vapply(z, full_digits, character(1)))
  lab <- paste(L$year, sn[L$season])
  Ev <- unname(idx[L$season])
  blk1 <- cbind(X = fmt(v), `TC (1)` = fmt(tc), `E.A (2)` = fmt(ea), `E (4)` = fmt(Ev))
  blk2 <- cbind(`A (5)` = fmt(ac), `X/E (6)` = fmt(des), `T (8)` = fmt(tr), `C (9)` = fmt(cyc))
  rownames(blk1) <- rownames(blk2) <- lab
  cat("\n     Original series and steps 1 to 4:\n"); print(blk1, quote = FALSE, right = TRUE)
  cat("\n     Steps 5 to 9:\n"); print(blk2, quote = FALSE, right = TRUE)
  cat("\n     Check ", if (mult) "T * E * C * A" else "T + E + C + A", " = X:\n", sep = "")
  chk <- cbind(rebuilt = fmt(rebuilt), X = fmt(v)); rownames(chk) <- lab
  print(chk, quote = FALSE, right = TRUE)
  if (!is.null(res$forecast)) {
    cat("\n  D) PREDICTION  (step 10)\n")
    f <- res$forecast
    pm <- cbind(t = f$t, trend = fmt(f$trend), index = fmt(f$seasonal_index), prediction = fmt(f$prediction))
    rownames(pm) <- paste(f$year, f$season)
    print(pm, quote = FALSE, right = TRUE)
  }
  cat("\n", strrep("=", 72), "\n", sep = "")
  final_box(paste0("FINAL ANSWER (", model, "):  ", nm), c(
    paste0("STEP 4  Corrected seasonal indices E':"), box_seasons(sn, idx, mult),
    "",
    "STEP 7/8  Trend line (regression of the deseasonalised series):",
    box_trend(res$a, res$b, res$r^2, "T", "t"),
    box_forecast(res$forecast)))
  invisible(res)
}

# -----------------------------------------------------------------------------
# seasonal_indices(ea, period = 4, model = "multiplicative", season_names = NULL)
# -----------------------------------------------------------------------------
# WHAT IT DOES : CASE: "Calculate the seasonal variation indices and the corrected
#                seasonal indices of the following table E.A (= X / T.C)" and
#                "interpret the results". Use it when the exercise GIVES you the
#                table E.A (not the original series). It does steps 3 and 4 of
#                the slides: the mean of each season (uncorrected indices, in %),
#                their sum, the correction to 400 % and the interpretation.
#                (If you have the series X itself, use time_series_steps().)
# HOW TO TYPE THE TABLE (the easiest way): copy the numbers of the table ROW BY
#                ROW, in quotes, and write "-" where the table has a dash. No
#                matrix, no NA, no byrow:
#                    seasonal_indices("- - 1.0778 0.9337
#                                      1.1079 0.8764 1.1025 0.9246
#                                      1.0906 0.8743 1.0932 0.9337
#                                      1.1069 0.8932 1.0703 0.9573
#                                      1.0559 0.8829 - -")
#                You can also give a normal vector c(NA, NA, 1.0778, ...) or a matrix.
# PARAMETERS   :
#   ea           : the table E.A: a text with the numbers row by row (see above),
#                  or a vector (row by row, NA or "-" for the dashes), or a matrix.
#   period       : seasons per cycle = columns of the table. Default 4 (quarterly);
#                  monthly = 12, four-month = 3 (ignored if ea is a matrix).
#   model        : "multiplicative" (default, indices in %, they must add up to
#                  period x 100) or "additive" (E+A table, they must add up to 0).
#   season_names : (optional) names of the seasons, e.g. c("Spring","Summer",
#                  "Autumn","Winter"). Default Q1..Q4 (or the column names).
# RETURNS      : Invisibly, a list with `uncorrected` and `corrected` indices.
# EXAMPLE      :
#   seasonal_indices("- - 1.0778 0.9337
#                     1.1079 0.8764 1.1025 0.9246
#                     1.0906 0.8743 1.0932 0.9337
#                     1.1069 0.8932 1.0703 0.9573
#                     1.0559 0.8829 - -",
#                    season_names = c("Spring","Summer","Autumn","Winter"))
# -----------------------------------------------------------------------------
seasonal_indices <- function(ea, period = 4, model = "multiplicative", season_names = NULL) {
  model <- match.arg(model, c("multiplicative", "additive"))
  mult <- model == "multiplicative"
  if (is.matrix(ea) || is.data.frame(ea)) {
    m <- as.matrix(ea)
  } else {
    if (is.character(ea)) {
      txt <- paste(ea, collapse = " ")
      txt <- iconv(txt, "UTF-8", "ASCII", sub = "-")              # long dashes (copied from a PDF) -> "-"
      txt <- gsub(",(\\s|$)", " ", txt)                         # "1.0778, 0.9" -> separator
      txt <- gsub("(?<=\\d),(?=\\d)", ".", txt, perl = TRUE)      # 1,0778 -> 1.0778
      tok <- strsplit(trimws(txt), "[[:space:];]+")[[1]]
      tok[grepl("^-+$", tok) | tok %in% c("NA", "na", "x", "X", "")] <- NA   # a dash = missing value
      v <- suppressWarnings(as.numeric(tok))
      if (any(is.na(v) & !is.na(tok))) stop("I could not read these values as numbers: ", paste(tok[is.na(v) & !is.na(tok)], collapse = ", "))
    } else v <- as.numeric(ea)
    if (length(v) %% period != 0)
      stop("The table has ", length(v), " values, which does not fill whole rows of ", period,
           " columns. Check the values or the `period` (columns of the table).")
    m <- matrix(v, ncol = period, byrow = TRUE)
  }
  p <- ncol(m)
  if (is.null(season_names)) season_names <- if (!is.null(colnames(m))) colnames(m) else default_season_names(p)
  if (length(season_names) != p) stop("`season_names` must have ", p, " names.")
  colnames(m) <- season_names
  if (is.null(rownames(m))) rownames(m) <- paste0("year ", seq_len(nrow(m)))
  unit <- if (mult) 100 else 1
  old_width <- options(width = max(getOption("width"), 150)); on.exit(options(old_width), add = TRUE)
  
  raw <- colMeans(m, na.rm = TRUE)
  idx <- if (mult) raw * p / sum(raw) else raw - mean(raw)
  target <- if (mult) p * 100 else 0
  tick <- function(z) paste(vapply(z, full_digits, character(1)), collapse = " + ")
  
  report_title(paste0("SEASONAL INDICES FROM THE TABLE ", if (mult) "E.A" else "E+A", "  (", model, ")"))
  cat("  The table E.A  (X / T.C):\n"); show_table(m)
  
  cat("\n", strrep("=", 72), "\n E (uncorrected) = MEAN of ", if (mult) "E.A" else "E+A",
      " in each season     [seasonal variation indices]\n", strrep("=", 72), "\n", sep = "")
  cat("  WHAT WE DO : Average each season over the years: this eliminates the random component A.\n")
  cat("  FORMULA    : I_season = mean of the values of that season\n")
  for (k in seq_len(p)) {
    v <- m[!is.na(m[, k]), k]
    cat("  ", sprintf("%-10s", season_names[k]), " = (", tick(v), ") / ", length(v), " = ", full_digits(raw[k]),
        if (mult) paste0("   =>  ", full_digits(raw[k] * 100), " %") else "", "\n", sep = "")
  }
  cat("  >>> ANSWER  E (uncorrected) =\n")
  for (k in seq_len(p)) cat(sprintf("        %-10s %s%s\n", season_names[k], full_digits(raw[k]),
                                    if (mult) paste0("   (", full_digits(raw[k] * 100), " %)") else ""))
  cat("  Sum of the indices: ", full_digits(sum(raw) * unit), if (mult) " %" else "", "   (they should add up to ",
      full_digits(target), if (mult) " %" else "", ")\n", sep = "")
  if (abs(sum(raw) * unit - target) < 1e-9) {
    cat("  -> They already add up exactly: the correction does not change them.\n")
  } else cat("  -> They do NOT add up exactly, so they must be CORRECTED (normalised).\n")
  
  cat("\n", strrep("=", 72), "\n E' = ", if (mult) paste0(p * 100, " / (sum of E in %) * E") else "E - mean(E)",
      "     [corrected / normalised indices]\n", strrep("=", 72), "\n", sep = "")
  if (mult) {
    cat("  FORMULA    : I' = ", p * 100, " / (sum of the indices in %) * I\n", sep = "")
    cat("  CORRECTION FACTOR : ", p * 100, " / ", full_digits(sum(raw) * 100), " = ",
        full_digits(p * 100 / (sum(raw) * 100)), "\n", sep = "")
    for (k in seq_len(p)) cat("  ", sprintf("%-10s", season_names[k]), " = ", full_digits(p * 100 / (sum(raw) * 100)), " * ",
                              full_digits(raw[k] * 100), " = ", full_digits(idx[k] * 100), " %\n", sep = "")
  } else {
    cat("  FORMULA    : I' = I - mean(I)      mean(I) = ", full_digits(mean(raw)), "\n", sep = "")
    for (k in seq_len(p)) cat("  ", sprintf("%-10s", season_names[k]), " = ", full_digits(raw[k]), " - (", full_digits(mean(raw)), ") = ",
                              full_digits(idx[k]), "\n", sep = "")
  }
  cat("  >>> ANSWER  E' (corrected seasonal indices) =\n")
  for (k in seq_len(p)) cat(sprintf("        %-10s %s%s\n", season_names[k], full_digits(idx[k]),
                                    if (mult) paste0("   (", full_digits(idx[k] * 100), " %)") else ""))
  cat("  Sum of the corrected indices: ", full_digits(sum(idx) * unit), if (mult) " %" else "", "\n", sep = "")
  cat("  NOTE: if the table E.A you typed is rounded (e.g. 4 decimals), the 3rd-4th decimal of these indices can\n",
      "  differ a little from the exercise solution, which was computed with the unrounded table.\n", sep = "")
  cat("\n  INTERPRETATION (what each season means compared with the annual average):\n")
  for (k in seq_len(p)) {
    if (mult) {
      d <- idx[k] * 100 - 100
      cat(sprintf("     %-10s %s %% %s than the annual average\n", season_names[k], full_digits(abs(d)), if (d >= 0) "HIGHER" else "LOWER"))
    } else cat(sprintf("     %-10s %s %s the annual average\n", season_names[k], full_digits(abs(idx[k])), if (idx[k] >= 0) "ABOVE" else "BELOW"))
  }
  cat("\n", strrep("=", 72), "\n", sep = "")
  final_box(paste0("SEASONAL INDICES (", model, ")"), c(
    "UNCORRECTED (mean of each season):", box_seasons(season_names, raw, mult, "I"),
    paste0("  sum = ", full_digits(sum(raw) * unit), if (mult) " %" else ""),
    "",
    "CORRECTED (normalised):", box_seasons(season_names, idx, mult, "I'"),
    paste0("  sum = ", full_digits(sum(idx) * unit), if (mult) " %" else "")))
  invisible(list(uncorrected = raw, corrected = idx))
}

# -----------------------------------------------------------------------------
# autocorrelation(x, lag = NULL, threshold = 0.95, plot = TRUE, max_lag = NULL)
# -----------------------------------------------------------------------------
# WHAT IT DOES : CASE: "Determine if the time series has a cycle of period T"
#                or "find the cycle of the series". Correlates the series X(t)
#                with the same series shifted `lag` periods, X(t+lag). If
#                r is close to 1, the series repeats itself every `lag`
#                periods: there is a cycle of that period.
#                - With `lag`: shows the table of pairs, r, and the verdict.
#                - Without `lag`: tries the lags 1, 2, ..., max_lag, keeps the one with
#                  the HIGHEST r and shows it in the same format as above (after a
#                  small table with the r of every lag tested).
#                - With several lags, e.g. lag = 2:5: tries only those and keeps the highest.
# FORMULA (for reference — matches the slides' notation):
#   r = correlation coefficient between (X_1..X_{N-lag}) and (X_{1+lag}..X_N)
# PARAMETERS   :
#   x         : the series, in time order.
#   lag       : (optional) the period to test, e.g. 3. Leave empty to scan.
#   threshold : r from which a cycle is declared (default 0.95, "r close to 1").
#   plot      : FALSE to skip the plot.
#   max_lag   : (optional) largest lag to try when `lag` is empty. Default: N / 2
#               (rounded down), so every r uses at least half of the data.
# RETURNS      : Invisibly, r (one lag) or a data.frame lag / pairs / r (scan).
# WHAT IS THE LAG? : how many positions the series is shifted before comparing
#                it with itself = the period of the cycle you want to test.
#                lag = 3  ->  pairs (X1,X4) (X2,X5) (X3,X6) ...
# WHICH LAG DO I USE? (look at what the exercise says):
#   "cycle of period 3" / "frequency 3" / "shifted 3 periods" ....... lag = 3
#   "r between X(t) and X(t+T)" ...................................... lag = T
#   quarterly data, "is there a yearly pattern?" ..................... lag = 4
#   monthly data,   "is there a yearly pattern?" ..................... lag = 12
#   four-month data (3 per year) ..................................... lag = 3
#   semester data (2 per year) ....................................... lag = 2
#   daily data,  "is there a weekly pattern?" ........................ lag = 7
#   hourly data, "is there a daily pattern?" ......................... lag = 24
#   "which is the period of the cycle?" (not given) .................. no lag
#                                  (or try several lags and keep the r nearest 1)
#   "is it 3 or 4?" .................. run lag = 3 and lag = 4, the higher r wins
# EXAMPLES     :
#   x <- c(1.2, 1.4, 2.0, 1.23, 1.37, 2.07, 1.25, 1.47, 2.09)
#   autocorrelation(x, lag = 3)     # r = 0.9936708...  -> cycle of period 3
#   autocorrelation(x, lag = 2)     # r far from 1      -> NO cycle of period 2
#   autocorrelation(x)              # tries the lags 1..N/2 and keeps the HIGHEST r (here lag 3)
#   autocorrelation(x, lag = 2:4)   # tries only the lags 2, 3 and 4 and keeps the highest r
#   autocorrelation(x, max_lag = 6) # tries the lags 1..6 (default max is N/2 = 4 here)
#   # What you see with autocorrelation(x) (a table of the lags tested, then the usual output):
#   #   lag pairs                  r         best
#   #     1     8 -0.381812081358112
#   #     2     7 -0.541019836413653
#   #     3     6   0.99367081338781 <- highest r
#   #     4     5 -0.320450134225392
#   #   -> r close to 1 for lag 3: the series HAS A CYCLE of period 3
#   # 5 years of quarterly data (20 values), is there a yearly pattern?
#   autocorrelation(sales, lag = 4)
#   # 6 years of monthly data (72 values), is there a yearly pattern?
#   autocorrelation(sales, lag = 12)
# HOW TO READ r: close to 1 (>= 0.95 by default) -> cycle of that period.
#                far from 1 (e.g. 0.3)           -> no cycle of that period.
# -----------------------------------------------------------------------------
autocorrelation <- function(x, lag = NULL, threshold = 0.95, plot = TRUE, max_lag = NULL) {
  nm <- variable_label(deparse(substitute(x)), "X")
  x <- as.numeric(x); n <- length(x)
  r_at <- function(l) cor(x[1:(n - l)], x[(1 + l):n])
  scan_tab <- NULL
  if (is.null(lag) || length(lag) > 1) {
    # No lag (or several lags) given: try them all and keep the one with the HIGHEST r.
    # Then show that lag in EXACTLY the same format as a single given lag.
    if (is.null(lag)) {
      if (is.null(max_lag)) max_lag <- n %/% 2            # at least half of the data in every pair set
      lags <- 1:min(max_lag, n - 3)
    } else lags <- lag
    if (n < 6 && is.null(lag)) stop("At least 6 data are needed to scan the lags; give a `lag` instead.")
    lags <- lags[lags >= 1 & n - lags >= 3]
    if (length(lags) == 0) stop("No valid lag to test (each lag needs at least 3 pairs).")
    rs <- vapply(lags, r_at, numeric(1))
    scan_tab <- data.frame(lag = lags, pairs = n - lags, r = rs)
    lag <- lags[which.max(rs)]
    report_section(paste0("LAGS TESTED (the highest r is kept): ", paste(lags, collapse = ", ")))
    print(data.frame(lag = lags, pairs = n - lags, r = vapply(rs, full_digits, character(1)),
                     best = ifelse(lags == lag, "<- highest r", "")), row.names = FALSE, right = TRUE)
  }
  if (lag < 1 || n - lag < 3) stop("The lag must be between 1 and ", n - 3, " (at least 3 pairs are needed).")
  r <- r_at(lag)
  report_title(paste0("AUTOCORRELATION WITH LAG ", lag, ":  ", nm))
  report_section(paste0("Series X(t) and the same series shifted ", lag, " periods X(t+", lag, ")"))
  # Same table as the slides: columns t = 1 .. N + lag, the shifted series starts `lag` columns later.
  pair_tab <- rbind(c(x, rep(NA, lag)), c(rep(NA, lag), x))
  rownames(pair_tab) <- c("X(t)", paste0("X(t+", lag, ")"))
  colnames(pair_tab) <- paste0("t=", seq_len(n + lag))
  show_table(pair_tab)
  report_line("Number of pairs used", n - lag)
  report_line(paste0("r = correlation of X(t) and X(t+", lag, ")"), r)
  cat("  ->", if (r >= threshold) paste0("r is close to 1: the series HAS A CYCLE of period ", lag)
      else paste0("r is not close to 1: there is NO clear cycle of period ", lag), "\n")
  final_box(paste0("AUTOCORRELATION, LAG ", lag), c(
    box_line(paste0("r (X(t) and X(t+", lag, "))"), r),
    paste0("Pairs used = ", n - lag),
    if (r >= threshold) paste0("CONCLUSION: r is close to 1 -> there IS a cycle of period ", lag)
    else paste0("CONCLUSION: r is not close to 1 -> there is NO cycle of period ", lag)))
  if (plot) {
    plot(x[1:(n - lag)], x[(1 + lag):n], pch = 19, col = "steelblue",
         xlab = "X(t)", ylab = paste0("X(t+", lag, ")"), main = paste0("Autocorrelation, lag ", lag, ",  r = ", signif(r, 6)))
    abline(lm(x[(1 + lag):n] ~ x[1:(n - lag)]), col = "firebrick", lwd = 2)
  }
  invisible(if (is.null(scan_tab)) r else scan_tab)
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
  final_box("SIMPLE INDEX", c(
    paste0("X(t/t0) = ", full_digits(value), " / ", full_digits(base_value), " = ", full_digits(idx)),
    box_line("Index in percentage (%)", idx * 100),
    box_line("Variation (%)", (idx - 1) * 100),
    if (idx > 1) paste0("-> INCREASE of ", full_digits((idx - 1) * 100), " % with respect to the base")
    else if (idx < 1) paste0("-> DECREASE of ", full_digits((1 - idx) * 100), " % with respect to the base")
    else "-> no change"))
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
  b <- labels[pos]
  report_title(paste0("INDEX NUMBERS:  ", nm, "  (base period = ", b, ")"))
  
  report_section("GENERAL DATA")
  report_line("Variable", nm)
  report_line("Base period (t0)", b)
  report_line(paste0("Value in the base period, X(", b, ")"), values[pos])
  report_line("Number of periods", n)
  cat("\n  FORMULA:  I(t/", b, ") = X(t) / X(", b, ")     variation (%) = (I - 1) * 100\n", sep = "")
  
  report_section(paste0("STEP BY STEP: one index per period, base ", b))
  for (i in seq_len(n)) {
    cat(sprintf("\n  I(%s/%s) = X(%s) / X(%s) = %s / %s\n", labels[i], b, labels[i], b,
                full_digits(values[i]), full_digits(values[pos])))
    cat(sprintf("           = %s   (= %s %%)\n", full_digits(idx[i]), full_digits(idx[i] * 100)))
    if (i == pos) cat("           (base period: its index is always 1 = 100 %)\n")
    else cat(sprintf("           variation = (%s - 1) * 100 = %s %%\n", full_digits(idx[i]), full_digits((idx[i] - 1) * 100)))
  }
  
  report_section("SUMMARY TABLE")
  tab <- data.frame(period = labels, value = values, index = idx,
                    percentage = idx * 100, variation_percent = (idx - 1) * 100)
  print(tab, row.names = FALSE)
  
  ans <- character(0)
  for (i in seq_len(n)) {
    d <- (idx[i] - 1) * 100
    what <- if (i == pos) "base period"
    else if (d > 0) paste0(full_digits(d), " % MORE than in ", b)
    else if (d < 0) paste0(full_digits(-d), " % LESS than in ", b)
    else paste0("same as in ", b)
    ans <- c(ans, sprintf("I(%s/%s) = %s = %s %%  -> %s", labels[i], b, full_digits(idx[i]), full_digits(idx[i] * 100), what))
  }
  cat("\n  >>> ANSWER: ALL THE ELEMENTARY INDICES WITH BASE ", b, "\n", sep = "")
  answer_box(ans)
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
  final_box("INVERSE INDEX", c(
    paste0("X(a/b) = 1 / X(b/a) = ", if (percent) "100 * 100 / " else "1 / ", full_digits(index)),
    box_line(if (percent) "X(a/b) (%)" else "X(a/b)", out)))
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
  final_box("CHAINED INDEX", c(
    paste0("X(a/z) = ", paste(full_digits(v), collapse = if (percent) " % * " else " * "), if (percent) " %" else ""),
    box_line(if (percent) "X(a/z) (%)" else "X(a/z)", out)))
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
  if (length(real) == 1) {
    final_box("DEFLATION: REAL VALUE", c(
      paste0("Real value = ", full_digits(nominal), " / ", full_digits(cpi_ratio)),
      box_line("REAL VALUE (money of the base period)", real)))
  } else {
    lb <- if (is.null(labels)) seq_along(real) else labels
    final_box("DEFLATION: REAL VALUES (money of the base period)",
              vapply(seq_along(real), function(i) paste0("Real(", lb[i], ") = ", full_digits(nominal[i]), " / ", full_digits(cpi_ratio[i]), " = ", full_digits(real[i])), character(1)))
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
  final_box("REAL CHANGE (PURCHASING POWER)", c(
    box_line("Real change (%)", (ratio - 1) * 100),
    if (ratio < 1) paste0("-> the purchasing power DECREASED by ", full_digits((1 - ratio) * 100), " %")
    else if (ratio > 1) paste0("-> the purchasing power INCREASED by ", full_digits((ratio - 1) * 100), " %")
    else "-> the purchasing power did not change"))
  invisible(ratio)
}

# -----------------------------------------------------------------------------
# real_salary(salary_pct = NULL, cpi_pct = NULL, years = NULL,
#             salary = NULL, cpi = NULL, labels = NULL, base = 1)
# -----------------------------------------------------------------------------
# WHAT IT DOES : ONE CALL FOR THE WHOLE "salary vs CPI" EXERCISE: "the salary grew
#                50 % and the CPI doubled: how much did it REALLY grow?", "what is
#                the real salary of each year?", "did the purchasing power go up or
#                down?". It gives the index of the salary, the index of the CPI, the
#                real index, the real change in %, the real value in money of the
#                base period, the average yearly change (if you give the years),
#                the interpretation, and a final box with the answer.
# THE EASIEST WAY (the exercise gives percentages):
#   real_salary(salary_pct = 50, cpi_pct = 100)             # salary +50 %, CPI +100 % ("doubled")
#   real_salary(salary_pct = 50, cpi_pct = 100, years = 10) # + the average yearly change
# CPI "doubled" = +100 %, "tripled" = +200 %, "grew by 30 %" = +30, "fell by 10 %" = -10.
# WITH A TABLE (the exercise gives the salary and the CPI of each year):
#   real_salary(salary = c(1000, 1200, 1500), cpi = c(100, 110, 130), labels = 1970:1972)
#   (cpi = the CPI of each year, any base: only its ratio to the base year matters)
# PARAMETERS   :
#   salary_pct : % change of the salary over the period (50 = grew 50 %, -10 = fell 10 %).
#   cpi_pct    : % change of the CPI (prices) over the same period.
#   years      : (optional) length of the period in years, for the average yearly change.
#   salary     : (table mode) the salary of each period.
#   cpi        : (table mode) the CPI of each period.
#   labels     : (table mode, optional) the periods, e.g. 1970:1972.
#   base       : (table mode) base period: its label (1970) if you use `labels`,
#                otherwise its position. Default: the first one.
# RETURNS      : Invisibly, a list (percentage mode: real_ratio, real_change_pct; table mode:
#                a data.frame with the nominal and real values and indices).
# EXAMPLES     :
#   real_salary(salary_pct = 50, cpi_pct = 100)   # real change = -25 %: purchasing power DOWN
#   real_salary(salary_pct = 80, cpi_pct = 40)    # real change = +28.57... %: UP
#   real_salary(salary = c(1000, 1200, 1500), cpi = c(100, 110, 130), labels = 1970:1972)
# -----------------------------------------------------------------------------
real_salary <- function(salary_pct = NULL, cpi_pct = NULL, years = NULL,
                        salary = NULL, cpi = NULL, labels = NULL, base = 1) {
  if (!is.null(salary_pct) && !is.null(cpi_pct)) {
    nom <- salary_pct / 100; inf <- cpi_pct / 100
    ratio <- (1 + nom) / (1 + inf)
    chg <- (ratio - 1) * 100
    report_title("REAL SALARY: SALARY vs CPI (percentage changes)")
    report_section("GENERAL DATA")
    report_line("Change of the salary over the period (%)", salary_pct)
    report_line("Change of the CPI (prices) over the period (%)", cpi_pct)
    if (!is.null(years)) report_line("Length of the period (years)", years)
    report_section("STEP BY STEP (taking 100 as the starting salary and the starting CPI)")
    cat("  1. Index of the salary  = 1 + ", full_digits(salary_pct), " / 100 = ", full_digits(1 + nom),
        "   (nominal salary: 100 -> ", full_digits(100 * (1 + nom)), ")\n", sep = "")
    cat("  2. Index of the CPI     = 1 + ", full_digits(cpi_pct), " / 100 = ", full_digits(1 + inf),
        "   (prices: 100 -> ", full_digits(100 * (1 + inf)), ")\n", sep = "")
    cat("  3. Real index = salary index / CPI index = ", full_digits(1 + nom), " / ", full_digits(1 + inf),
        " = ", full_digits(ratio), "   (= ", full_digits(ratio * 100), " %)\n", sep = "")
    cat("  4. Real change (%) = (real index - 1) * 100 = (", full_digits(ratio), " - 1) * 100 = ", full_digits(chg), " %\n", sep = "")
    cat("  5. Real value of the new salary in money of the start: 100 * ", full_digits(ratio), " = ", full_digits(100 * ratio), "\n", sep = "")
    yr <- NULL
    if (!is.null(years)) {
      yr <- list(nominal = ((1 + nom)^(1 / years) - 1) * 100, cpi = ((1 + inf)^(1 / years) - 1) * 100,
                 real = (ratio^(1 / years) - 1) * 100)
      cat("  6. Average yearly change (compound): salary ", full_digits(yr$nominal), " %, CPI ", full_digits(yr$cpi),
          " %, REAL ", full_digits(yr$real), " %\n", sep = "")
    }
    verdict <- if (ratio < 1) paste0("the purchasing power DECREASED by ", full_digits((1 - ratio) * 100), " %")
    else if (ratio > 1) paste0("the purchasing power INCREASED by ", full_digits((ratio - 1) * 100), " %")
    else "the purchasing power did not change"
    cat("\n  ->", verdict, "\n")
    final_box("REAL SALARY: ANSWER", c(
      box_line("Salary index", 1 + nom),
      box_line("CPI index", 1 + inf),
      box_line("Real index", ratio),
      box_line("REAL CHANGE (%)", chg),
      box_line("Real value of 100 (money of the start)", 100 * ratio),
      if (!is.null(yr)) box_line("Average yearly REAL change (%)", yr$real),
      paste0("-> ", verdict)))
    return(invisible(list(real_ratio = ratio, real_change_pct = chg, yearly = yr)))
  }
  if (is.null(salary) || is.null(cpi))
    stop("Give salary_pct and cpi_pct (e.g. real_salary(50, 100)), or the tables salary = ... and cpi = ...")
  n <- length(salary)
  if (length(cpi) != n) stop("`salary` and `cpi` must have the same length.")
  if (is.null(labels)) labels <- seq_len(n)
  if (length(labels) != n) stop("`labels` must have one entry per period.")
  pos <- if (base %in% labels) which(labels == base)[1] else base
  if (is.na(pos) || pos < 1 || pos > n) stop("The base period is not in the data.")
  cpi_ratio <- cpi / cpi[pos]
  real <- salary / cpi_ratio
  nom_idx <- salary / salary[pos] * 100
  real_idx <- real / real[pos] * 100
  tab <- data.frame(period = labels, salary = salary, cpi = cpi, cpi_ratio = cpi_ratio,
                    real_salary = real, nominal_index = nom_idx, real_index = real_idx,
                    real_change_pct = real_idx - 100)
  report_title(paste0("REAL SALARY: SALARY vs CPI (base period = ", labels[pos], ")"))
  report_section("GENERAL DATA")
  report_line("Number of periods", n)
  report_line("Base period", labels[pos])
  cat("  FORMULAS: CPI ratio = CPI(t) / CPI(base);  real salary = salary(t) / CPI ratio;\n",
      "            real index = real salary(t) / real salary(base) * 100\n", sep = "")
  report_section("STEP BY STEP")
  for (i in seq_len(n)) {
    cat(sprintf("\n  %s:  CPI ratio = %s / %s = %s\n", labels[i], full_digits(cpi[i]), full_digits(cpi[pos]), full_digits(cpi_ratio[i])))
    cat(sprintf("        real salary = %s / %s = %s\n", full_digits(salary[i]), full_digits(cpi_ratio[i]), full_digits(real[i])))
    cat(sprintf("        real index = %s / %s * 100 = %s %%   (nominal index = %s %%)\n", full_digits(real[i]), full_digits(real[pos]),
                full_digits(real_idx[i]), full_digits(nom_idx[i])))
  }
  report_section("SUMMARY TABLE")
  print(tab, row.names = FALSE)
  last <- n
  chg <- real_idx[last] - 100
  verdict <- if (chg < 0) paste0("the purchasing power DECREASED by ", full_digits(-chg), " % between ", labels[pos], " and ", labels[last])
  else if (chg > 0) paste0("the purchasing power INCREASED by ", full_digits(chg), " % between ", labels[pos], " and ", labels[last])
  else "the purchasing power did not change"
  cat("\n  ->", verdict, "\n")
  final_box(paste0("REAL SALARY: ANSWER (base ", labels[pos], ")"), c(
    vapply(seq_len(n), function(i) paste0(labels[i], ":  real salary = ", full_digits(real[i]), "   real index = ", full_digits(real_idx[i]), " %"), character(1)),
    "",
    box_line(paste0("REAL CHANGE (%) from ", labels[pos], " to ", labels[last]), chg),
    paste0("-> ", verdict)))
  invisible(tab)
}

# -----------------------------------------------------------------------------
# cpi_conversion(cpi, amount, year = "t", base = "base", amount_base = NULL,
#                cpi_is_ratio = FALSE)
# -----------------------------------------------------------------------------
# WHAT IT DOES : ONE CALL FOR THE EXERCISE "Knowing that the CPI of the year 1998
#                with respect to 1990 is 135 %: (1) the real value in 1990 of a
#                product that cost 1000 in 1998; (2) the real value in 1998 of a
#                product that cost 1000 in 1990". It does BOTH directions, step
#                by step: the CPI, the inverse CPI, the real value in the base
#                year (divide by the CPI) and the real value in the later year
#                (multiply by the CPI = divide by the inverse CPI).
# THE EASIEST WAY:
#   cpi_conversion(cpi = 135, amount = 1000, year = 1998, base = 1990)
# PARAMETERS   :
#   cpi          : the CPI of the later year with respect to the base year, as the
#                  exercise gives it: 135 for "135 %". (If you have it as a ratio, 1.35,
#                  use cpi_is_ratio = TRUE.)
#   amount       : the price / amount in the LATER year (1998) for part (1)
#                  and, if `amount_base` is empty, also the price in the BASE year (1990)
#                  for part (2) (in the exercise both are 1000).
#   year         : the later year (only the label for the output), e.g. 1998.
#   base         : the base year (only the label), e.g. 1990.
#   amount_base  : (optional) the price in the BASE year for part (2) when it is not
#                  the same number as `amount`.
#   cpi_is_ratio : TRUE if you typed the CPI as a ratio (1.35) and not as a % (135).
# RETURNS      : Invisibly, a list with cpi, inverse_cpi, real_in_base and real_in_year.
# EXAMPLES     :
#   cpi_conversion(cpi = 135, amount = 1000, year = 1998, base = 1990)
#       # (1) 1000 of 1998 are 740.74... of 1990   (2) 1000 of 1990 are 1350 of 1998
#   cpi_conversion(cpi = 135, amount = 1000, amount_base = 2500, year = 1998, base = 1990)
# -----------------------------------------------------------------------------
cpi_conversion <- function(cpi, amount, year = "t", base = "base", amount_base = NULL, cpi_is_ratio = FALSE) {
  ratio <- if (cpi_is_ratio) cpi else cpi / 100
  if (is.null(amount_base)) amount_base <- amount
  inv <- 1 / ratio
  real_base <- amount / ratio                 # (1) value of the year-t amount in money of the base year
  real_year <- amount_base * ratio            # (2) value of the base-year amount in money of year t
  report_title(paste0("CPI CONVERSION: ", year, " <-> ", base))
  report_section("GENERAL DATA")
  report_line(paste0("CPI of ", year, " with respect to ", base, " (%)"), ratio * 100)
  report_line(paste0("Amount in ", year), amount)
  report_line(paste0("Amount in ", base), amount_base)
  cat("  RULE: to go from ", year, " back to ", base, " DIVIDE by the CPI; to go from ", base, " to ", year,
      " MULTIPLY by the CPI.\n", sep = "")
  report_section(paste0("STEP 1: the CPI as a ratio and its inverse"))
  cat("  CPI(", year, "/", base, ") = ", full_digits(ratio * 100), " / 100 = ", full_digits(ratio), "\n", sep = "")
  cat("  INVERSE: CPI(", base, "/", year, ") = 1 / ", full_digits(ratio), " = ", full_digits(inv),
      "   (= ", full_digits(inv * 100), " %)\n", sep = "")
  cat("  Prices ", if (ratio >= 1) "ROSE" else "FELL", " ", full_digits(abs(ratio - 1) * 100), " % between ", base, " and ", year, ".\n", sep = "")
  report_section(paste0("STEP 2  (1): real value in ", base, " of ", full_digits(amount), " of ", year))
  cat("  FORMULA : real value in ", base, " = amount in ", year, " / CPI(", year, "/", base, ")\n", sep = "")
  cat("  NUMBERS : ", full_digits(amount), " / ", full_digits(ratio), " = ", full_digits(real_base), "\n", sep = "")
  cat("  -> ", full_digits(amount), " of ", year, " are worth ", full_digits(real_base), " in money of ", base,
      "  (", if (real_base < amount) "less" else "more", ": prices ", if (ratio >= 1) "rose" else "fell", ")\n", sep = "")
  report_section(paste0("STEP 3  (2): real value in ", year, " of ", full_digits(amount_base), " of ", base))
  cat("  FORMULA : real value in ", year, " = amount in ", base, " / CPI(", base, "/", year, ") = amount in ", base, " * CPI(", year, "/", base, ")\n", sep = "")
  cat("  NUMBERS : ", full_digits(amount_base), " / ", full_digits(inv), " = ", full_digits(amount_base), " * ", full_digits(ratio),
      " = ", full_digits(real_year), "\n", sep = "")
  cat("  -> ", full_digits(amount_base), " of ", base, " are worth ", full_digits(real_year), " in money of ", year,
      "  (", if (real_year > amount_base) "more" else "less", ": prices ", if (ratio >= 1) "rose" else "fell", ")\n", sep = "")
  final_box("CPI CONVERSION: ANSWER", c(
    box_line(paste0("CPI(", year, "/", base, ")"), ratio),
    box_line(paste0("Inverse CPI(", base, "/", year, ")"), inv),
    "",
    paste0("(1) ", full_digits(amount), " of ", year, " = ", full_digits(real_base), " in ", base),
    paste0("(2) ", full_digits(amount_base), " of ", base, " = ", full_digits(real_year), " in ", year)))
  invisible(list(cpi = ratio, inverse_cpi = inv, real_in_base = real_base, real_in_year = real_year))
}