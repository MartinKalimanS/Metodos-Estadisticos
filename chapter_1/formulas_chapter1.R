##SOURCE PARA IMPORTAR LA LIBRERIA 
## source("chapter_1/formulas_chapter1.R")

# =============================================================================
#  CHAPTER 1 - DESCRIPTIVE STATISTICS OF ONE VARIABLE
#  One R function for every formula / concept in the slides.
#  (Department of Applied Mathematics, University of Malaga, 2024-2025)
# =============================================================================
#
#  HOW TO USE THIS FILE
#  --------------------
#  1. Save this file somewhere on your computer.
#  2. In R / RStudio run:      source("chapter1_descriptive_stats.R")
#  3. All the functions are now available. Example:
#         arithmetic_mean(c(7, 11, 11, 8, 12, 7, 6, 6))     # 8.5
#
#  Only base R is used. No packages need to be installed.
#
#  THE TWO WAYS OF GIVING DATA TO (ALMOST) EVERY FUNCTION
#  -------------------------------------------------------
#  (A) RAW DATA: just the list of observations, with repetitions.
#         x = c(6, 8, 6, 8, 6, 12)
#
#  (B) VALUES + FREQUENCIES (a frequency table): the different values xi
#      in `x` and how many times each one appears (ni) in `n`.
#         x = c(6, 8, 12)      # the values xi
#         n = c(4, 2, 1)       # their absolute frequencies ni
#      `x` and `n` must have the same length.
#
#  If you do NOT give `n`, every value in `x` counts once (case A).
#
#  GROUPED DATA (INTERVALS)
#  ------------------------
#  For data grouped in intervals (L_{i-1}, L_i] you have two options:
#   - Functions whose name ends in `_grouped` take `lower`, `upper`, `n`.
#   - For means / variances / moments, pass the CLASS MARKS as `x` and the
#     interval frequencies as `n` (the slides say: "we consider the class
#     marks as the values of the variable"). Use class_mark() to get them.
#
#  POPULATION vs SAMPLE
#  --------------------
#  Unless the name says "sample" or "quasi", the formulas divide by N
#  (population formulas, exactly as in the slides). R's built-in var() and
#  sd() divide by n-1 -> they correspond to sample_quasi_variance() and
#  sample_quasi_sd() here, NOT to variance().
#
#  INDEX (in order of the slides)
#  -------------------------------
#  1. FREQUENCIES
#       absolute_freq, relative_freq, cumulative_absolute_freq,
#       cumulative_relative_freq, frequency_table
#  2. GROUPED DATA / INTERVALS
#       interval_width, class_mark, histogram_height,
#       group_into_intervals, grouped_frequency_table
#  3. GRAPHS
#       plot_bar_chart, plot_pie_chart, plot_dot_diagram,
#       plot_histogram, plot_frequency_polygon
#  4. MEASURES OF CENTRAL TENDENCY
#       arithmetic_mean, sample_mean, affine_mean, affine_transform,
#       weighted_mean, quadratic_mean, mode_value, modal_interval,
#       median_value
#  5. QUANTILES
#       quantile_discrete, quartile, decile, percentile,
#       quantile_grouped, median_grouped, quartile_grouped,
#       decile_grouped, percentile_grouped
#  6. DISPERSION
#       range_value, interquartile_range, intercentile_range,
#       deviations, mean_deviation, mean_squared_error,
#       variance, variance_shortcut, standard_deviation,
#       sample_variance, sample_sd, sample_quasi_variance,
#       sample_quasi_sd, quasi_variance_from_sample_variance
#  7. COMPARISON MEASURES
#       standardize, standardize_data, coefficient_of_variation,
#       coefficient_of_variation_from_values
#  8. MOMENTS
#       moment_about_point, ordinary_moment, central_moment,
#       variance_from_ordinary_moments, central_moment3_from_ordinary,
#       central_moment4_from_ordinary
#  9. SHAPE (SKEWNESS & KURTOSIS)
#       pearson_skewness, fisher_skewness, kurtosis_coefficient,
#       interpret_skewness, interpret_kurtosis
#  10. EVERYTHING AT ONCE
#       describe_all
# =============================================================================


# -----------------------------------------------------------------------------
# INTERNAL HELPER (you do not need to call this one yourself)
# Turns (x, n) into the full list of observations, sorted.
#   expand_data(c(6, 8), c(2, 3))  ->  6 6 8 8 8
# -----------------------------------------------------------------------------
expand_data <- function(x, n = NULL) {
  if (is.null(n)) n <- rep(1, length(x))
  if (length(x) != length(n)) stop("`x` and `n` must have the same length.")
  sort(rep(x, times = n))
}

# INTERNAL HELPER: gives the frequencies to use (1 for each value if n = NULL)
freq_or_ones <- function(x, n = NULL) {
  if (is.null(n)) n <- rep(1, length(x))
  if (length(x) != length(n)) stop("`x` and `n` must have the same length.")
  n
}


# =============================================================================
# 1. FREQUENCIES
# =============================================================================

# -----------------------------------------------------------------------------
# absolute_freq(data)
# -----------------------------------------------------------------------------
# WHAT IT DOES : Counts how many times each different value appears.
#                This count is called the ABSOLUTE FREQUENCY of that value.
# FORMULA      : n_i = number of individuals with value x_i
# PARAMETERS   :
#   data : vector of raw observations (numbers OR text, e.g. "Spain").
# RETURNS      : Named numeric vector. Names = the values (sorted),
#                numbers = the absolute frequency.
# EXAMPLE      :
#   prices <- c(6,8,6,8,6,8,12,6,8,8,6,8,8,8,12,12,8,8,12,6,8,6,6,8,12,6,6,6,6,6)
#   absolute_freq(prices)
#   #  6  8 12
#   # 13 12  5
# -----------------------------------------------------------------------------
absolute_freq <- function(data) {
  t <- table(data)
  result <- as.numeric(t)
  names(result) <- names(t)
  result
}

# -----------------------------------------------------------------------------
# relative_freq(data)
# -----------------------------------------------------------------------------
# WHAT IT DOES : Proportion of individuals that have each value.
# FORMULA      : f_i = n_i / N          (N = total number of data)
# PARAMETERS   :
#   data : vector of raw observations (numbers or text).
# RETURNS      : Named numeric vector with the relative frequency (they add up to 1).
# EXAMPLE      :
#   relative_freq(prices)
#   #         6         8        12
#   # 0.4333333 0.4000000 0.1666667
# -----------------------------------------------------------------------------
relative_freq <- function(data) {
  ni <- absolute_freq(data)
  ni / sum(ni)
}

# -----------------------------------------------------------------------------
# cumulative_absolute_freq(data)
# -----------------------------------------------------------------------------
# WHAT IT DOES : For each value, how many data are LESS THAN OR EQUAL to it.
# FORMULA      : N_i = n_1 + n_2 + ... + n_i
# PARAMETERS   :
#   data : vector of raw observations. Must be orderable (numbers or
#          ordinal categories). For NOMINAL variables (e.g. colours) the
#          slides say cumulative frequencies make no sense.
# RETURNS      : Named numeric vector with the cumulative absolute frequency (the last one equals the total number of data).
# EXAMPLE      :
#   cumulative_absolute_freq(prices)
#   #  6  8 12
#   # 13 25 30
# -----------------------------------------------------------------------------
cumulative_absolute_freq <- function(data) {
  cumsum(absolute_freq(data))
}

# -----------------------------------------------------------------------------
# cumulative_relative_freq(data)
# -----------------------------------------------------------------------------
# WHAT IT DOES : Proportion of data that are LESS THAN OR EQUAL to each value.
# FORMULA      : F_i = N_i / N
# PARAMETERS   :
#   data : vector of raw observations (orderable).
# RETURNS      : Named numeric vector with the cumulative relative frequency (the last one equals 1).
# EXAMPLE      :
#   cumulative_relative_freq(prices)
#   #         6         8        12
#   # 0.4333333 0.8333333 1.0000000
# -----------------------------------------------------------------------------
cumulative_relative_freq <- function(data) {
  Ni <- cumulative_absolute_freq(data)
  Ni / Ni[length(Ni)]
}

# -----------------------------------------------------------------------------
# frequency_table(data)
# -----------------------------------------------------------------------------
# WHAT IT DOES : Builds the complete frequency table of the slides:
#                columns  each value | the absolute frequency | the relative frequency | the cumulative absolute frequency | the cumulative relative frequency
# PARAMETERS   :
#   data : vector of raw observations.
# RETURNS      : A data.frame with columns xi, ni, fi, Ni, Fi.
# EXAMPLE      :
#   frequency_table(prices)
#   #   xi ni        fi Ni        Fi
#   # 1  6 13 0.4333333 13 0.4333333
#   # 2  8 12 0.4000000 25 0.8333333
#   # 3 12  5 0.1666667 30 1.0000000
# -----------------------------------------------------------------------------
frequency_table <- function(data) {
  ni <- absolute_freq(data)
  xi <- names(ni)
  if (is.numeric(data)) xi <- as.numeric(xi)
  data.frame(xi = xi,
             ni = as.numeric(ni),
             fi = as.numeric(ni) / sum(ni),
             Ni = cumsum(as.numeric(ni)),
             Fi = cumsum(as.numeric(ni)) / sum(ni))
}


# =============================================================================
# 2. GROUPED DATA (INTERVALS)
#    Intervals are of the form (L_{i-1}, L_i]  ->  lower limit NOT included,
#    upper limit included.
# =============================================================================

# -----------------------------------------------------------------------------
# interval_width(lower, upper)
# -----------------------------------------------------------------------------
# WHAT IT DOES : Width (amplitude) of each interval.
# FORMULA      : a_i = L_i - L_{i-1}
# PARAMETERS   :
#   lower : vector with the lower limits the lower limit of the interval of the intervals.
#   upper : vector with the upper limits the upper limit of the interval   of the intervals.
# RETURNS      : Numeric vector with the interval width.
# EXAMPLE      :  (cafeteria example: 0-3, 3-6, 6-9, 9-12, 12-15)
#   interval_width(c(0,3,6,9,12), c(3,6,9,12,15))     # 3 3 3 3 3
# -----------------------------------------------------------------------------
interval_width <- function(lower, upper) {
  upper - lower
}

# -----------------------------------------------------------------------------
# class_mark(lower, upper)
# -----------------------------------------------------------------------------
# WHAT IT DOES : Midpoint of each interval (the "class mark"). It is the
#                value that represents the interval in all calculations.
# FORMULA      : x_i = (L_{i-1} + L_i) / 2
# PARAMETERS   :
#   lower : vector of lower limits the lower limit of the interval.
#   upper : vector of upper limits the upper limit of the interval.
# RETURNS      : Numeric vector with each value.
# EXAMPLE      :
#   class_mark(c(0,3,6,9,12), c(3,6,9,12,15))   # 1.5 4.5 7.5 10.5 13.5
# -----------------------------------------------------------------------------
class_mark <- function(lower, upper) {
  (lower + upper) / 2
}

# -----------------------------------------------------------------------------
# histogram_height(n, lower, upper, relative = FALSE)
# -----------------------------------------------------------------------------
# WHAT IT DOES : Height of each rectangle of the histogram, so that the
#                AREA of the rectangle is proportional to the frequency.
#                Needed when the intervals have DIFFERENT widths.
# FORMULA      : h_i = n_i / a_i       (relative = FALSE, uses n_i)
#                h_i = f_i / a_i       (relative = TRUE,  uses f_i)
#                Both give the same shape (slide "Note on Histograms").
# PARAMETERS   :
#   n        : vector with the absolute frequency of each interval.
#   lower    : vector of lower limits the lower limit of the interval.
#   upper    : vector of upper limits the upper limit of the interval.
#   relative : FALSE (default) -> the absolute frequency/the interval width ;  TRUE -> the relative frequency/the interval width.
# RETURNS      : Numeric vector with the histogram height.
# EXAMPLE      :  (temperatures "But also" table)
#   histogram_height(c(9,8,5,7,6), c(70,80,85,90,95), c(80,85,90,95,110),
#                    relative = TRUE)
#   # 0.0257 0.0457 0.0286 0.0400 0.0114
# -----------------------------------------------------------------------------
histogram_height <- function(n, lower, upper, relative = FALSE) {
  a <- interval_width(lower, upper)
  if (relative) (n / sum(n)) / a else n / a
}

# -----------------------------------------------------------------------------
# group_into_intervals(data, breaks, right_closed = TRUE)
# -----------------------------------------------------------------------------
# WHAT IT DOES : Takes RAW data and counts how many observations fall in
#                each interval.
# PARAMETERS   :
#   data         : vector of raw numeric observations.
#   breaks       : vector with ALL the interval limits in increasing order,
#                  e.g. c(70, 75, 80) means two intervals: 70-75 and 75-80.
#                  Every data value must be inside the first and last break.
#   right_closed : TRUE (default) -> each interval excludes its lower limit
#                  and includes its upper limit, the convention of the
#                  slides: a value equal to a limit goes to the interval on
#                  its LEFT (80 goes into 75-80).
#                  FALSE -> each interval includes its lower limit and
#                  excludes its upper limit: a value equal to a limit goes
#                  to the interval on its RIGHT (80 goes into 80-85). The
#                  temperature table on slide 22 uses this one.
# RETURNS      : data.frame with columns lower, upper, ni.
# EXAMPLE      :
#   temps <- c(72,78,86,93,106,107,98,82,81,77,87,82,91,95,92,83,76,78,73,
#              81,86,92,93,84,107,99,94,86,81,77,73,76,80,88,91)
#   group_into_intervals(temps, seq(70, 110, by = 5), right_closed = FALSE)
#   # ni = 3 6 8 5 7 3 0 3   (same as the table on slide 22)
# -----------------------------------------------------------------------------
group_into_intervals <- function(data, breaks, right_closed = TRUE) {
  counts <- table(cut(data, breaks = breaks, right = right_closed,
                      include.lowest = TRUE))
  data.frame(lower = breaks[-length(breaks)],
             upper = breaks[-1],
             ni    = as.numeric(counts))
}

# -----------------------------------------------------------------------------
# grouped_frequency_table(lower, upper, n)
# -----------------------------------------------------------------------------
# WHAT IT DOES : Builds the full table for grouped data:
#                lower | upper | each value (class mark) | the interval width | the absolute frequency | the relative frequency |
#                the cumulative absolute frequency | the cumulative relative frequency | the histogram height (= the absolute frequency / the interval width)
# PARAMETERS   :
#   lower : vector of lower limits the lower limit of the interval.
#   upper : vector of upper limits the upper limit of the interval.
#   n     : vector of absolute how many times each one appears (its frequency).
# RETURNS      : data.frame with all those columns.
# EXAMPLE      :
#   grouped_frequency_table(c(0,3,6,9,12), c(3,6,9,12,15), c(40,30,10,5,5))
# -----------------------------------------------------------------------------
grouped_frequency_table <- function(lower, upper, n) {
  data.frame(lower = lower,
             upper = upper,
             xi = class_mark(lower, upper),
             ai = interval_width(lower, upper),
             ni = n,
             fi = n / sum(n),
             Ni = cumsum(n),
             Fi = cumsum(n) / sum(n),
             hi = n / interval_width(lower, upper))
}


# =============================================================================
# 3. GRAPHS
# =============================================================================

# -----------------------------------------------------------------------------
# plot_bar_chart(categories, n, horizontal = FALSE, main = "Bar chart")
# -----------------------------------------------------------------------------
# WHAT IT DOES : Bar (rectangle) diagram for QUALITATIVE variables. Each
#                category gets a bar whose height is its frequency.
# PARAMETERS   :
#   categories : vector with the names of the categories.
#   n          : vector with the frequency of each category.
#   horizontal : TRUE for horizontal bars (like the slide), FALSE vertical.
#   main       : title of the plot (text).
# RETURNS      : Nothing; draws the plot.
# EXAMPLE      :
#   plot_bar_chart(c("Chemistry","Mathematics","Physics","Biology","Geology"),
#                  c(1500, 750, 1000, 500, 250))
# -----------------------------------------------------------------------------
plot_bar_chart <- function(categories, n, horizontal = FALSE, main = "Bar chart") {
  barplot(n, names.arg = categories, horiz = horizontal, main = main,
          col = "steelblue", las = 1)
}

# -----------------------------------------------------------------------------
# plot_pie_chart(categories, n, main = "Pie chart")
# -----------------------------------------------------------------------------
# WHAT IT DOES : Pie chart. Each sector has an area proportional to the
#                frequency of its category. Labels show the percentage.
# PARAMETERS   :
#   categories : vector with the names of the categories.
#   n          : vector with the frequency of each category.
#   main       : title of the plot.
# RETURNS      : Nothing; draws the plot.
# EXAMPLE      :
#   plot_pie_chart(c("Philadelphia","Suburbs","PA","NJ","Elsewhere"),
#                  c(225, 100, 60, 75, 40))
# -----------------------------------------------------------------------------
plot_pie_chart <- function(categories, n, main = "Pie chart") {
  pct <- round(100 * n / sum(n), 1)
  pie(n, labels = paste0(categories, " (", pct, "%)"), main = main)
}

# -----------------------------------------------------------------------------
# plot_dot_diagram(x, n, main = "Bar / dot diagram")
# -----------------------------------------------------------------------------
# WHAT IT DOES : Bar or dot diagram for DISCRETE quantitative variables:
#                a thin vertical bar at each different value, with the bar's height
#                equal to how many times that value appears (its frequency), and
#                a dot on top.
# PARAMETERS   :
#   x    : vector of the different values.
#   n    : vector with how many times each value appears (its frequency).
#   main : title.
# RETURNS      : Nothing; draws the plot.
# EXAMPLE      :
#   plot_dot_diagram(c(6, 8, 12), c(13, 12, 5))
# -----------------------------------------------------------------------------
plot_dot_diagram <- function(x, n, main = "Bar / dot diagram") {
  plot(x, n, type = "h", lwd = 3, col = "steelblue", ylim = c(0, max(n)),
       xlab = "x", ylab = "Frequency", main = main)
  points(x, n, pch = 19, col = "steelblue")
}

# -----------------------------------------------------------------------------
# plot_histogram(lower, upper, n, main = "Histogram")
# -----------------------------------------------------------------------------
# WHAT IT DOES : Histogram for data grouped in intervals. Each rectangle
#                has as base the interval, and its height is set so the
#                rectangle's AREA is proportional to the absolute frequency
#                of that interval (this works even when interval widths differ).
# PARAMETERS   :
#   lower : vector of lower limits.
#   upper : vector of upper limits.
#   n     : vector of absolute frequencies.
#   main  : title.
# RETURNS      : Nothing; draws the plot.
# EXAMPLE      :
#   plot_histogram(c(70,80,85,90,95), c(80,85,90,95,110), c(9,8,5,7,6))
# -----------------------------------------------------------------------------
plot_histogram <- function(lower, upper, n, main = "Histogram") {
  h <- histogram_height(n, lower, upper)
  plot(NA, xlim = c(min(lower), max(upper)), ylim = c(0, max(h)),
       xlab = "x", ylab = "Height  h = n / a", main = main)
  rect(lower, 0, upper, h, col = "khaki1", border = "black")
}

# -----------------------------------------------------------------------------
# plot_frequency_polygon(lower, upper, n, add = FALSE, main = "Frequency polygon")
# -----------------------------------------------------------------------------
# WHAT IT DOES : Frequency polygon: joins the top midpoints of the histogram
#                rectangles. As the slides say, one extra empty interval of
#                the same width is added at the start and at the end, so the
#                line starts and ends at height 0.
# PARAMETERS   :
#   lower : vector of lower limits.
#   upper : vector of upper limits.
#   n     : vector of absolute frequencies.
#   add   : TRUE draws the polygon ON TOP of a plot already drawn
#           (e.g. right after plot_histogram). FALSE makes a new plot.
#   main  : title (only used when add = FALSE).
# RETURNS      : Nothing; draws the plot.
# EXAMPLE      :
#   L <- seq(70, 105, 5); U <- L + 5; ni <- c(3,6,8,5,7,3,0,3)
#   plot_histogram(L, U, ni)
#   plot_frequency_polygon(L, U, ni, add = TRUE)
# -----------------------------------------------------------------------------
plot_frequency_polygon <- function(lower, upper, n, add = FALSE,
                                   main = "Frequency polygon") {
  h  <- histogram_height(n, lower, upper)
  a  <- interval_width(lower, upper)
  xs <- c(lower[1] - a[1] / 2, class_mark(lower, upper),
          upper[length(upper)] + a[length(a)] / 2)
  ys <- c(0, h, 0)
  if (add) {
    lines(xs, ys, col = "blue", lwd = 2)
  } else {
    plot(xs, ys, type = "l", col = "blue", lwd = 2,
         xlab = "x", ylab = "Height  h = n / a", main = main)
  }
}


# =============================================================================
# 4. MEASURES OF CENTRAL TENDENCY
# =============================================================================

# -----------------------------------------------------------------------------
# arithmetic_mean(x, n = NULL)
# -----------------------------------------------------------------------------
# WHAT IT DOES : Simple arithmetic mean: the sum of all values divided by
#                the number of data.
# FORMULA      : mu = sum(x_i * n_i) / N  = sum(x_i * f_i)
# PARAMETERS   :
#   x : raw data, OR the different values (or class marks).
#   n : (optional) the absolute frequency of each value in `x`. Leave empty for raw data.
# RETURNS      : One number, the mean.
# EXAMPLES     :
#   arithmetic_mean(c(7, 11, 11, 8, 12, 7, 6, 6))                  # 8.5
#   arithmetic_mean(c(13.725,13.975,14.225,14.725), c(7,14,18,6))  # 14.13611
#   # Grouped data -> use class marks:
#   arithmetic_mean(class_mark(c(0,3,6,9,12), c(3,6,9,12,15)), c(40,30,10,5,5))
# -----------------------------------------------------------------------------
arithmetic_mean <- function(x, n = NULL) {
  n <- freq_or_ones(x, n)
  sum(x * n) / sum(n)
}

# -----------------------------------------------------------------------------
# sample_mean(x, n = NULL)
# -----------------------------------------------------------------------------
# WHAT IT DOES : Sample mean x-bar. It is computed exactly like the
#                arithmetic mean; it is the best estimator of the
#                population mean the mean. (Exists only so the name matches
#                the slide "Sample Mean and Variance".)
# FORMULA      : x_bar = sum(x_i * n_i) / n
# PARAMETERS   : same as arithmetic_mean().
# RETURNS      : One number.
# EXAMPLE      :  sample_mean(c(12, 10, 9, 9, 10))     # 10
# -----------------------------------------------------------------------------
sample_mean <- function(x, n = NULL) {
  arithmetic_mean(x, n)
}

# -----------------------------------------------------------------------------
# affine_mean(mean_x, a, b)
# -----------------------------------------------------------------------------
# WHAT IT DOES : If Y = aX + b, the mean of Y can be obtained from the mean
#                of X without recomputing anything.
# FORMULA      : mu_Y = a * mu_X + b
# PARAMETERS   :
#   mean_x : the mean of the original variable X.
#   a      : the number that multiplies X.
#   b      : the number that is added.
# RETURNS      : One number: the mean of Y.
# EXAMPLE      :  (slide 31: X = 0.25*Y + 13.975. The y values are
#                 -1, 0, 1, 3 with n = 7, 14, 18, 6, so mu_Y = 29/45)
#   affine_mean(29/45, a = 0.25, b = 13.975)     # 14.13611 = mu_X
# -----------------------------------------------------------------------------
affine_mean <- function(mean_x, a, b) {
  a * mean_x + b
}

# -----------------------------------------------------------------------------
# affine_transform(x, a, b)
# -----------------------------------------------------------------------------
# WHAT IT DOES : Applies the change of variable y_i = a * x_i + b to every
#                value. Useful to simplify calculations (slide 31).
# PARAMETERS   :
#   x : vector of values.
#   a : multiplier.
#   b : amount added.
# RETURNS      : Vector with the transformed values y_i.
# EXAMPLE      :  (Y = (X - 13.975)/0.25  ->  a = 1/0.25 = 4, b = -13.975*4)
#   affine_transform(c(13.725,13.975,14.225,14.725), a = 4, b = -13.975*4)
#   # -1 0 1 3
# -----------------------------------------------------------------------------
affine_transform <- function(x, a, b) {
  a * x + b
}

# -----------------------------------------------------------------------------
# weighted_mean(x, w)
# -----------------------------------------------------------------------------
# WHAT IT DOES : Mean where each value has a different importance (weight).
# FORMULA      : WM = sum(x_i * w_i) / sum(w_i)
# PARAMETERS   :
#   x : vector of values.
#   w : vector of weights w_i (same length as x).
# RETURNS      : One number.
# EXAMPLE      :  (student grades)
#   weighted_mean(c(2.6, 3.7, 5.1, 4.9, 6.4), c(1, 1, 1, 2, 3))     # 5.05
# -----------------------------------------------------------------------------
weighted_mean <- function(x, w) {
  if (length(x) != length(w)) stop("`x` and `w` must have the same length.")
  sum(x * w) / sum(w)
}

# -----------------------------------------------------------------------------
# quadratic_mean(x, n = NULL)
# -----------------------------------------------------------------------------
# WHAT IT DOES : Quadratic mean / Root Mean Square (RMS): square root of the
#                mean of the squares.
# FORMULA      : RMS = sqrt( sum(n_i * x_i^2) / N )
# PARAMETERS   :
#   x : raw data, or the different values.
#   n : (optional) how many times each one appears (its frequency).
# RETURNS      : One number.
# EXAMPLE      :  (calls to the technical service)
#   quadratic_mean(c(2, 3, 1, 0, 4, 3))      # 2.54951
# -----------------------------------------------------------------------------
quadratic_mean <- function(x, n = NULL) {
  n <- freq_or_ones(x, n)
  sqrt(sum(n * x^2) / sum(n))
}

# -----------------------------------------------------------------------------
# mode_value(x, n = NULL)
# -----------------------------------------------------------------------------
# WHAT IT DOES : Mode (the mode): the value(s) with the highest frequency.
#                - It can have several modes (all are returned).
#                - If ALL values have the same frequency there is NO mode
#                  and the function returns NA.
# PARAMETERS   :
#   x : raw data (numbers or text), or the different values.
#   n : (optional) how many times each one appears (its frequency).
# RETURNS      : Vector with the mode(s), or NA if there is no mode.
# EXAMPLES     :
#   mode_value(c(7, 11, 11, 8, 12, 9, 6, 6))            # 6 11  (two modes)
#   mode_value(c(7, 11, 11, 8, 12, 12, 12, 7, 6, 6))    # 12
#   mode_value(c(7, 11, 8, 12, 9, 6))                   # NA   (no mode)
# -----------------------------------------------------------------------------
mode_value <- function(x, n = NULL) {
  data <- expand_data(x, n)
  ni <- absolute_freq(data)
  if (length(unique(ni)) == 1 && length(ni) > 1) return(NA)
  modes <- names(ni)[ni == max(ni)]
  if (is.numeric(x)) as.numeric(modes) else modes
}

# -----------------------------------------------------------------------------
# modal_interval(lower, upper, n)
# -----------------------------------------------------------------------------
# WHAT IT DOES : For grouped (continuous) data: the interval with the
#                highest histogram height (absolute frequency divided by
#                interval width). This is NOT simply the interval with the
#                highest absolute frequency, because widths may differ.
# PARAMETERS   :
#   lower : vector of lower limits.
#   upper : vector of upper limits.
#   n     : vector of absolute frequencies.
# RETURNS      : data.frame with the modal interval(s): lower, upper, height.
# EXAMPLE      :
#   modal_interval(c(70,80,85,90,95), c(80,85,90,95,110), c(9,8,5,7,6))
#   # (80, 85]   because h = 8/5 = 1.6 is the largest
# -----------------------------------------------------------------------------
modal_interval <- function(lower, upper, n) {
  h <- histogram_height(n, lower, upper)
  i <- which(h == max(h))
  data.frame(lower = lower[i], upper = upper[i], height = h[i])
}

# -----------------------------------------------------------------------------
# median_value(x, n = NULL)
# -----------------------------------------------------------------------------
# WHAT IT DOES : Median (the median): value that splits the ordered data in two
#                halves. With an odd number of data, it is the middle value.
#                With an even number of data, it is the mean of the two
#                middle values. (It is the quantile c = 0.5.)
#                The data do NOT need to be sorted; the function sorts them.
# PARAMETERS   :
#   x : raw data, or the different values.
#   n : (optional) how many times each one appears (its frequency).
# RETURNS      : One number.
# EXAMPLES     :
#   median_value(c(11, 11, 16, 17, 25))             # 16
#   median_value(c(1, 4, 8, 8, 10, 16, 16, 19))     # 9
# -----------------------------------------------------------------------------
median_value <- function(x, n = NULL) {
  quantile_discrete(x, 0.5, n)
}


# =============================================================================
# 5. QUANTILES (quartiles, deciles, percentiles)
# =============================================================================

# -----------------------------------------------------------------------------
# quantile_discrete(x, c, n = NULL)
# -----------------------------------------------------------------------------
# WHAT IT DOES : The quantile of NON-grouped data, using EXACTLY the rule of
#                the slides (this can differ from R's quantile(), which uses
#                another method):
#                  1. Sort the data. Compute c*the total number of data = E + D
#                     (E = integer part, D = decimal part).
#                  2. If D != 0 -> the quantile = value in position E + 1.
#                  3. If D == 0 -> the quantile = (value in position E +
#                                         value in position E+1) / 2.
# PARAMETERS   :
#   x : raw data (any order), or the different values.
#   c : a number strictly between 0 and 1 (e.g. 0.25 for the first quartile, 0.37 for the 37th percentile).
#   n : (optional) how many times each one appears (its frequency).
# RETURNS      : One number.
# EXAMPLE      :
#   d <- c(2, 5, 3, 4, 7, 0, 11, 2, 3, 8)
#   quantile_discrete(d, 0.25)   # 2      (Q1)
#   quantile_discrete(d, 0.37)   # 3      (P37)
#   quantile_discrete(d, 0.68)   # 5      (P68)
# -----------------------------------------------------------------------------
quantile_discrete <- function(x, c, n = NULL) {
  if (c <= 0 || c >= 1) stop("`c` must be strictly between 0 and 1.")
  data <- expand_data(x, n)
  N  <- length(data)
  cN <- round(c * N, 10)          # round to avoid tiny decimal errors
  E  <- floor(cN)
  D  <- cN - E
  if (D != 0) data[E + 1] else (data[E] + data[E + 1]) / 2
}

# -----------------------------------------------------------------------------
# quartile(x, k, n = NULL)
# -----------------------------------------------------------------------------
# WHAT IT DOES : Quartile Q_k = X(k/4).   the first quartile = 25%, Q2 = median, the third quartile = 75%.
# PARAMETERS   :
#   x : raw data, or the different values.
#   k : 1, 2 or 3.
#   n : (optional) how many times each one appears (its frequency).
# RETURNS      : One number.
# EXAMPLE      :
#   quartile(c(2,5,3,4,7,0,11,2,3,8), 1)    # 2
#   quartile(c(2,5,3,4,7,0,11,2,3,8), 2)    # 3.5
#   quartile(c(2,5,3,4,7,0,11,2,3,8), 3)    # 7
# -----------------------------------------------------------------------------
quartile <- function(x, k, n = NULL) {
  if (!(k %in% 1:3)) stop("`k` must be 1, 2 or 3.")
  quantile_discrete(x, k / 4, n)
}

# -----------------------------------------------------------------------------
# decile(x, k, n = NULL)
# -----------------------------------------------------------------------------
# WHAT IT DOES : Decile D_k = X(k/10). There are 9 deciles.
# PARAMETERS   :
#   x : raw data, or the different values.
#   k : whole number from 1 to 9.
#   n : (optional) how many times each one appears (its frequency).
# RETURNS      : One number.
# EXAMPLE      :  decile(c(2,5,3,4,7,0,11,2,3,8), 9)     # 9.5
# -----------------------------------------------------------------------------
decile <- function(x, k, n = NULL) {
  if (!(k %in% 1:9)) stop("`k` must be a whole number from 1 to 9.")
  quantile_discrete(x, k / 10, n)
}

# -----------------------------------------------------------------------------
# percentile(x, k, n = NULL)
# -----------------------------------------------------------------------------
# WHAT IT DOES : The percentile that leaves k% of the data below it.
# PARAMETERS   :
#   x : raw data, or the different values.
#   k : whole number from 1 to 99.
#   n : (optional) how many times each one appears (its frequency).
# RETURNS      : One number.
# EXAMPLE      :
#   percentile(c(2,5,3,4,7,0,11,2,3,8), 37)    # 3
#   percentile(c(2,5,3,4,7,0,11,2,3,8), 68)    # 5
# -----------------------------------------------------------------------------
percentile <- function(x, k, n = NULL) {
  if (!(k %in% 1:99)) stop("`k` must be a whole number from 1 to 99.")
  quantile_discrete(x, k / 100, n)
}

# -----------------------------------------------------------------------------
# quantile_grouped(lower, upper, n, c)
# -----------------------------------------------------------------------------
# WHAT IT DOES : Quantile the quantile for data GROUPED IN INTERVALS (continuous
#                case), by linear interpolation inside the interval:
#                  1. Compute c*the total number of data.
#                  2. Find the first interval i with N_{i-1} <= c*the total number of data < the cumulative absolute frequency.
#                  3. the quantile = the lower limit of the interval + (c*the total number of data - N_{i-1}) / the absolute frequency * the interval width
#                where the lower limit of the interval = lower limit of that interval,
#                      N_{i-1} = cumulative frequency of the PREVIOUS interval,
#                      the interval width     = width of the interval,
#                      the absolute frequency     = frequency of the interval.
# PARAMETERS   :
#   lower : vector of lower limits.
#   upper : vector of upper limits.
#   n     : vector of absolute frequencies.
#   c     : number strictly between 0 and 1.
# RETURNS      : One number.
# EXAMPLE      :
#   quantile_grouped(c(0,3,6,9,12), c(3,6,9,12,15), c(40,30,10,5,5), 0.5)
#   # cN = 45 -> interval (3,6] -> 3 + (45-40)/30*3 = 3.5
# -----------------------------------------------------------------------------
quantile_grouped <- function(lower, upper, n, c) {
  if (c <= 0 || c >= 1) stop("`c` must be strictly between 0 and 1.")
  cN   <- round(c * sum(n), 10)
  Ni   <- cumsum(n)
  Nprev <- c(0, Ni[-length(Ni)])        # N_{i-1}
  i    <- which(Nprev <= cN & cN < Ni)[1]
  a    <- interval_width(lower, upper)
  lower[i] + (cN - Nprev[i]) / n[i] * a[i]
}

# -----------------------------------------------------------------------------
# median_grouped(lower, upper, n)
# quartile_grouped(lower, upper, n, k)      k = 1, 2, 3
# decile_grouped(lower, upper, n, k)        k = 1..9
# percentile_grouped(lower, upper, n, k)    k = 1..99
# -----------------------------------------------------------------------------
# WHAT THEY DO : Same as median / quartile / decile / percentile but for
#                grouped data. They just call quantile_grouped() with
#                c = 0.5, k/4, k/10, k/100.
# PARAMETERS   :
#   lower, upper, n : as in quantile_grouped().
#   k               : which quartile / decile / percentile.
# RETURNS      : One number.
# EXAMPLE      :
#   L <- c(0,3,6,9,12); U <- c(3,6,9,12,15); ni <- c(40,30,10,5,5)
#   median_grouped(L, U, ni)           # 3.5
#   quartile_grouped(L, U, ni, 1)      # 1.6875
#   decile_grouped(L, U, ni, 1)        # 0.675
#   percentile_grouped(L, U, ni, 90)   # 9.6
# -----------------------------------------------------------------------------
median_grouped <- function(lower, upper, n) {
  quantile_grouped(lower, upper, n, 0.5)
}
quartile_grouped <- function(lower, upper, n, k) {
  if (!(k %in% 1:3)) stop("`k` must be 1, 2 or 3.")
  quantile_grouped(lower, upper, n, k / 4)
}
decile_grouped <- function(lower, upper, n, k) {
  if (!(k %in% 1:9)) stop("`k` must be a whole number from 1 to 9.")
  quantile_grouped(lower, upper, n, k / 10)
}
percentile_grouped <- function(lower, upper, n, k) {
  if (!(k %in% 1:99)) stop("`k` must be a whole number from 1 to 99.")
  quantile_grouped(lower, upper, n, k / 100)
}


# =============================================================================
# 6. DISPERSION MEASURES
# =============================================================================

# -----------------------------------------------------------------------------
# range_value(x)
# -----------------------------------------------------------------------------
# WHAT IT DOES : Range R = largest value - smallest value.
#                (Very sensitive to errors / outliers.)
# PARAMETERS   :
#   x : raw data or the different values (frequencies don't matter).
# RETURNS      : One number.
# EXAMPLE      :  range_value(c(2, 5, 3, 4, 7, 0, 11, 2, 3, 8))    # 11
# -----------------------------------------------------------------------------
range_value <- function(x) {
  max(x) - min(x)
}

# -----------------------------------------------------------------------------
# interquartile_range(x, n = NULL)
# -----------------------------------------------------------------------------
# WHAT IT DOES : Interquartile range  R_Q = the third quartile - the first quartile  (robust).
# PARAMETERS   :
#   x : raw data, or the different values.
#   n : (optional) how many times each one appears (its frequency).
# RETURNS      : One number.
# EXAMPLE      :  interquartile_range(c(2,5,3,4,7,0,11,2,3,8))    # 7 - 2 = 5
# -----------------------------------------------------------------------------
interquartile_range <- function(x, n = NULL) {
  quartile(x, 3, n) - quartile(x, 1, n)
}

# -----------------------------------------------------------------------------
# intercentile_range(x, n = NULL)
# -----------------------------------------------------------------------------
# WHAT IT DOES : Intercentile range  R_P = P99 - P1  (robust).
# PARAMETERS   :
#   x : raw data, or the different values.
#   n : (optional) how many times each one appears (its frequency).
# RETURNS      : One number.
# EXAMPLE      :  intercentile_range(c(2,5,3,4,7,0,11,2,3,8))    # 11 - 0 = 11
# -----------------------------------------------------------------------------
intercentile_range <- function(x, n = NULL) {
  percentile(x, 99, n) - percentile(x, 1, n)
}

# -----------------------------------------------------------------------------
# deviations(x, p)
# -----------------------------------------------------------------------------
# WHAT IT DOES : Deviation of each value with respect to a point p
#                (usually p = mean or median).
# FORMULA      : d_i = |x_i - p|
# PARAMETERS   :
#   x : vector of values.
#   p : the reference number.
# RETURNS      : Vector with d_i.
# EXAMPLE      :  deviations(c(5, 2, 3, 3, 3, 5, 7), 4)   # 1 2 1 1 1 1 3
# -----------------------------------------------------------------------------
deviations <- function(x, p) {
  abs(x - p)
}

# -----------------------------------------------------------------------------
# mean_deviation(x, n = NULL, p = NULL)
# -----------------------------------------------------------------------------
# WHAT IT DOES : Mean of the absolute deviations with respect to p.
# FORMULA      : MD(p) = sum(|x_i - p| * n_i) / N
#                If p is not given, p = arithmetic mean  ->  "mean deviation" MD.
# PARAMETERS   :
#   x : raw data, or the different values.
#   n : (optional) how many times each one appears (its frequency).
#   p : (optional) reference point. Default: the mean.
#       Use p = median_value(x) for the deviation about the median.
# RETURNS      : One number.
# EXAMPLES     :
#   v <- c(5, 2, 3, 3, 3, 5, 7)
#   mean_deviation(v)                       # about the mean (4):   1.428571
#   mean_deviation(v, p = median_value(v))  # about the median (3): 1.285714
# -----------------------------------------------------------------------------
mean_deviation <- function(x, n = NULL, p = NULL) {
  n <- freq_or_ones(x, n)
  if (is.null(p)) p <- arithmetic_mean(x, n)
  sum(abs(x - p) * n) / sum(n)
}

# -----------------------------------------------------------------------------
# mean_squared_error(x, n = NULL, p = NULL)
# -----------------------------------------------------------------------------
# WHAT IT DOES : Mean of the SQUARED deviations with respect to p.
# FORMULA      : MSE(p) = sum(n_i * (x_i - p)^2) / N
#                With p = mean, MSE equals the variance.
# PARAMETERS   :
#   x : raw data, or the different values.
#   n : (optional) how many times each one appears (its frequency).
#   p : (optional) reference point. Default: the mean.
# RETURNS      : One number.
# EXAMPLES     :
#   v <- c(5, 2, 3, 3, 3, 5, 7)
#   mean_squared_error(v)                        # about the mean (4):   2.571429
#   mean_squared_error(v, p = median_value(v))   # about the median (3): 3.571429
#   # Error of a prediction (slide 53): use x = predicted - actual, p = 0
#   err <- c(0,0,0,1,1,1,3,3,3) - c(0,1,3,0,1,3,0,1,3)
#   mean_squared_error(err, n = c(6,3,2,3,5,1,1,3,7), p = 0)
# -----------------------------------------------------------------------------
mean_squared_error <- function(x, n = NULL, p = NULL) {
  n <- freq_or_ones(x, n)
  if (is.null(p)) p <- arithmetic_mean(x, n)
  sum(n * (x - p)^2) / sum(n)
}

# -----------------------------------------------------------------------------
# variance(x, n = NULL)
# -----------------------------------------------------------------------------
# WHAT IT DOES : (Population) variance: mean of the squared deviations from
#                the mean. Divides by the total number of data.   !! R's var() divides by the total number of data-1 !!
# FORMULA      : V = sigma^2 = sum((x_i - mu)^2 * n_i) / N
# PARAMETERS   :
#   x : raw data, or the different values (or class marks).
#   n : (optional) how many times each one appears (its frequency).
# RETURNS      : One number.
# EXAMPLE      :
#   variance(c(12, 10, 9, 9, 10))     # 1.2
#   variance(c(5, 10, 16, 15, 4))     # 24.4
# -----------------------------------------------------------------------------
variance <- function(x, n = NULL) {
  n  <- freq_or_ones(x, n)
  mu <- arithmetic_mean(x, n)
  sum((x - mu)^2 * n) / sum(n)
}

# -----------------------------------------------------------------------------
# variance_shortcut(x, n = NULL)
# -----------------------------------------------------------------------------
# WHAT IT DOES : Same variance, computed with the equivalent "shortcut"
#                formula of the slides (mean of squares minus square of mean).
# FORMULA      : V = sum(n_i * x_i^2) / N  -  mu^2
# PARAMETERS   : same as variance().
# RETURNS      : One number (same result as variance()).
# EXAMPLE      :  variance_shortcut(c(12, 10, 9, 9, 10))     # 1.2
# -----------------------------------------------------------------------------
variance_shortcut <- function(x, n = NULL) {
  n <- freq_or_ones(x, n)
  sum(n * x^2) / sum(n) - arithmetic_mean(x, n)^2
}

# -----------------------------------------------------------------------------
# standard_deviation(x, n = NULL)
# -----------------------------------------------------------------------------
# WHAT IT DOES : (Population) standard deviation = square root of variance.
#                Divides by the total number of data.   !! R's sd() divides by the total number of data-1 !!
# FORMULA      : sigma = sqrt(V)
# PARAMETERS   :
#   x : raw data, or the different values.
#   n : (optional) how many times each one appears (its frequency).
# RETURNS      : One number.
# EXAMPLE      :
#   standard_deviation(c(12, 10, 9, 9, 10))   # 1.095445  (List A: concentrated)
#   standard_deviation(c(5, 10, 16, 15, 4))   # 4.939636  (List B: dispersed)
# -----------------------------------------------------------------------------
standard_deviation <- function(x, n = NULL) {
  sqrt(variance(x, n))
}

# -----------------------------------------------------------------------------
# sample_variance(x, n = NULL)
# -----------------------------------------------------------------------------
# WHAT IT DOES : Sample variance s_n^2 of a sample of size n.
#                Same formula as variance() (divides by n).
# FORMULA      : s_n^2 = sum((x_i - x_bar)^2 * n_i) / n
# PARAMETERS   : same as variance().
# RETURNS      : One number.
# EXAMPLE      :  sample_variance(c(12, 10, 9, 9, 10))    # 1.2
# -----------------------------------------------------------------------------
sample_variance <- function(x, n = NULL) {
  variance(x, n)
}

# -----------------------------------------------------------------------------
# sample_sd(x, n = NULL)
# -----------------------------------------------------------------------------
# WHAT IT DOES : Sample standard deviation s_n = sqrt(s_n^2)  (divides by n).
# PARAMETERS   : same as variance().
# RETURNS      : One number.
# EXAMPLE      :  sample_sd(c(12, 10, 9, 9, 10))    # 1.095445
# -----------------------------------------------------------------------------
sample_sd <- function(x, n = NULL) {
  sqrt(sample_variance(x, n))
}

# -----------------------------------------------------------------------------
# sample_quasi_variance(x, n = NULL)
# -----------------------------------------------------------------------------
# WHAT IT DOES : Sample QUASI-variance s^2. Divides by (n - 1).
#                It is the BEST ESTIMATOR of the population variance.
#                Same result as R's built-in var() on raw data.
# FORMULA      : s^2 = sum((x_i - x_bar)^2 * n_i) / (n - 1)
# PARAMETERS   :
#   x : raw data, or the different values.
#   n : (optional) how many times each one appears (its frequency).
# RETURNS      : One number.
# EXAMPLE      :  sample_quasi_variance(c(12, 10, 9, 9, 10))    # 1.5
# -----------------------------------------------------------------------------
sample_quasi_variance <- function(x, n = NULL) {
  n     <- freq_or_ones(x, n)
  x_bar <- arithmetic_mean(x, n)
  sum((x - x_bar)^2 * n) / (sum(n) - 1)
}

# -----------------------------------------------------------------------------
# sample_quasi_sd(x, n = NULL)
# -----------------------------------------------------------------------------
# WHAT IT DOES : Sample quasi-standard deviation s = sqrt(s^2).
#                Same result as R's built-in sd() on raw data.
# PARAMETERS   : same as sample_quasi_variance().
# RETURNS      : One number.
# EXAMPLE      :  sample_quasi_sd(c(12, 10, 9, 9, 10))    # 1.224745
# -----------------------------------------------------------------------------
sample_quasi_sd <- function(x, n = NULL) {
  sqrt(sample_quasi_variance(x, n))
}

# -----------------------------------------------------------------------------
# quasi_variance_from_sample_variance(sn2, size)
# -----------------------------------------------------------------------------
# WHAT IT DOES : Converts a sample variance s_n^2 into the quasi-variance s^2.
# FORMULA      : s^2 = n / (n - 1) * s_n^2
# PARAMETERS   :
#   sn2  : the sample variance s_n^2 (a number).
#   size : the sample size n (a number).
# RETURNS      : One number.
# EXAMPLE      :  quasi_variance_from_sample_variance(1.2, 5)    # 1.5
# -----------------------------------------------------------------------------
quasi_variance_from_sample_variance <- function(sn2, size) {
  size / (size - 1) * sn2
}


# =============================================================================
# 7. COMPARISON MEASURES
# =============================================================================

# -----------------------------------------------------------------------------
# standardize(value, mean, sd)
# -----------------------------------------------------------------------------
# WHAT IT DOES : Standardised value (z-score): how many standard deviations
#                a value is above (+) or below (-) the mean. It has no units,
#                so it lets you compare values from different distributions.
# FORMULA      : z = (x - mu) / sigma
# PARAMETERS   :
#   value : the value(s) x to standardise (one number or a vector).
#   mean  : the mean of its distribution.
#   sd    : the standard deviation of its distribution.
# RETURNS      : The z value(s).
# EXAMPLE      :  (students A and B)
#   standardize(8.5, mean = 7.9, sd = 0.8)    # 0.75
#   standardize(7.4, mean = 7.0, sd = 0.5)    # 0.8   -> B did better
# -----------------------------------------------------------------------------
standardize <- function(value, mean, sd) {
  (value - mean) / sd
}

# -----------------------------------------------------------------------------
# standardize_data(x, n = NULL)
# -----------------------------------------------------------------------------
# WHAT IT DOES : Standardises every value of a data set, computing the mean
#                and (population) standard deviation from the data itself.
# FORMULA      : z_i = (x_i - mu) / sigma
# PARAMETERS   :
#   x : raw data, or the different values.
#   n : (optional) the absolute frequency of each value (used to compute the mean and the standard deviation).
# RETURNS      : Vector with z_i (one per element of x).
# EXAMPLE      :  standardize_data(c(12, 10, 9, 9, 10))
# -----------------------------------------------------------------------------
standardize_data <- function(x, n = NULL) {
  (x - arithmetic_mean(x, n)) / standard_deviation(x, n)
}

# -----------------------------------------------------------------------------
# coefficient_of_variation(x, n = NULL, percent = TRUE)
# -----------------------------------------------------------------------------
# WHAT IT DOES : Pearson's coefficient of variation, computed from DATA.
#                Relative dispersion (no units) -> compare the dispersion of
#                samples with different units or very different means.
#                Not defined if the mean is 0.
# FORMULA      : CV = sigma / |mu|      (x 100 if in percentage)
# PARAMETERS   :
#   x       : raw data, or the different values.
#   n       : (optional) how many times each one appears (its frequency).
#   percent : TRUE (default) gives it in %, FALSE as a ratio.
# RETURNS      : One number.
# EXAMPLE      :  coefficient_of_variation(c(12, 10, 9, 9, 10))   # 10.95445 %
# -----------------------------------------------------------------------------
coefficient_of_variation <- function(x, n = NULL, percent = TRUE) {
  coefficient_of_variation_from_values(standard_deviation(x, n),
                                       arithmetic_mean(x, n), percent)
}

# -----------------------------------------------------------------------------
# coefficient_of_variation_from_values(sd, mean, percent = TRUE)
# -----------------------------------------------------------------------------
# WHAT IT DOES : Same coefficient, when you ALREADY KNOW the standard deviation and the mean.
# FORMULA      : CV = sigma / |mu|      (x 100 if in percentage)
# PARAMETERS   :
#   sd      : standard deviation the standard deviation.
#   mean    : mean the mean (must not be 0).
#   percent : TRUE (default) -> %, FALSE -> ratio.
# RETURNS      : One number.
# EXAMPLE      :  (TV tubes)
#   coefficient_of_variation_from_values(280, 1495)   # 18.73 %  (A)
#   coefficient_of_variation_from_values(310, 1875)   # 16.53 %  (B)
# -----------------------------------------------------------------------------
coefficient_of_variation_from_values <- function(sd, mean, percent = TRUE) {
  if (mean == 0) stop("The CV is not defined when the mean is 0.")
  cv <- sd / abs(mean)
  if (percent) cv * 100 else cv
}


# =============================================================================
# 8. MOMENTS
# =============================================================================

# -----------------------------------------------------------------------------
# moment_about_point(x, r, c, n = NULL)
# -----------------------------------------------------------------------------
# WHAT IT DOES : Moment of order r with respect to a point c.
# FORMULA      : m_r(c) = sum(n_i * (x_i - c)^r) / N
# PARAMETERS   :
#   x : raw data, or the different values.
#   r : the order (0, 1, 2, 3, ...).
#   c : the point (a number), e.g. the median.
#   n : (optional) how many times each one appears (its frequency).
# RETURNS      : One number.
# EXAMPLE      :  (slide 64, Me = 2.5)
#   d <- c(0, 0, 2, 2, 2, 2, 3, 3, 4, 4, 7, 7)
#   moment_about_point(d, 1, median_value(d))    # m1(Me) = 0.5
#   moment_about_point(d, 2, median_value(d))    # m2(Me) = 4.916667
# -----------------------------------------------------------------------------
moment_about_point <- function(x, r, c, n = NULL) {
  n <- freq_or_ones(x, n)
  sum(n * (x - c)^r) / sum(n)
}

# -----------------------------------------------------------------------------
# ordinary_moment(x, r, n = NULL)
# -----------------------------------------------------------------------------
# WHAT IT DOES : Ordinary moment of order r = moment about c = 0 = mean of
#                the r-th powers.   m0 = 1,  m1 = mean,  m2 = the variance + the mean^2.
# FORMULA      : m_r = sum(n_i * x_i^r) / N
# PARAMETERS   :
#   x : raw data, or the different values.
#   r : the order (0, 1, 2, 3, ...).
#   n : (optional) how many times each one appears (its frequency).
# RETURNS      : One number.
# EXAMPLE      :  ordinary_moment(c(12, 10, 9, 9, 10), 2)    # 101.2
# -----------------------------------------------------------------------------
ordinary_moment <- function(x, r, n = NULL) {
  moment_about_point(x, r, 0, n)
}

# -----------------------------------------------------------------------------
# central_moment(x, r, n = NULL)
# -----------------------------------------------------------------------------
# WHAT IT DOES : Central moment of order r = moment about the mean.
#                mu_0 = 1,  mu_1 = 0,  mu_2 = variance.
# FORMULA      : mu_r = sum(n_i * (x_i - mu)^r) / N
# PARAMETERS   :
#   x : raw data, or the different values.
#   r : the order (0, 1, 2, 3, ...).
#   n : (optional) how many times each one appears (its frequency).
# RETURNS      : One number.
# EXAMPLE      :  central_moment(c(12, 10, 9, 9, 10), 2)    # 1.2 (= variance)
# -----------------------------------------------------------------------------
central_moment <- function(x, r, n = NULL) {
  moment_about_point(x, r, arithmetic_mean(x, n), n)
}

# -----------------------------------------------------------------------------
# variance_from_ordinary_moments(m1, m2)
# central_moment3_from_ordinary(m1, m2, m3)
# central_moment4_from_ordinary(m1, m2, m3, m4)
# -----------------------------------------------------------------------------
# WHAT THEY DO : Compute central moments using ONLY ordinary moments
#                (useful for Big Data: ordinary moments can be computed in
#                parallel / incrementally). Remember m1 = the mean.
# FORMULAS     : mu_2 = m2 - mu^2
#                mu_3 = m3 - 3*m2*mu + 2*mu^3
#                mu_4 = m4 - 4*m3*mu + 6*m2*mu^2 - 3*mu^4
# PARAMETERS   :
#   m1, m2, m3, m4 : ordinary moments of order 1, 2, 3, 4 (numbers),
#                    e.g. from ordinary_moment(x, 3).
# RETURNS      : One number.
# EXAMPLE      :
#   v  <- c(12, 10, 9, 9, 10)
#   m  <- sapply(1:4, function(r) ordinary_moment(v, r))
#   variance_from_ordinary_moments(m[1], m[2])               # 1.2
#   central_moment3_from_ordinary(m[1], m[2], m[3])          # = central_moment(v, 3)
#   central_moment4_from_ordinary(m[1], m[2], m[3], m[4])    # = central_moment(v, 4)
# -----------------------------------------------------------------------------
variance_from_ordinary_moments <- function(m1, m2) {
  m2 - m1^2
}
central_moment3_from_ordinary <- function(m1, m2, m3) {
  m3 - 3 * m2 * m1 + 2 * m1^3
}
central_moment4_from_ordinary <- function(m1, m2, m3, m4) {
  m4 - 4 * m3 * m1 + 6 * m2 * m1^2 - 3 * m1^4
}


# =============================================================================
# 9. SHAPE MEASURES: SKEWNESS AND KURTOSIS
# =============================================================================

# -----------------------------------------------------------------------------
# pearson_skewness(x, n = NULL, mode = NULL)
# -----------------------------------------------------------------------------
# WHAT IT DOES : Pearson's skewness coefficient.
#                > 0 right/positive skew, = 0 symmetric, < 0 left/negative.
# FORMULA      : A_P = (mu - Mo) / sigma
# PARAMETERS   :
#   x    : raw data, or the different values.
#   n    : (optional) how many times each one appears (its frequency).
#   mode : (optional) the mode to use. If empty it is computed with
#          mode_value(). If the data have SEVERAL modes you MUST choose one
#          and give it here; if there is NO mode the result is NA.
# RETURNS      : One number.
# EXAMPLE      :  pearson_skewness(c(1, 2, 2, 2, 3, 4, 9))    # 0.5162 (> 0: right)
# -----------------------------------------------------------------------------
pearson_skewness <- function(x, n = NULL, mode = NULL) {
  if (is.null(mode)) {
    mode <- mode_value(x, n)
    if (length(mode) > 1) stop("Several modes found: ", paste(mode, collapse = ", "),
                               ". Choose one and pass it as `mode =`.")
    if (is.na(mode)) { warning("The data have no mode."); return(NA) }
  }
  (arithmetic_mean(x, n) - mode) / standard_deviation(x, n)
}

# -----------------------------------------------------------------------------
# fisher_skewness(x, n = NULL)
# -----------------------------------------------------------------------------
# WHAT IT DOES : Fisher's skewness coefficient (compared with the normal
#                distribution, which has g1 = 0).
#                > 0 right/positive skew, = 0 symmetric, < 0 left/negative.
# FORMULA      : g1 = mu_3 / sigma^3
# PARAMETERS   :
#   x : raw data, or the different values.
#   n : (optional) how many times each one appears (its frequency).
# RETURNS      : One number.
# EXAMPLE      :  fisher_skewness(c(1, 2, 2, 2, 3, 4, 9))    # 1.5587 (> 0: right)
# -----------------------------------------------------------------------------
fisher_skewness <- function(x, n = NULL) {
  central_moment(x, 3, n) / standard_deviation(x, n)^3
}

# -----------------------------------------------------------------------------
# kurtosis_coefficient(x, n = NULL)
# -----------------------------------------------------------------------------
# WHAT IT DOES : Fisher's kurtosis coefficient: how peaked / flat the
#                distribution is compared with a normal distribution
#                (which has g2 = 0).
#                < 0 platykurtic (flatter), = 0 mesokurtic,
#                > 0 leptokurtic (more peaked).
# FORMULA      : g2 = mu_4 / sigma^4 - 3
# PARAMETERS   :
#   x : raw data, or the different values.
#   n : (optional) how many times each one appears (its frequency).
# RETURNS      : One number.
# EXAMPLE      :  kurtosis_coefficient(c(1, 2, 3, 4, 5))    # -1.3 (platykurtic)
# -----------------------------------------------------------------------------
kurtosis_coefficient <- function(x, n = NULL) {
  central_moment(x, 4, n) / standard_deviation(x, n)^4 - 3
}

# -----------------------------------------------------------------------------
# interpret_skewness(value, tolerance = 1e-8)
# -----------------------------------------------------------------------------
# WHAT IT DOES : Translates a skewness coefficient (A_P or g1) into words.
# PARAMETERS   :
#   value     : the coefficient.
#   tolerance : values closer to 0 than this count as 0 (default 1e-8,
#               i.e. only exact zeros; use e.g. 0.1 to be more lenient).
# RETURNS      : Text.
# EXAMPLE      :  interpret_skewness(1.65)   # "Right or positive skew"
# -----------------------------------------------------------------------------
interpret_skewness <- function(value, tolerance = 1e-8) {
  if (abs(value) <= tolerance) "Symmetric"
  else if (value > 0) "Right or positive skew"
  else "Left or negative skew"
}

# -----------------------------------------------------------------------------
# interpret_kurtosis(value, tolerance = 1e-8)
# -----------------------------------------------------------------------------
# WHAT IT DOES : Translates the kurtosis coefficient g2 into words.
# PARAMETERS   :
#   value     : the coefficient g2.
#   tolerance : values closer to 0 than this count as 0.
# RETURNS      : Text.
# EXAMPLE      :  interpret_kurtosis(-1.3)   # "Platykurtic (less peaked than the normal)"
# -----------------------------------------------------------------------------
interpret_kurtosis <- function(value, tolerance = 1e-8) {
  if (abs(value) <= tolerance) "Mesokurtic (same peakedness as the normal)"
  else if (value > 0) "Leptokurtic (more peaked than the normal)"
  else "Platykurtic (less peaked than the normal)"
}


# =============================================================================
# 10. EVERYTHING AT ONCE
# =============================================================================

# -----------------------------------------------------------------------------
# describe_all(x, n = NULL)
# -----------------------------------------------------------------------------
# WHAT IT DOES : Computes (almost) every statistic of the chapter at once
#                and prints them in a readable list. Handy to check exercises.
# PARAMETERS   :
#   x : raw numeric data, or the different values (or class marks).
#   n : (optional) how many times each one appears (its frequency).
# RETURNS      : A named list with all the results (also printed).
# EXAMPLE      :  describe_all(c(2, 5, 3, 4, 7, 0, 11, 2, 3, 8))
# -----------------------------------------------------------------------------
describe_all <- function(x, n = NULL) {
  modes <- mode_value(x, n)
  res <- list(
    N                        = sum(freq_or_ones(x, n)),
    mean                     = arithmetic_mean(x, n),
    quadratic_mean           = quadratic_mean(x, n),
    mode                     = modes,
    median                   = median_value(x, n),
    Q1                       = quartile(x, 1, n),
    Q3                       = quartile(x, 3, n),
    range                    = range_value(x),
    interquartile_range      = interquartile_range(x, n),
    mean_deviation           = mean_deviation(x, n),
    variance                 = variance(x, n),
    standard_deviation       = standard_deviation(x, n),
    sample_quasi_variance    = sample_quasi_variance(x, n),
    sample_quasi_sd          = sample_quasi_sd(x, n),
    coefficient_of_variation = if (arithmetic_mean(x, n) != 0)
      coefficient_of_variation(x, n) else NA,
    pearson_skewness         = if (length(modes) == 1 && !is.na(modes[1]))
      pearson_skewness(x, n, modes) else NA,
    fisher_skewness          = fisher_skewness(x, n),
    kurtosis                 = kurtosis_coefficient(x, n)
  )
  for (nm in names(res)) cat(sprintf("%-26s: %s\n", nm,
                                     paste(signif(res[[nm]], 7), collapse = ", ")))
  invisible(res)
}