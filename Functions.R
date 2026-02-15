##########################################
#' Plot for the PDF of a Probability distribution.
#'
#' Plots the PDF of a probability distribution.
#' Supports multiple sets of parameters with shaded areas under the curves.
#'
#' @param pdf Function that computes the PDF. Must accept x as the first argument.
#' @param param_list A list of parameter lists. Each element is a named list of parameters for \code{pdf}.
#' @param xlim Numeric vector of length 2, specifying the x-axis limits. Default is c(0, 10).
#' @param ylim Numeric vector of length 2, specifying the y-axis limits. Default is c(0, 6).
#' @param n Number of points to evaluate on the x-axis. Default is 500.
#' @param main Title of the plot. Default is "PDF Plot".
#' @param xlab Label for the x-axis. Default is "x".
#' @param ylab Label for the y-axis. Default is "Density".
#' @param colors Vector of colors for the lines. Default is \code{rainbow}.
#' @param shade_colors Vector of colors for shading under curves. Default is semi-transparent version of \code{colors}.
#' @param lwd Line width. Default is 3.
#' @param lty Line type. Default is 2 (dashed).
#' @param grid Logical, whether to draw a grid. Default is TRUE.
#' @param grid_lty Line type for grid. Default is 3.
#' @param grid_col Grid color. Default is "gray80".
#' @param grid_lwd Grid line width. Default is 1.
#' @importFrom graphics plot lines legend polygon grid axis curve
#' @importFrom grDevices rainbow adjustcolor col2rgb rgb
#' @return A PDF plot is displayed. The function invisibly returns NULL.
#' @examples
#' # Example 1 with Generalized Exponential Distribution
#' ge_pdf <- function(x, alpha, lambda) {
#' alpha * lambda * exp(-lambda * x) * (1 - exp(-lambda * x))^(alpha - 1)
#'}
#'param_values <- list(list(alpha = 1, lambda = 1),
#'                     list(alpha = 2, lambda = 1),
#'                     list(alpha = 3, lambda = 0.5),
#'                     list(alpha = 4, lambda = 1.5),
#'                     list(alpha = 5, lambda = 2.5))
#'plot_pdf(pdf = ge_pdf, param_list = param_values, ylim = c(0, 1),
#'         main = "Generalized Exponential Distribution")
#'
#' # Example 2 with Exponentiated Weibull Distribution
#' pdf_expweibull <- function(x, a, b, c){
#'a * b * c * exp(-(b*x)^c) *
#'  (b*x)^(c-1) * (1 - exp(-(b*x)^c))^(a-1)
#'}
#'param_values <- list(list(a = 0.3, b = 1.2, c = 1.0),
#'                     list(a = 1.3, b = 0.4, c = 2.3),
#'                     list(a = 1.5, b = 0.9, c = 3.0),
#'                     list(a = 2.0, b = 1.8, c = 2.8),
#'                     list(a = 3.7, b = 2.0, c = 1.5))
#'colors <- c("green", "purple", "yellow", "orange", "darkblue")
#'plot_pdf(pdf = pdf_expweibull, param_list = param_values,
#'         main = "PDF of EW Distribution",
#'         colors = colors, xlim = c(0, 5), ylim = c(0, 3))
#'
#' @export
plot_pdf <- function(
    pdf,
    param_list,
    xlim = c(0, 10),
    ylim = c(0, 6),
    n = 500,
    main = "PDF Plot",
    xlab = "x",
    ylab = "Density",
    colors = NULL,
    shade_colors = NULL,
    lwd = 3,
    lty = 2,
    grid = TRUE,
    grid_lty = 3,
    grid_col = "gray80",
    grid_lwd = 1
){
  x <- seq(xlim[1], xlim[2], length.out = n)
  k <- length(param_list)
  if (is.null(colors)) colors <- rainbow(k)
  if (is.null(shade_colors)) shade_colors <- adjustcolor(colors, alpha.f = 0.25)
  first_y <- do.call(pdf, c(list(x), param_list[[1]]))
  plot(x, first_y, type="n", xlab=xlab, ylab=ylab, main=main,
       ylim = ylim)
  if (grid) {
    grid(nx = NA, ny = NULL, lty = grid_lty, col = grid_col, lwd = grid_lwd)
  }
  for (i in seq_along(param_list)) {
    params <- param_list[[i]]
    y <- do.call(pdf, c(list(x), params))
    polygon(
      c(x, rev(x)),
      c(y, rep(0, length(y))),
      col = shade_colors[i],
      border = NA
    )
    lines(x, y, col = colors[i], lwd = lwd, lty = lty)
  }
  legend_labels <- sapply(param_list, function(p) {
    paste(names(p), "=", p, collapse = ", ")
  })
  legend("topright", legend = legend_labels,
         col = colors, lwd = lwd, lty = lty)
}


#' Plot CDF for the Probability of a Distribution
#'
#' #' Plots the CDF of a probability distribution.
#' Supports multiple sets of parameters with shaded areas under the curves.
#'
#' @param cdf Function that computes the CDF. Must accept x as the first argument.
#' @param param_list A list of parameter lists. Each element is a named list of parameters for \code{cdf}.
#' @param xlim Numeric vector of length 2, specifying the x-axis limits. Default is c(0, 10).
#' @param ylim Numeric vector of length 2, specifying the y-axis limits. Default is c(0, 1).
#' @param n Number of points to evaluate on the x-axis. Default is 500.
#' @param main Title of the plot. Default is "Custom CDF Plot".
#' @param xlab Label for the x-axis. Default is "x".
#' @param ylab Label for the y-axis. Default is "CDF".
#' @param colors Vector of colors for the lines. Default is \code{rainbow}.
#' @param shade_colors Vector of colors for shading under curves. Default is semi-transparent version of \code{colors}.
#' @param lwd Line width. Default is 3.
#' @param lty Line type. Default is 2 (dashed).
#' @param grid Logical, whether to draw a grid. Default is TRUE.
#' @param grid_lty Line type for grid. Default is 3.
#' @param grid_col Grid color. Default is "gray80".
#' @param grid_lwd Grid line width. Default is 1.
#' @importFrom graphics plot lines legend polygon grid axis curve
#' @importFrom grDevices rainbow adjustcolor col2rgb rgb

#' @return A CDF plot is displayed. The function invisibly returns NULL.
#' @examples
#' # Example 1: Generalized Exponential Distribution
#' ge_cdf <- function(x, alpha, lambda) {
#'   (1 - exp(-lambda * x))^alpha
#' }
#' param_values <- list(
#'   list(alpha = 1, lambda = 1),
#'   list(alpha = 2, lambda = 1),
#'   list(alpha = 3, lambda = 0.5),
#'   list(alpha = 4, lambda = 1.5),
#'   list(alpha = 5, lambda = 2.5)
#' )
#' plot_cdf(cdf = ge_cdf, param_list = param_values, main = "CDF GE Distribution")
#'
#' # Example 2: Exponentiated Weibull Distribution
#' cdf_expweibull <- function(x, a, b, c){
#'   (1 - exp(-(b*x)^c))^a
#' }
#' param_values <- list(
#'   list(a = 0.3, b = 1.2, c = 1.0),
#'   list(a = 1.3, b = 0.4, c = 2.3),
#'   list(a = 1.5, b = 0.9, c = 3.0),
#'   list(a = 2.0, b = 1.8, c = 2.8),
#'   list(a = 3.7, b = 2.0, c = 1.5)
#' )
#' colors <- c("green", "purple", "yellow", "orange", "darkblue")
#' plot_cdf(cdf = cdf_expweibull, param_list = param_values,
#'          main = "CDF of EW Distribution", colors = colors, xlim = c(0, 5))
#'
#' @export
plot_cdf <- function(
    cdf,
    param_list,
    xlim = c(0, 10),
    ylim = c(0, 1),
    n = 500,
    main = "Custom CDF Plot",
    xlab = "x",
    ylab = "CDF",
    colors = NULL,
    shade_colors = NULL,
    lwd = 3,
    lty = 2,
    grid = TRUE,
    grid_lty = 3,
    grid_col = "gray80",
    grid_lwd = 1
){
  x <- seq(xlim[1], xlim[2], length.out = n)
  k <- length(param_list)
  if (is.null(colors)) colors <- rainbow(k)
  if (is.null(shade_colors)) shade_colors <- adjustcolor(colors, alpha.f = 0.25)

  F0 <- do.call(cdf, c(list(x), param_list[[1]]))
  plot(x, F0, type = "n", ylim = ylim, xlab = xlab, ylab = ylab, main = main)

  if (grid) grid(nx = NA, ny = NULL, lty = grid_lty, col = grid_col, lwd = grid_lwd)

  for (i in seq_along(param_list)) {
    params <- param_list[[i]]
    F <- do.call(cdf, c(list(x), params))
    polygon(
      c(x, rev(x)),
      c(F, rep(0, length(F))),
      col = shade_colors[i],
      border = NA
    )
    lines(x, F, col = colors[i], lwd = lwd, lty = lty)
  }

  legend(
    "bottomright",
    legend = sapply(param_list, function(p) paste(names(p), "=", p, collapse = ", ")),
    col = colors, lwd = lwd, lty = lty
  )
}


#' Plot for the Survival Function of a Probability Distribution
#'
#' #' Plots the SF of a probability distribution.
#' Supports multiple sets of parameters with shaded areas under the curves.
#'
#' @param sf Function that computes the SF. Must accept x as the first argument.
#' @param param_list A list of parameter lists. Each element is a named list of parameters for \code{sf}.
#' @param xlim Numeric vector of length 2, specifying the x-axis limits. Default is c(0, 10).
#' @param ylim Numeric vector of length 2, specifying the y-axis limits. Default is c(0, 1).
#' @param n Number of points to evaluate on the x-axis. Default is 500.
#' @param main Title of the plot. Default is "SF Plot".
#' @param xlab Label for the x-axis. Default is "x".
#' @param ylab Label for the y-axis. Default is "SF".
#' @param colors Vector of colors for the lines. Default is \code{rainbow}.
#' @param shade_colors Vector of colors for shading under curves. Default is semi-transparent version of \code{colors}.
#' @param lwd Line width. Default is 3.
#' @param lty Line type. Default is 2 (dashed).
#' @param grid Logical, whether to draw a grid. Default is TRUE.
#' @param grid_lty Line type for grid. Default is 3.
#' @param grid_col Grid color. Default is "gray80".
#' @param grid_lwd Grid line width. Default is 1.
#' @importFrom graphics plot lines legend polygon grid axis curve
#' @importFrom grDevices rainbow adjustcolor col2rgb rgb
#' @return A SF plot is displayed. The function invisibly returns NULL.
#' @examples
#' # Example 1: Generalized Exponential Distribution
#' ge_sf <- function(x, alpha, lambda) {
#'   1 - (1 - exp(-lambda * x))^alpha
#' }
#' param_values <- list(
#'   list(alpha = 1, lambda = 1),
#'   list(alpha = 2, lambda = 1),
#'   list(alpha = 3, lambda = 0.5),
#'   list(alpha = 4, lambda = 1.5),
#'   list(alpha = 5, lambda = 2.5)
#' )
#' plot_sf(sf = ge_sf, param_list = param_values, main = "SF GE Distribution")
#'
#' # Example 2: Exponentiated Weibull Distribution
#' sf_expweibull <- function(x, a, b, c) {
#'   1 - (1 - exp(-(b*x)^c))^a
#' }
#' param_values <- list(
#'   list(a = 0.3, b = 1.2, c = 1.0),
#'   list(a = 1.3, b = 0.4, c = 2.3),
#'   list(a = 1.5, b = 0.9, c = 3.0),
#'   list(a = 2.0, b = 1.8, c = 2.8),
#'   list(a = 3.7, b = 2.0, c = 1.5)
#' )
#' colors <- c("green", "purple", "yellow", "orange", "darkblue")
#' plot_sf(sf = sf_expweibull, param_list = param_values,
#'         main = "SF of EW Distribution", colors = colors, xlim = c(0, 5))
#'
#' @export
plot_sf <- function(
    sf,
    param_list,
    xlim = c(0, 10),
    ylim = c(0, 1),
    n = 500,
    main = "SF Plot",
    xlab = "x",
    ylab = "SF",
    colors = NULL,
    shade_colors = NULL,
    lwd = 3,
    lty = 2,
    grid = TRUE,
    grid_lty = 3,
    grid_col = "gray80",
    grid_lwd = 1
){
  x <- seq(xlim[1], xlim[2], length.out = n)
  k <- length(param_list)
  if (is.null(colors)) colors <- rainbow(k)
  if (is.null(shade_colors)) shade_colors <- adjustcolor(colors, alpha.f = 0.25)

  first_y <- do.call(sf, c(list(x), param_list[[1]]))
  plot(x, first_y, type="n", xlab=xlab, ylab=ylab, main=main, xlim = xlim, ylim = ylim)

  if (grid) grid(nx = NA, ny = NULL, lty = grid_lty, col = grid_col, lwd = grid_lwd)

  for(i in seq_along(param_list)){
    params <- param_list[[i]]
    y <- do.call(sf, c(list(x), params))
    polygon(c(x, rev(x)), c(y, rep(0, length(y))), col = shade_colors[i], border = NA)
    lines(x, y, col = colors[i], lwd = lwd, lty = lty)
  }

  legend_labels <- sapply(param_list, function(p) paste(names(p), "=", p, collapse = ", "))
  legend("topright", legend = legend_labels, col = colors, lwd = lwd, lty = lty)
}


#' Plot Hazard Function of a Probability Distribution
#'
#' #' Plots the PDF of a probability distribution.
#' Supports multiple sets of parameters with shaded areas under the curves.
#'
#' @param pdf Function that computes the PDF. Must accept x as the first argument.
#' @param cdf Function that computes the CDF. Must accept x as the first argument.
#' @param param_list A list of parameter lists. Each element is a named list of parameters for \code{pdf} and \code{cdf}.
#' @param xlim Numeric vector of length 2, specifying the x-axis limits. Default is c(0, 10).
#' @param ylim Numeric vector of length 2, specifying the y-axis limits. Default is c(0, 6).
#' @param n Number of points to evaluate on the x-axis. Default is 500.
#' @param main Title of the plot. Default is "Hazard Function Plot".
#' @param xlab Label for the x-axis. Default is "x".
#' @param ylab Label for the y-axis. Default is "h(x)".
#' @param colors Vector of colors for the lines. Default is \code{rainbow}.
#' @param shade_colors Vector of colors for shading under curves. Default is semi-transparent version of \code{colors}.
#' @param lwd Line width. Default is 3.
#' @param lty Line type. Default is 2 (dashed).
#' @param grid Logical, whether to draw a grid. Default is TRUE.
#' @param grid_lty Line type for grid. Default is 3.
#' @param grid_col Grid color. Default is "gray80".
#' @param grid_lwd Grid line width. Default is 1.
#' @importFrom graphics plot lines legend polygon grid axis curve
#' @importFrom grDevices rainbow adjustcolor col2rgb rgb
#' @return A hazard function plot is displayed. The function invisibly returns NULL.
#' @examples
#' # Example 1: Generalized Exponential Distribution
#' pdf_ge <- function(x, alpha, lambda) {
#'   alpha * lambda * exp(-lambda*x) * (1 - exp(-lambda*x))^(alpha - 1)
#' }
#' cdf_ge <- function(x, alpha, lambda) {
#'   1 - (1 - exp(-lambda*x))^alpha
#' }
#' param_values <- list(
#'   list(alpha = 1, lambda = 1),
#'   list(alpha = 2, lambda = 1),
#'   list(alpha = 3, lambda = 0.5),
#'   list(alpha = 4, lambda = 1.5),
#'   list(alpha = 5, lambda = 2.5)
#' )
#' plot_hf(pdf_ge, cdf_ge, param_values, xlim=c(0,5), ylim=c(0,4), main="HF GE Distribution")
#'
#' # Example 2: Exponentiated Weibull Distribution
#' pdf_expweibull <- function(x, a, b, c){
#'   a * b * c * exp(-(b*x)^c) * (b*x)^(c-1) * (1 - exp(-(b*x)^c))^(a-1)
#' }
#' cdf_expweibull <- function(x, a, b, c){
#'   1 - (1 - exp(-(b*x)^c))^a
#' }
#' param_values <- list(
#'   list(a = 0.3, b = 1.2, c = 1.0),
#'   list(a = 1.3, b = 0.4, c = 2.3),
#'   list(a = 1.5, b = 0.9, c = 3.0),
#'   list(a = 2.0, b = 1.8, c = 2.8),
#'   list(a = 3.7, b = 2.0, c = 1.5)
#' )
#' plot_hf(pdf_expweibull, cdf_expweibull, param_values)
#'
#' @export
plot_hf <- function(
    pdf,
    cdf,
    param_list,
    xlim = c(0, 10),
    ylim = c(0, 6),
    n = 500,
    main = "Hazard Function Plot",
    xlab = "x",
    ylab = "h(x)",
    colors = NULL,
    shade_colors = NULL,
    lwd = 3,
    lty = 2,
    grid = TRUE,
    grid_lty = 3,
    grid_col = "gray80",
    grid_lwd = 1
){
  x <- seq(xlim[1], xlim[2], length.out = n)
  k <- length(param_list)
  if (is.null(colors)) colors <- rainbow(k)
  if (is.null(shade_colors)) shade_colors <- adjustcolor(colors, alpha.f = 0.25)

  p1 <- do.call(pdf, c(list(x), param_list[[1]]))
  F1 <- do.call(cdf, c(list(x), param_list[[1]]))
  h1 <- p1 / (1 - F1)

  plot(x, h1, type="n", xlab=xlab, ylab=ylab, main=main, ylim=ylim)

  if (grid) grid(nx = NA, ny = NULL, lty = grid_lty, col = grid_col, lwd = grid_lwd)

  for (i in seq_along(param_list)) {
    params <- param_list[[i]]
    p <- do.call(pdf, c(list(x), params))
    F <- do.call(cdf, c(list(x), params))
    h <- p / (1 - F)
    h[!is.finite(h)] <- NA

    polygon(c(x, rev(x)), c(h, rep(0, length(h))), col = shade_colors[i], border = NA)
    lines(x, h, col = colors[i], lwd = lwd, lty = lty)
  }

  legend_labels <- sapply(param_list, function(p) paste(names(p), "=", p, collapse = ", "))
  legend("topright", legend = legend_labels, col = colors, lwd = lwd, lty = lty)
}


#' Compute Moments of a Probability Distribution
#'
#' Calculates mean, variance, skewness, kurtosis, median, and mode
#' for a given probability distribution defined by PDF and CDF.
#'
#' @param pdf Function for the probability density function (PDF).
#'            Must accept \code{x} as the first argument and parameters as a named list.
#' @param cdf Function for the cumulative distribution function (CDF).
#'            Must accept \code{x} as the first argument and parameters as a named list.
#' @param support Numeric vector of length 2. The support (lower and upper limits) of the distribution.
#' @param params Named list of parameters to pass to \code{pdf} and \code{cdf}.
#' @importFrom stats integrate var uniroot
#' @return A data frame with Mean, Variance, Skewness, Kurtosis, Median, and Mode.
#'
#' @examples
#' # Generalized Exponential Distribution
#' pdf_ge <- function(x, alpha, lambda) {
#'   alpha * lambda * (1 - exp(-lambda * x))^(alpha - 1) * exp(-lambda * x)
#' }
#' cdf_ge <- function(x, alpha, lambda) {
#'   (1 - exp(-lambda * x))^alpha
#' }
#' dist_moments(pdf_ge, cdf_ge, support = c(0, Inf), params = list(alpha = 2, lambda = 3))
#'
#' # Exponentiated Weibull Distribution
#' pdf_expweibull <- function(x, a, b, c){
#'   a * b * c * exp(-(b*x)^c) * (b*x)^(c-1) * (1 - exp(-(b*x)^c))^(a-1)
#' }
#' cdf_expweibull <- function(x, a, b, c){
#'   (1 - exp(-(b*x)^c))^a
#' }
#' dist_moments(pdf_expweibull, cdf_expweibull, support = c(0, Inf),
#'              params = list(a = 1.0, b = 1.4, c = 2.3))
#'
#' @export
dist_moments <- function(pdf, cdf, support = c(0, Inf), params = list()) {
  mean_val <- integrate(function(x) x * do.call(pdf, c(list(x = x), params)),
                        lower = support[1], upper = support[2])$value
  var_val <- integrate(function(x) (x - mean_val)^2 * do.call(pdf, c(list(x = x), params)),
                       lower = support[1], upper = support[2])$value
  skew_val <- integrate(function(x) ((x - mean_val)^3) * do.call(pdf, c(list(x = x), params)),
                        lower = support[1], upper = support[2])$value / (var_val^(3/2))
  kurt_val <- integrate(function(x) ((x - mean_val)^4) * do.call(pdf, c(list(x = x), params)),
                        lower = support[1], upper = support[2])$value / (var_val^2)

  xmax <- support[2]
  if (!is.finite(xmax)) {
    xmax <- 1
    while (do.call(cdf, c(list(x = xmax), params)) < 0.999) {
      xmax <- xmax * 2
      if (xmax > 1e6) break
    }
  }
  median_val <- uniroot(function(q) do.call(cdf, c(list(x = q), params)) - 0.5,
                        lower = support[1], upper = xmax)$root

  grid <- seq(support[1], xmax, length.out = 500)
  pdf_vals <- sapply(grid, function(xx) do.call(pdf, c(list(x = xx), params)))
  mode_val <- grid[which.max(pdf_vals)]

  data.frame(
    Mean     = mean_val,
    Variance = var_val,
    Skewness = skew_val,
    Kurtosis = kurt_val,
    Median   = median_val,
    Mode     = mode_val,
    row.names = NULL
  )
}

#' Plot Fitted Distribution with Diagnostic Plots
#'
#' This function produces a 2x2 panel of plots for a fitted distribution:
#' 1. Fitted PDF over histogram of data
#' 2. Fitted CDF vs empirical CDF
#' 3. QQ-plot (Theoretical vs Sample Quantiles)
#' 4. PP-plot (Fitted CDF vs Empirical CDF)
#'
#' @param data Numeric vector of observed data.
#' @param pdf_fun Function to compute the PDF; must take (par, x).
#' @param cdf_fun Function to compute the CDF; must take (par, x).
#' @param par Numeric vector of fitted parameters.
#' @param q_fun Optional quantile function; must take (par, p). Default = NULL.
#' @param xlim Numeric vector of length 2 for x-axis limits (default = range of data).
#' @param ylim_pdf Numeric vector for y-axis limits of the PDF plot.
#' @param ylim_cdf Numeric vector for y-axis limits of the CDF plot.
#' @param lwd Line width for curves (default = 3).
#' @param lty Line type for curves (default = 2).
#' @param col_pdf Color for the PDF curve (default = "yellow").
#' @param col_cdf Color for the CDF curve (default = "red").
#' @param col_qq Color for QQ-plot points (default = "purple").
#' @param col_pp Color for PP-plot points (default = "darkgreen").
#'
#' @importFrom graphics plot lines legend polygon grid points abline axis curve rect par hist text
#' @importFrom grDevices rainbow adjustcolor col2rgb rgb
#' @importFrom stats ecdf
#'
#' @return NULL (plots are generated as a side effect)
#'
#' @export

plot_fitted <- function(data,
                        pdf_fun,
                        cdf_fun,
                        par,
                        q_fun = NULL,
                        xlim = NULL,
                        ylim_pdf = NULL,
                        ylim_cdf = NULL,
                        lwd = 3, lty = 2,
                        col_pdf = "yellow",
                        col_cdf = "red",
                        col_qq = "purple",
                        col_pp = "darkgreen") {
  x <- NULL
  n <- length(data)
  data_sorted <- sort(data)
  if (is.null(xlim)) xlim <- range(data)
  x_seq <- seq(xlim[1], xlim[2], length.out = 300)
  pdf_vals <- pdf_fun(par, x_seq)
  cdf_vals <- cdf_fun(par, x_seq)
  ecdf_vals <- ecdf(data)(x_seq)
  fancy_bg <- function() {
    usr <- par("usr")
    rect(usr[1], usr[3], usr[2], usr[4],
         col = rgb(0.96, 0.96, 0.99, 1), border = NA)
    grid(col = "gray80", lty = 3, lwd = 1)
  }
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par), add = TRUE)
  par(mfrow = c(2, 2), mar = c(4, 4, 3, 2))
  plot(0, 0, type = "n", xlim = xlim,
       ylim = if (!is.null(ylim_pdf)) ylim_pdf else c(0, max(pdf_vals)*1.2),
       main = "Fitted PDF", xlab = "x", ylab = "Density")
  fancy_bg()
  hist(data, freq = FALSE, col = rgb(0.5,0.5,0.9,0.4),
       border = NA, add = TRUE)
  lines(x_seq, pdf_vals, col = col_pdf, lwd = lwd, lty = lty)
  legend("topright", legend = c("Fitted PDF", "Histogram"),
         col = c(col_pdf, rgb(0.5,0.5,0.9,0.4)),
         lwd = c(lwd, 10), lty = c(lty, 1), bty = "n")
  plot(0, 0, type = "n", xlim = xlim,
       ylim = if (!is.null(ylim_cdf)) ylim_cdf else c(0,1),
       main = "Fitted CDF vs Empirical CDF",
       xlab = "x", ylab = "CDF")
  fancy_bg()
  lines(x_seq, cdf_vals, col = col_cdf, lwd = lwd, lty = lty)
  lines(x_seq, ecdf_vals, col = "black", lwd = lwd, lty = 3)
  legend("bottomright",
         legend = c("Fitted CDF", "Empirical CDF"),
         col = c(col_cdf, "black"),
         lwd = lwd, lty = c(lty, 3), bty = "n")
  plot(0, 0, type = "n", xlim = xlim, ylim = xlim,
       main = "QQ-Plot",
       xlab = "Theoretical Quantiles",
       ylab = "Sample Quantiles")
  fancy_bg()
  if (!is.null(q_fun)) {
    p_vals <- (1:n - 0.5)/n
    theoretical_q <- q_fun(par, p_vals)
    points(theoretical_q, data_sorted, pch = 19, col = col_qq)
    abline(0, 1, col = "red", lwd = lwd, lty = lty)
    legend("topleft",
           legend = c("Data Points", "Reference Line"),
           col = c(col_qq, "red"),
           pch = c(19, NA),
           lty = c(NA, lty),
           lwd = c(2, lwd),
           bty = "n")
  } else {
    text(mean(xlim), mean(xlim),
         "No q_fun provided", cex = 1.2, col = "red")
  }
  plot(0, 0, type = "n", xlim = c(0, 1), ylim = c(0, 1),
       main = "PP-Plot",
       xlab = "Fitted CDF",
       ylab = "Empirical CDF")
  fancy_bg()
  emp_cdf <- (1:n)/n
  fitted_cdf <- cdf_fun(par, data_sorted)
  points(fitted_cdf, emp_cdf, pch = 19, col = col_pp)
  abline(0, 1, col = "red", lwd = lwd, lty = lty)
  legend("topleft", legend = "PP Points",
         col = col_pp, pch = 19,
         lwd = lwd, lty = lty, bty = "n")
}



#' Plot Multiple Fitted Distributions
#'
#' Creates 2x2 plots for multiple fitted distributions:
#' Fitted PDFs, Fitted CDFs vs Empirical CDF, QQ-Plots, and PP-Plots.
#'
#' @param data Numeric vector of observed data.
#' @param pdf_list List of PDF functions. Each function should take \code{x} and \code{par}.
#' @param cdf_list List of CDF functions. Each function should take \code{x} and \code{par}.
#' @param qf_list List of quantile functions (inverse CDF). Each function should take \code{p} and \code{par}.
#' @param params_list List of parameter vectors corresponding to each distribution.
#' @param dist_names Optional vector of distribution names.
#' @param col_list Optional vector of colors for each distribution.
#' @param lty_list Optional vector of line types for each distribution.
#' @param lwd_list Optional vector of line widths for each distribution.
#' @param main_pdf Title for PDF plot.
#' @param main_cdf Title for CDF plot.
#' @param main_qq Title for QQ plot.
#' @param main_pp Title for PP plot.
#' @param xlab Label for x-axis.
#' @importFrom graphics plot lines legend polygon grid points abline plot.window plot.new title text rect par
#' @importFrom grDevices rainbow adjustcolor col2rgb rgb
#' @importFrom stats ecdf
#' @return NULL (plots are generated as a side effect)
#' @export
#'
#' @examples
#' # Example Multiple Distributions
#' set.seed(1)
#' data <- rexp(200, 1.1)
#'
#' # Exponential
#' pdf_exp <- function(x, par) par[1] * exp(-par[1] * x)
#' cdf_exp <- function(x, par) 1 - exp(-par[1] * x)
#' qf_exp  <- function(p, par) -log(1 - p) / par[1]
#'
#' # Generalized Exponential
#' pdf_gexp <- function(x, par) {
#'   a <- par[1]; l <- par[2]
#'   a * l * exp(-l*x) * (1-exp(-l*x))^(a-1)
#' }
#' cdf_gexp <- function(x, par) {
#'   a <- par[1]; l <- par[2]
#'   (1-exp(-l*x))^a
#' }
#' qf_gexp <- function(p, par) {
#'   a <- par[1]; l <- par[2]
#'   -log(1 - p^(1/a)) / l
#' }
#'
#' # Weibull
#' pdf_weibull <- function(x, par) {
#'   k <- par[1]; l <- par[2]
#'   (k/l) * (x/l)^(k-1) * exp(-(x/l)^k)
#' }
#' cdf_weibull <- function(x, par) {
#'   k <- par[1]; l <- par[2]
#'   1 - exp(-(x/l)^k)
#' }
#' qf_weibull <- function(p, par) {
#'   k <- par[1]; l <- par[2]
#'   l * (-log(1 - p))^(1/k)
#' }
#'
#' # Normal
#' pdf_norm <- function(x, par) dnorm(x, par[1], par[2])
#' cdf_norm <- function(x, par) pnorm(x, par[1], par[2])
#' qf_norm <- function(p, par) qnorm(p, par[1], par[2])
#'
#' data <- rexp(200, 1)
#' # Call the plot function
#' plot_multi_fitted(
#'   data = data,
#'   pdf_list = list(pdf_exp, pdf_gexp, pdf_weibull, pdf_norm),
#'   cdf_list = list(cdf_exp, cdf_gexp, cdf_weibull, cdf_norm),
#'   qf_list  = list(qf_exp, qf_gexp, qf_weibull, qf_norm),
#'   params_list = list(
#'     c(1.1),
#'     c(2, 1.3),
#'     c(1.5, 2),
#'     c(0, 1)
#'   ),
#'   dist_names = c("Exp", "GExp", "Weibull", "Normal"),
#'   col_list = c("blue", "red", "darkgreen", "purple"),
#'   lty_list = c(1, 2, 3, 4),
#'   lwd_list = c(3, 3, 3, 3)
#' )
plot_multi_fitted <- function(
    data,
    pdf_list,
    cdf_list,
    qf_list,
    params_list,
    dist_names = NULL,
    col_list = NULL,
    lty_list = NULL,
    lwd_list = NULL,
    main_pdf = "Fitted PDFs",
    main_cdf = "Fitted CDFs",
    main_qq  = "QQ Plots",
    main_pp  = "PP Plots",
    xlab = "x"
) {
  x <- NULL
  k <- length(pdf_list)
  if (is.null(dist_names)) dist_names <- paste("Dist", 1:k)
  if (is.null(col_list)) col_list <- 1:k
  if (is.null(lty_list)) lty_list <- rep(1, k)
  if (is.null(lwd_list)) lwd_list <- rep(2, k)
  xmin <- min(data)
  xmax <- max(data)
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par), add = TRUE)
  par(
    mfrow = c(2, 2),
    bg = "white",
    col.axis = "black",
    col.lab  = "black",
    col.main = "black",
    family   = "serif",
    mar = c(4.8, 4.5, 4, 2)
  )
  fancy_bg <- function() {
    usr <- par("usr")
    rect(usr[1], usr[3], usr[2], usr[4],
         col = rgb(0.96, 0.96, 0.99, 1), border = NA)
    grid(col = "gray85", lty = 3)
  }
  plot.new()
  plot.window(xlim = c(xmin, xmax), ylim = c(0, max(density(data)$y)*1.3))
  fancy_bg()
  hist(data, freq = FALSE, col = rgb(0.5, 0.5, 0.9, 0.4),
       border = NA, add = TRUE)
  for (i in 1:k) {
    curve(pdf_list[[i]](x, params_list[[i]]),
          from = xmin, to = xmax,
          add = TRUE, col = col_list[i],
          lwd = lwd_list[i], lty = lty_list[i])
  }
  title(main_pdf, font.main = 2, cex.main = 1.3)
  title(xlab = xlab)
  legend("topright", legend = dist_names,
         col = col_list, lwd = lwd_list,
         lty = lty_list, cex = 1, bty = "n")
  plot.new()
  plot.window(xlim = c(xmin, xmax), ylim = c(0, 1))
  fancy_bg()
  plot(ecdf(data), add = TRUE, col = "black", lwd = 2)
  for (i in 1:k) {
    curve(cdf_list[[i]](x, params_list[[i]]),
          from = xmin, to = xmax,
          add = TRUE, col = col_list[i],
          lwd = lwd_list[i], lty = lty_list[i])
  }
  title(main_cdf, font.main = 2, cex.main = 1.3)
  title(xlab = xlab, ylab = "CDF", col.lab = "black")
  legend("bottomright", legend = dist_names,
         col = col_list, lwd = lwd_list,
         lty = lty_list, cex = 1, bty = "n")
  n <- length(data)
  probs <- ppoints(n)
  sorted_data <- sort(data)
  plot(NULL, xlim = range(data), ylim = range(data),
       xlab = "Theoretical Quantiles",
       ylab = "Sample Quantiles",
       main = main_qq)
  fancy_bg()
  for (i in 1:k) {
    q_theoretical <- qf_list[[i]](probs, params_list[[i]])
    lines(q_theoretical, sorted_data,
          col = col_list[i],
          lwd = lwd_list[i], lty = lty_list[i])
  }
  abline(0, 1, col = "black", lwd = 2)
  legend("topleft", legend = dist_names,
         col = col_list, lwd = lwd_list,
         lty = lty_list, bty = "n")
  plot(NULL, xlim = c(0,1), ylim = c(0,1),
       xlab = "Theoretical CDF", ylab = "Empirical CDF",
       main = main_pp)
  fancy_bg()
  empirical_probs <- (1:n)/n
  for (i in 1:k) {
    theo <- cdf_list[[i]](sorted_data, params_list[[i]])
    lines(theo, empirical_probs,
          col = col_list[i],
          lwd = lwd_list[i],
          lty = lty_list[i])
  }
  abline(0, 1, col = "black", lwd = 2)
  legend("bottomright", legend = dist_names,
         col = col_list, lwd = lwd_list,
         lty = lty_list, bty = "n")
}


#' Plot Data Overview
#'
#' Generates a 2x2 plot layout showing:
#' Histogram, Boxplot, Kernel Density, and TTT Plot.
#'
#' @param x Numeric vector of data.
#' @param col Color for plots (default "steelblue").
#' @param border Border color for histograms/boxplots (default "black").
#' @param transparency Transparency for filled areas (0-1, default 0.5).
#' @param lwd Line width for plots (default 2).
#' @param breaks Histogram breaks (default "Sturges").
#' @importFrom graphics plot lines legend polygon grid points abline boxplot rect par
#' @importFrom grDevices rainbow adjustcolor col2rgb rgb
#' @importFrom stats density ppoints
#' @return NULL (plots are drawn)
#' @export
#'
#' @examples
#' set.seed(123)
#' mydata <- rexp(100, 1)
#' plot_data(mydata, col = "darkblue", transparency = 0.35, lwd = 2)
plot_data <- function(
    x,
    col = "steelblue",
    border = "black",
    transparency = 0.5,
    lwd = 2,
    breaks = "Sturges"
){
  make_transparent <- function(col, alpha = transparency){
    rgb_val <- col2rgb(col)
    rgb(rgb_val[1]/255, rgb_val[2]/255, rgb_val[3]/255, alpha)
  }
  colT <- make_transparent(col)
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par), add = TRUE)
  par(mfrow = c(2,2), mar = c(4,4,3,2))
  hist_obj <- hist(x, probability = TRUE, breaks = breaks,
                   col = colT, border = NA,
                   main = "Histogram", xlab = "Value", ylab = "Density")
  usr <- par("usr")
  rect(usr[1], usr[3], usr[2], usr[4], col = rgb(0.96,0.96,0.99,1), border = NA)
  grid(col="gray85", lty=3)
  hist(x, probability = TRUE, breaks = breaks, col = colT, border = NA, add=TRUE)
  legend("topright", legend = "Histogram", fill = colT, border = NA)

  plot(0, 0, type="n", xlim=c(min(x), max(x)), ylim=c(0.5, 1.5),
       axes=FALSE, xlab="", ylab="", main="Boxplot")
  rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4],
       col = rgb(0.96,0.96,0.99,1), border = NA)
  grid(col="gray85", lty=3)
  boxplot(x, horizontal=TRUE, at=1, col=colT, border=border,
          lwd=lwd, add=TRUE, axes=FALSE)
  axis(1)
  axis(2, at=1, labels="")
  legend("topright", legend = "Boxplot", fill = colT, border = border)

  dens <- density(x)
  plot(dens, type="n", main="Kernel Density", xlab="Value", ylab="Density")
  rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4],
       col = rgb(0.96,0.96,0.99,1), border = NA)
  grid(col="gray85", lty=3)
  polygon(c(dens$x, rev(dens$x)), c(dens$y, rep(0,length(dens$y))),
          col = colT, border = border, lwd = lwd)
  lines(dens, col = col, lwd = lwd)
  legend("topright", legend = "Kernel Density", col = col, lwd = lwd)
  x_sorted <- sort(x)
  n <- length(x_sorted)
  S <- cumsum(x_sorted)/sum(x_sorted)
  T <- (1:n)/n
  plot(T, S, type="n", lwd=lwd, col=col, main="TTT Plot",
       xlab="Fraction of Population", ylab="TTT(t)")
  rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4],
       col = rgb(0.96,0.96,0.99,1), border = NA)
  grid(col="gray85", lty=3)
  lines(T, S, col=col, lwd=lwd)
  abline(0,1,col="red", lwd=lwd, lty=2)
  legend("topleft", legend=c("TTT Curve","Reference Line"),
         col=c(col,"red"), lty=c(1,2), lwd=c(lwd,lwd))
}
