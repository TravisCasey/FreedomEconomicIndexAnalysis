plot_scree <- function(scree.data, elbow, exceeds.count, scale) {
  # Helper function for factor.count
  # Also used in shiny application

  # construct plot
  n <- length(scree.data$components)
  components <- NULL
  variance <- NULL
  scree.plot <- ggplot2::ggplot(scree.data, ggplot2::aes(components, variance)) +
    ggplot2::geom_point(size = 1.5, color = "blue") +
    ggplot2::geom_line() +
    ggplot2::labs(title = "Scree Plot") +
    ggplot2::theme(aspect.ratio = 1.0) +
    ggplot2::scale_x_continuous(breaks = seq(from = 1, to = n, by = ceiling(n/8)))

  # Mark elbow on scree plot
  scree.plot <- scree.plot +
    ggplot2::geom_vline(xintercept = elbow$elbow, linetype = 'dashed',
                        color = 'red', size = 1.5,)

  # Mark cutoff for those exceeding unit variance, if scale is TRUE
  if (scale) {
    scree.plot <- scree.plot +
      ggplot2::geom_vline(xintercept = exceeds.count$unit,
                          linetype = 'dashed', color = 'green', size = 1.5)
  }

  scree.plot
}


plot_sum <- function(sum.data) {
  # Helper function for factor.count
  # Also used in shiny application
  n <- length(sum.data$components)

  cum_sum <- NULL
  components <- NULL
  sum.plot <- ggplot2::ggplot(sum.data, ggplot2::aes(components, cum_sum)) +
    ggplot2::geom_point(size=1.5, color="blue") +
    ggplot2::geom_line() +
    ggplot2::labs(title = "Cumulative Sum of Variance", y = "Cumulative Variance") +
    ggplot2::theme(aspect.ratio = 1.0) +
    ggplot2::scale_x_continuous(breaks = seq(from = 1, to = n, by = ceiling(n/8)))

  sum.plot
}


#' Factor Count Analysis
#'
#' Plots a scree plot and cumulative sum of variance plot, and calculates
#' quantities helpful to choosing the number of components and/or factor to
#' use in a PCA or factor analysis.
#'
#' @param X A dataframe of quantitative data. Optional if S is
#' specified.
#' @param S An optional variance-covariance matrix that supersedes X if not set
#' to NULL.
#' @param scale Whether to use standardized variables in the analysis.
#'
#' @return A list containing the variances for each component and a cumulative
#' sum of these variances. Also contains the automatically found elbow in the
#' scree plot and the angle at each point. Finally, reports the number of
#' components with variance exceeding unit variance, and those exceeding 0.9
#' and 1.1 variance, respectively.
#' @export
#'
#' @examples
#' factor_info <- factor.count(X = freedom_index[4:15])
factor.count <- function(X = NULL, S = NULL, scale = FALSE) {

  ##################
  # Input Handling #
  ##################

  # User can specify either a dataframe X or a variance-covariance matrix
  # S but not neither.
  if (is.null(X) & is.null(S)) {
    stop("Must specify either data X or covariance matrix S.")
  }

  # Only use data X if it is valid and S is NULL.
  X <- as.data.frame(X)
  if (is.null(S) & all(sapply(X, is.numeric))) {
    if (nrow(X) < ncol(X)) {
      stop("X must contain at least any many observations as variables.")
    }
    S <- stats::cov(X)
  } else if (is.numeric(S)) {
    S <- as.matrix(S)
    if (nrow(S) != ncol(S) | any(t(S) != S)) {
      stop("S must be a square, symmetric matrix.")
    }
  } else {
    stop("S and X must be numeric.")
  }

  # Default is FALSE
  if (!is.logical(scale)) {
    scale = FALSE
  }

  # Scale uses standardized variables -> use correlation matrix instead
  if (scale) {
    S <- stats::cov2cor(S)
  }

  # Only use variance-covariance/correlation matrix S from here on

  ##########################
  # Spectral Decomposition #
  ##########################

  decomp <- eigen(S, only.values = TRUE)
  n <- length(decomp$values)

  # If scaled, suggest rule of thumb to only include principal components with
  # eigenvalues greater than "about" 1.00.
  # Not relevant for unscaled data.
  if (scale) {
    exceeds.count <- list(lower = sum(decomp$values > 0.9),
                          unit = sum(decomp$values > 1.00),
                          upper = sum(decomp$values > 1.1))
  }

  # Compute the cumulative sum of eigenvalues (variances due to each compoennt).
  var.cumsum <- cumsum(decomp$values)

  ##################
  # Elbow Detector #
  ##################

  # Detects elbow in scree plot by first rescaling the data to fit how the plot
  # looks. Then computes the angle at each point (except the endpoints), and
  # returns the component amount with the least angle.
  rescale <- n / decomp$values[1]
  var.diffs <- diff(decomp$values) * rescale

  theta <- vector(mode = "numeric", length = (length(var.diffs) - 1))
  for (i in 1:length(theta)) {
    theta[i] <- ((3 * pi / 2) - atan(-1 / var.diffs[i+1])
                              - atan(-1 * var.diffs[i]))
  }

  elbow <- list(elbow = (which.min(theta) + 1),
                theta = theta)

  ##############
  # Scree Plot #
  ##############

  scree.data <- data.frame(components = seq(from = 1, to = n, by = 1),
                           variance = decomp$values)

  scree.plot <- plot_scree(scree.data, elbow, exceeds.count, scale)
  plot(scree.plot)

  #######################
  # Cumulative Sum Plot #
  #######################

  sum.data <- data.frame(components = seq(from = 1, to = n, by = 1),
                         cum_sum = var.cumsum)

  sum.plot <- plot_sum(sum.data)
  plot(sum.plot)

  ##########
  # Return #
  ##########

  if (scale) {
    return_list <- list(var.values = decomp$values,
                        var.cumsum = var.cumsum,
                        elbow = elbow,
                        exceeds.count = exceeds.count)
  } else {
    return_list <- list(var.values = decomp$values,
                        var.cumsum = var.cumsum,
                        elbow = elbow)
  }

  invisible(return_list)
}
