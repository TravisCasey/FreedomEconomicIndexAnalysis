plot_scatter <- function(scores, categories = NULL) {
  # Helper function for factor.analysis
  # Also used in shiny application

  Factor1 <- NULL
  Factor2 <- NULL
  category <- NULL

  # Determine whether to color points by category
  if (is.null(categories)) {
    plot.data <- data.frame(Factor1 = scores[,1], Factor2 = scores[,2])
    scatter <- ggplot2::ggplot(plot.data, ggplot2::aes(x = Factor1, y = Factor2))

  } else {
    plot.data <- data.frame(Factor1 = scores[,1], Factor2 = scores[,2],
                            category = categories[,1])
    scatter <- ggplot2::ggplot(plot.data, ggplot2::aes(x = Factor1, y = Factor2,
                                                       color = category))
  }

  scatter <- scatter + ggplot2::geom_point() +
    ggplot2::labs(title = "Data Projected on Factors")
}


#' Factor Analysis
#'
#' A wrapper for the `factanal` function that provides input checking, and plots
#' of the results.
#'
#' @param X A dataframe of quantitative data
#' @param factors The number of factors to use in the factor analysis. Must be
#' greater than 1 but less than the number of variables in the dataset.
#' @param categories An optional dataframe with one column representing a
#' categorial variable on the observations in X. Used for coloring the scatter
#' plot. Can be set to NULL to disable this.
#' @param rotation Rotation specifier. See documentation for `factanal`.
#'
#' @return The output of the constructed `factanal` call.
#' @export
#'
#' @examples
#' fa <- factor.analysis(freedom_index[4:15], factors = 3,
#'                       categories = freedom_index[2], rotation = "promax")
factor.analysis <- function(X, factors, categories = NULL, rotation = "none") {

  ##################
  # Input Handling #
  ##################

  if (is.null(X)) {
    stop("X must be specified")
  }

  X <- as.data.frame(X)
  if (!all(sapply(X, is.numeric))) {
    stop("X must be numeric")
  }
  if (nrow(X) < ncol(X)) {
    stop("X must have at least as many observations as rows.")
  }

  if (factors < 1) {
    stop("Must specify at least one factor.")
  }
  if (factors > ncol(X)) {
    stop("There cannot be more factors than variables.")
  }

  if (!is.null(categories)) {
    if (nrow(categories) != nrow(X)) {
      stop("categories must contain a category for each observation")
    }
  }

  ###################
  # Factor Analysis #
  ###################

  fact <- stats::factanal(X, factors, rotation = rotation, scores = "Bartlett")

  ################
  # Scatter Plot #
  ################

  if (factors > 1) {
    plot(plot_scatter(fact$scores, categories))
  }

  ##########
  # Return #
  ##########

  invisible(fact)

}
