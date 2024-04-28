library(shiny)

# Helper plotting functions

plot_scree <- function(scree.data, elbow, exceeds.count, scale) {
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

plot_scatter <- function(scores, categories = NULL) {
  Factor1 <- NULL
  Factor2 <- NULL
  category <- NULL
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

# Define server logic required to draw plots
function(input, output, session) {

  # Inputs are:
  #   scale (bool) whether to use standardized variables in scree/sum
  #   categorize (bool) color scatter plot according to region
  #   rotation (bool) whether to rotate factors (uses promax)

  output$screePlot <- renderPlot({
    n <- 12
    factor_info <- factor.count(freedom_index[4:15], scale = input$scale)
    scree.data <- data.frame(components = seq(from = 1, to = n, by = 1),
                             variance = factor_info$var.values)
    if (input$scale) {
      scree.plot <- plot_scree(scree.data, factor_info$elbow, factor_info$exceeds.count, TRUE)
    } else {
      scree.plot <- plot_scree(scree.data, factor_info$elbow, NULL, FALSE)
    }

    scree.plot
  })

  output$sumPlot <- renderPlot({
    n <- 12
    factor_info <- factor.count(freedom_index[4:15], scale = input$scale)
    sum.data <- data.frame(components = seq(from = 1, to = n, by = 1),
                           cum_sum = factor_info$var.cumsum)
    plot_sum(sum.data)
  })

  output$scatterPlot <- renderPlot({

    if (input$categorize) {
      categories <- freedom_index[2]
    } else {
      categories <- NULL
    }

    if (input$rotation) {
      rotation <- "promax"
    } else {
      rotation <- "none"
    }

    fa <- factor.analysis(freedom_index[4:15], factors = 2,
                          categories = categories, rotation = rotation)
    plot_scatter(fa$scores, categories)
  })

}
