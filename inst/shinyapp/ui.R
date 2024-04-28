library(shiny)

# Define UI for application that draws plots
fluidPage(

  # Application title
  titlePanel("Freedom Economic Index Factor Analysis"),

  # Sidebar with checkboxes for options
  sidebarLayout(
    sidebarPanel(
      checkboxInput("scale",
                    "Standardized Variables",
                    value = FALSE),
      checkboxInput("rotation",
                    "Rotate Factors",
                    value = FALSE),
      checkboxInput("categorize",
                    "Color Regions",
                    value = TRUE)

    ),

    # Show plots
    mainPanel(
      fluidRow(
        column(width = 6,
               plotOutput("screePlot")),
        column(width = 6,
               plotOutput("sumPlot"))
      ),
      fluidRow(
        plotOutput("scatterPlot")
      )
    )
  )
)
