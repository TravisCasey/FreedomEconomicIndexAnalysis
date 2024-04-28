#' Economic Freedom Index Shiny Application
#'
#' @export
#'
#' @examples
#' \dontrun{runEFIApp()}
runEFIApp <- function() {
  shiny::runApp(system.file("shinyapp", package = "MATH5793Project3case0005"),
                launch.browser = TRUE)
}
