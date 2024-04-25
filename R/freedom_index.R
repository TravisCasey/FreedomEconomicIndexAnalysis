#' Freedom Economic Index dataset
#'
#' Economic freedom data on 176 countries. Includes data on the country, as well
#' as 12 markers of economic freedom and an overall economic freedom score. All
#' scores are out of 100.
#'
#' @format ## `freedom_index`
#' A data frame with 176 rows and 15 columns:
#' \describe{
#'   \item{Country}{Country name}
#'   \item{Reguin}{One of `Asia-Pacific`, `Europe`, `America`, `Sub-Saharan Africa`, or `Middle East/North Africa`}
#'   \item{Overall.Score}{The overall economic freedom score for the country. Out of 100.}
#'   \item{Economic Scores}{Remaining 12 columns are scores out of 100 on aspects of economic freedom.}
#'   ...
#' }
#' @source <https://www.kaggle.com/datasets/mlippo/freedom-economic-index>
"freedom_index"
