#' Extract an expression that defines a path to a terminal node
#'
#' A rule is a logical expression of predictor variables that reflects which
#' data are contained in or sent to a terminal node in a tree-based model. Rules
#' can take any form but, for most trees, they are simple statements such as
#' `x < 1.2`, `y == "red"`, or `z %in% c("blue", "green")`.
#'
#' @param x A object
#' @param ... Other arguments passed to methods
#'
#' @return A data frame with column `equation` (an R expression) and `id` (an
#' identifier).
#'
#' @export
extract_rules <- function(x, ...) {
  UseMethod("extract_rules")
}
