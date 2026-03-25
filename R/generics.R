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
#' @return A data frame with column `rules` (an R expression) and `id` (an
#' identifier).
#'
#' @export
extract_rules <- function(x, ...) {
  UseMethod("extract_rules")
}

# ------------------------------------------------------------------------------

#' Extract the active features from a tree
#'
#' If a tree does not use a predictor in the training set in any of its splits
#' it is functionally independent of the prediction function. This generic
#' returns a data frame containing character vector of predictor names that
#' were used in at least one split.
#'
#' @param x A object
#' @param ... Other arguments passed to methods
#'
#' @return A tibble with list column `active_predictors` containing a character
#' vector of predictors.
#'
#' @export
active_predictors <- function(x, ...) {
 UseMethod("active_predictors")
}
