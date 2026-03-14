#' Convert a rectangular split to an R expression
#'
#' This function converts a split condition from a tree-based model into an
#' valid R expression. It is primarily used as a building block for
#' [extract_rules()] to construct paths to terminal nodes.
#'
#' @param split A named list with three required elements:
#'   * `column`: character string - variable name for the split
#'   * `value`: numeric, character, or character vector - the split
#'     threshold/value(s)
#'   * `operator`: character string - one of: `<`, `<=`, `>`, `>=`, `==`,
#'     `%in%`
#'
#' @return An R expression object that can be evaluated.
#'
#' @examples
#' # Numeric comparison
#' rect_split_to_expr(list(column = "age", value = 25, operator = "<"))
#'
#' # Single character value uses ==
#' rect_split_to_expr(list(column = "color", value = "red", operator = "=="))
#'
#' # Multiple character values use %in%
#' rect_split_to_expr(
#'   list(column = "color", value = c("red", "blue"), operator = "%in%")
#' )
#'
#' # Evaluate the expression
#' expr <- rect_split_to_expr(list(column = "age", value = 30, operator = ">="))
#' test_data <- data.frame(age = c(20, 30, 40))
#' test_data[eval(expr, test_data), ]
#'
#' @export
rect_split_to_expr <- function(split) {
  validate_split(split)

  column_sym <- rlang::sym(split$column)
  operator <- split$operator
  value <- split$value

  rlang::call2(operator, column_sym, value)
}

# Internal helper to validate split structure
validate_split <- function(split) {
  # Check that input is a list
  if (!is.list(split)) {
    cli::cli_abort(
      "{.arg split} must be a list, not {.obj_type_friendly {split}}."
    )
  }

  # Check required names
  required_names <- c("column", "value", "operator")
  if (!all(required_names %in% names(split))) {
    missing <- setdiff(required_names, names(split))
    cli::cli_abort(
      "{.arg split} must have elements {.field {required_names}}, missing {.field {missing}}."
    )
  }

  # Check for extra names
  extra_names <- setdiff(names(split), required_names)
  if (length(extra_names) > 0) {
    cli::cli_abort(
      "{.arg split} must only have elements {.field {required_names}}, found extra {.field {extra_names}}."
    )
  }

  # Validate column
  if (
    !is.character(split$column) ||
      length(split$column) != 1 ||
      split$column == ""
  ) {
    cli::cli_abort("{.field column} must be a non-empty character string.")
  }

  # Validate operator
  valid_operators <- c("<", "<=", ">", ">=", "==", "%in%")
  if (!split$operator %in% valid_operators) {
    cli::cli_abort(
      "{.field operator} must be one of {.or {valid_operators}}, not {.val {split$operator}}."
    )
  }

  # Validate value based on operator
  if (split$operator == "%in%") {
    if (!is.character(split$value)) {
      cli::cli_abort(
        "{.field value} must be a character vector when {.field operator} is {.val %in%}."
      )
    }
    if (length(split$value) == 1) {
      cli::cli_abort(
        "Single character values should use {.val ==} operator, not {.val %in%}."
      )
    }
  } else {
    # For other operators, value should be single numeric or character
    if (length(split$value) != 1) {
      cli::cli_abort(
        "{.field value} must be length 1 for operator {.val {split$operator}}."
      )
    }
    if (!is.numeric(split$value) && !is.character(split$value)) {
      cli::cli_abort(
        "{.field value} must be numeric or character for operator {.val {split$operator}}."
      )
    }
  }

  invisible(NULL)
}
