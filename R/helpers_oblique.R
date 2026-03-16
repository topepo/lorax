#' Convert an oblique split to an R expression
#'
#' This function converts an oblique split condition (linear combination) from a
#' tree-based model into a valid R expression. Oblique splits use a weighted sum
#' of multiple variables compared to a threshold.
#'
#' @param split A named list with four required elements:
#'   * `columns`: character vector - variable names for the linear combination.
#'   * `values`: numeric vector - coefficients for each variable (same length as.
#'     `columns`)
#'   * `operator`: character string - one of: `<`, `<=`, `>`, `>=`, `==`.
#'   * `threshold`: numeric scalar - the threshold value for comparison.
#'
#' @return An R expression object that can be evaluated. The expression
#'   represents: `values[1]*columns[1] + .. + values[n]*columns[n] {operator}
#'   threshold`.
#'
#' @examples
#' # Simple oblique split with two variables
#' obliq_split_to_expr(list(
#'   columns = c("x", "y"),
#'   values = c(2, 3),
#'   operator = ">",
#'   threshold = 10
#' ))
#'
#' # Oblique split with negative coefficients
#' obliq_split_to_expr(list(
#'   columns = c("age", "income"),
#'   values = c(1.5, -0.001),
#'   operator = "<=",
#'   threshold = 50
#' ))
#'
#' # Three-variable oblique split
#' obliq_split_to_expr(list(
#'   columns = c("x", "y", "z"),
#'   values = c(1, 2, -1),
#'   operator = ">=",
#'   threshold = 0
#' ))
#'
#' # Evaluate the expression
#' expr <- obliq_split_to_expr(list(
#'   columns = c("x", "y"),
#'   values = c(1, 1),
#'   operator = ">",
#'   threshold = 5
#' ))
#' test_data <- data.frame(x = c(2, 3, 4), y = c(2, 3, 4))
#' test_data[eval(expr, test_data), ]
#'
#' @export
obliq_split_to_expr <- function(split) {
  validate_obliq_split(split, call = rlang::current_env())

  # Build linear combination expression
  lhs <- build_linear_combination(split$columns, split$values)

  # Create comparison expression
  rlang::call2(split$operator, lhs, split$threshold)
}

# Internal helper to build linear combination expression
build_linear_combination <- function(columns, values) {
  # Start with first term
  terms <- list(rlang::call2("*", values[1], rlang::sym(columns[1])))

  # Add remaining terms
  if (length(columns) > 1) {
    for (i in 2:length(columns)) {
      term <- rlang::call2("*", values[i], rlang::sym(columns[i]))

      if (values[i] < 0) {
        # Use subtraction for negative coefficients
        terms[[i]] <- rlang::call2(
          "-",
          terms[[i - 1]],
          rlang::call2("*", abs(values[i]), rlang::sym(columns[i]))
        )
      } else {
        # Use addition for positive coefficients
        terms[[i]] <- rlang::call2("+", terms[[i - 1]], term)
      }
    }
  }

  # Return the final expression
  terms[[length(terms)]]
}

# Internal helper to validate oblique split structure
validate_obliq_split <- function(split, call = rlang::caller_env()) {
  # Check that input is a list
  if (!is.list(split)) {
    cli::cli_abort(
      "{.arg split} must be a list, not {.obj_type_friendly {split}}.",
      call = call
    )
  }

  # Check required names
  required_names <- c("columns", "values", "operator", "threshold")
  if (!all(required_names %in% names(split))) {
    missing <- setdiff(required_names, names(split))
    cli::cli_abort(
      "{.arg split} must have elements {.field {required_names}}, missing {.field {missing}}.",
      call = call
    )
  }

  # Check for extra names
  extra_names <- setdiff(names(split), required_names)
  if (length(extra_names) > 0) {
    cli::cli_abort(
      "{.arg split} must only have elements {.field {required_names}}, found extra {.field {extra_names}}.",
      call = call
    )
  }

  # Validate columns
  if (!is.character(split$columns) || length(split$columns) == 0) {
    cli::cli_abort(
      "{.field columns} must be a non-empty character vector.",
      call = call
    )
  }

  if (any(split$columns == "")) {
    cli::cli_abort(
      "{.field columns} must not contain empty strings.",
      call = call
    )
  }

  # Validate values
  if (!is.numeric(split$values)) {
    cli::cli_abort(
      "{.field values} must be numeric, not {.obj_type_friendly {split$values}}.",
      call = call
    )
  }

  if (length(split$values) != length(split$columns)) {
    cli::cli_abort(
      "{.field values} must have the same length as {.field columns} ({length(split$columns)}), not {length(split$values)}.",
      call = call
    )
  }

  # Validate operator
  valid_operators <- c("<", "<=", ">", ">=", "==")
  if (!split$operator %in% valid_operators) {
    cli::cli_abort(
      "{.field operator} must be one of {.or {valid_operators}}, not {.val {split$operator}}.",
      call = call
    )
  }

  # Validate threshold
  if (!is.numeric(split$threshold) || length(split$threshold) != 1) {
    cli::cli_abort(
      "{.field threshold} must be a single numeric value, not {.obj_type_friendly {split$threshold}}.",
      call = call
    )
  }

  invisible(NULL)
}
