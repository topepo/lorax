#' Combine multiple R expressions into a single composite expression
#'
#' This function takes a list of R expressions and combines them using a
#' logical operator to create a single composite expression. It is useful for
#' building complete rule paths by combining individual split conditions from
#' tree-based models.
#'
#' @param exprs A list of R expressions to combine. Each element must be a
#'   language object (expression or symbol). The list can be empty (returns
#'   `TRUE`), contain a single expression (returns unchanged), or multiple
#'   expressions (combines with operator).
#' @param operator A character string specifying the logical operator to use:
#'   * `"&"` (default): combines expressions with AND logic
#'   * `"|"`: combines expressions with OR logic
#'
#' @return An R expression object that combines all input expressions. Returns
#'   `TRUE` for empty list, the single expression for length-1 list, or a
#'   nested expression for multiple elements.
#'
#' @examples
#' # Basic AND combination
#' expr1 <- rlang::expr(x > 5)
#' expr2 <- rlang::expr(y < 10)
#' combine_rule_elements(list(expr1, expr2))
#'
#' # OR operator
#' combine_rule_elements(list(expr1, expr2), operator = "|")
#'
#' # Integration with rect_split_to_expr()
#' split1 <- list(column = "age", value = 30, operator = ">=")
#' split2 <- list(column = "income", value = 50000, operator = ">")
#' exprs <- list(
#'   rect_split_to_expr(split1),
#'   rect_split_to_expr(split2)
#' )
#' rule <- combine_rule_elements(exprs)
#'
#' # Evaluate with data
#' test_data <- data.frame(age = 35, income = 60000)
#' eval(rule, test_data)
#'
#' # Edge cases
#' combine_rule_elements(list()) # returns TRUE
#' combine_rule_elements(list(rlang::expr(x > 0))) # returns x > 0
#'
#' @export
combine_rule_elements <- function(exprs, operator = "&") {
  validate_exprs(exprs, operator, call = rlang::current_env())

  if (length(exprs) == 0) {
    return(rlang::expr(TRUE))
  }

  if (length(exprs) == 1) {
    return(exprs[[1]])
  }

  purrr::reduce(exprs, \(x, y) rlang::call2(operator, x, y))
}

# Internal helper to validate exprs and operator
validate_exprs <- function(exprs, operator, call = rlang::caller_env()) {
  # Check that input is a list
  if (!is.list(exprs)) {
    cli::cli_abort(
      "{.arg exprs} must be a list, not {.obj_type_friendly {exprs}}.",
      call = call
    )
  }

  # Check that operator is valid
  valid_operators <- c("&", "|")
  if (!operator %in% valid_operators) {
    cli::cli_abort(
      "{.arg operator} must be {.or {valid_operators}}, not {.val {operator}}.",
      call = call
    )
  }

  # Check each element is an expression
  for (i in seq_along(exprs)) {
    elem <- exprs[[i]]

    # Check for NULL
    if (is.null(elem)) {
      cli::cli_abort(
        "{.arg exprs} element {i} is {.val NULL}.",
        call = call
      )
    }

    # Check that element is a language object (expression or symbol)
    if (!is.language(elem) && !is.symbol(elem)) {
      cli::cli_abort(
        "{.arg exprs} element {i} must be an expression, not {.obj_type_friendly {elem}}.",
        call = call
      )
    }
  }

  invisible(NULL)
}
