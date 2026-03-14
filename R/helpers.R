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

# ------------------------------------------------------------------------------

#' Convert a rule expression to a readable text format
#'
#' This function formats R expressions representing rules from tree-based models
#' as character strings. It provides options for formatting numeric values,
#' displaying rules as bulleted lists, and controlling output width.
#'
#' @param expr An R expression to format. Typically created by
#'   [rect_split_to_expr()] or [combine_rule_elements()].
#' @param bullets Logical indicating whether to break apart rule elements and
#'   display as a bulleted list. If `TRUE`, splits on `&` operators and creates
#'   a bulleted list with each condition on a new line. If `FALSE` (default),
#'   returns as a single line.
#' @param digits Integer number of significant digits to use when formatting
#'   numeric values in the rule. Default is 4.
#' @param max_width Maximum width for the output when `bullets = FALSE`. If the
#'   formatted rule exceeds this width, it will be truncated with `...`
#'   appended. The `...` is included in the width count. Default is `Inf` (no
#'   truncation).
#'
#' @return A character string containing the formatted rule. When
#'   `bullets = TRUE`, conditions are separated by newlines with bullet markers.
#'
#' @examples
#' # Simple numeric rule
#' rule1 <- rlang::expr(age >= 30)
#' rule_text(rule1)
#'
#' # Multiple conditions
#' rule2 <- rlang::expr(age >= 30 & income > 50000)
#' rule_text(rule2)
#'
#' # Bulleted format
#' cat(rule_text(rule2, bullets = TRUE), "\n")
#'
#' # Control numeric precision
#' rule3 <- rlang::expr(x > 1.23456789)
#' rule_text(rule3, digits = 2)
#' rule_text(rule3, digits = 6)
#'
#' # Truncate long rules
#' rule4 <- rlang::expr(very_long_variable_name > 100 & another_long_name < 50)
#' rule_text(rule4, max_width = 30)
#'
#' # Integration with other helpers
#' split1 <- list(column = "age", value = 30.5, operator = ">=")
#' split2 <- list(column = "income", value = 50000, operator = ">")
#' split3 <- list(column = "city", value = c("NYC", "LA"), operator = "%in%")
#' rule <- combine_rule_elements(list(
#'   rect_split_to_expr(split1),
#'   rect_split_to_expr(split2),
#'   rect_split_to_expr(split3)
#' ))
#' cat(rule_text(rule, bullets = TRUE), "\n")
#'
#' @export
rule_text <- function(expr, bullets = FALSE, digits = 4, max_width = Inf) {
  validate_rule_text_args(
    expr,
    bullets,
    digits,
    max_width,
    call = rlang::current_env()
  )

  # Format numeric values in the expression
  formatted_expr <- format_numeric_in_expr(expr, digits)

  if (bullets) {
    # Extract individual conditions and format as bullets
    conditions <- extract_conditions(formatted_expr)
    paste0("* ", conditions, collapse = "\n")
  } else {
    # Return as single line with optional truncation
    rule_str <- deparse1(formatted_expr)
    if (nchar(rule_str) > max_width) {
      rule_str <- paste0(substr(rule_str, 1, max_width - 3), "...")
    }
    rule_str
  }
}

# Internal helper to validate rule_text arguments
validate_rule_text_args <- function(
  expr,
  bullets,
  digits,
  max_width,
  call = rlang::caller_env()
) {
  if (!is.language(expr) && !is.symbol(expr)) {
    cli::cli_abort(
      "{.arg expr} must be an expression, not {.obj_type_friendly {expr}}.",
      call = call
    )
  }

  if (!is.logical(bullets) || length(bullets) != 1 || is.na(bullets)) {
    cli::cli_abort(
      "{.arg bullets} must be a single logical value, not {.obj_type_friendly {bullets}}.",
      call = call
    )
  }

  if (
    !is.numeric(digits) ||
      length(digits) != 1 ||
      is.na(digits) ||
      digits < 1 ||
      digits != as.integer(digits)
  ) {
    cli::cli_abort(
      "{.arg digits} must be a single positive integer, not {.obj_type_friendly {digits}}.",
      call = call
    )
  }

  if (
    !is.numeric(max_width) ||
      length(max_width) != 1 ||
      is.na(max_width) ||
      max_width < 4
  ) {
    cli::cli_abort(
      "{.arg max_width} must be a single numeric value >= 4, not {.obj_type_friendly {max_width}}.",
      call = call
    )
  }

  invisible(NULL)
}

# Internal helper to format numeric values in an expression
format_numeric_in_expr <- function(expr, digits) {
  if (is.numeric(expr)) {
    # Format numeric values
    formatted <- format(expr, digits = digits, trim = TRUE)
    # Return as numeric if possible to preserve structure
    return(as.numeric(formatted))
  } else if (is.call(expr)) {
    # Recursively format call arguments
    expr[] <- lapply(expr, format_numeric_in_expr, digits = digits)
    return(expr)
  } else {
    # Return other types unchanged
    return(expr)
  }
}

# Internal helper to extract individual conditions from an expression
extract_conditions <- function(expr) {
  if (is.call(expr) && as.character(expr[[1]]) == "&") {
    # Split on & operator
    left <- extract_conditions(expr[[2]])
    right <- extract_conditions(expr[[3]])
    return(c(left, right))
  } else {
    # Base case: return as single condition
    return(deparse1(expr))
  }
}
