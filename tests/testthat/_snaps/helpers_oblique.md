# obliq_split_to_expr() validates input structure

    Code
      obliq_split_to_expr("not a list")
    Condition
      Error in `obliq_split_to_expr()`:
      ! `split` must be a list, not a string.

---

    Code
      obliq_split_to_expr(list(columns = "x", values = 1, operator = ">"))
    Condition
      Error in `obliq_split_to_expr()`:
      ! `split` must have elements columns, values, operator, and threshold, missing threshold.

---

    Code
      obliq_split_to_expr(list(columns = "x", values = 1, threshold = 10))
    Condition
      Error in `obliq_split_to_expr()`:
      ! `split` must have elements columns, values, operator, and threshold, missing operator.

---

    Code
      obliq_split_to_expr(list(columns = "x", values = 1, operator = ">", threshold = 10,
        extra = "bad"))
    Condition
      Error in `obliq_split_to_expr()`:
      ! `split` must only have elements columns, values, operator, and threshold, found extra extra.

# obliq_split_to_expr() validates columns

    Code
      obliq_split_to_expr(list(columns = character(0), values = numeric(0), operator = ">",
      threshold = 10))
    Condition
      Error in `obliq_split_to_expr()`:
      ! columns must be a non-empty character vector.

---

    Code
      obliq_split_to_expr(list(columns = c("x", ""), values = c(1, 2), operator = ">",
      threshold = 10))
    Condition
      Error in `obliq_split_to_expr()`:
      ! columns must not contain empty strings.

---

    Code
      obliq_split_to_expr(list(columns = 123, values = c(1, 2), operator = ">",
      threshold = 10))
    Condition
      Error in `obliq_split_to_expr()`:
      ! columns must be a non-empty character vector.

# obliq_split_to_expr() validates values

    Code
      obliq_split_to_expr(list(columns = c("x", "y"), values = "not numeric",
      operator = ">", threshold = 10))
    Condition
      Error in `obliq_split_to_expr()`:
      ! values must be numeric, not a string.

---

    Code
      obliq_split_to_expr(list(columns = c("x", "y"), values = c(1, 2, 3), operator = ">",
      threshold = 10))
    Condition
      Error in `obliq_split_to_expr()`:
      ! values must have the same length as columns (2), not 3.

---

    Code
      obliq_split_to_expr(list(columns = c("x", "y", "z"), values = c(1, 2),
      operator = ">", threshold = 10))
    Condition
      Error in `obliq_split_to_expr()`:
      ! values must have the same length as columns (3), not 2.

# obliq_split_to_expr() validates operator

    Code
      obliq_split_to_expr(list(columns = "x", values = 1, operator = "!=", threshold = 10))
    Condition
      Error in `obliq_split_to_expr()`:
      ! operator must be one of <, <=, >, >=, or ==, not "!=".

---

    Code
      obliq_split_to_expr(list(columns = "x", values = 1, operator = "%in%",
        threshold = 10))
    Condition
      Error in `obliq_split_to_expr()`:
      ! operator must be one of <, <=, >, >=, or ==, not "%in%".

# obliq_split_to_expr() validates threshold

    Code
      obliq_split_to_expr(list(columns = "x", values = 1, operator = ">", threshold = "not numeric"))
    Condition
      Error in `obliq_split_to_expr()`:
      ! threshold must be a single numeric value, not a string.

---

    Code
      obliq_split_to_expr(list(columns = "x", values = 1, operator = ">", threshold = c(
        1, 2)))
    Condition
      Error in `obliq_split_to_expr()`:
      ! threshold must be a single numeric value, not a double vector.

