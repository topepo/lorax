# rect_split_to_expr() validates input structure

    Code
      rect_split_to_expr("not a list")
    Condition
      Error in `validate_split()`:
      ! `split` must be a list, not a string.

---

    Code
      rect_split_to_expr(list(column = "x", value = 1))
    Condition
      Error in `validate_split()`:
      ! `split` must have elements column, value, and operator, missing operator.

---

    Code
      rect_split_to_expr(list(column = "x", operator = "<"))
    Condition
      Error in `validate_split()`:
      ! `split` must have elements column, value, and operator, missing value.

---

    Code
      rect_split_to_expr(list(value = 1, operator = "<"))
    Condition
      Error in `validate_split()`:
      ! `split` must have elements column, value, and operator, missing column.

---

    Code
      rect_split_to_expr(list(column = "x", value = 1, operator = "<", extra = "bad"))
    Condition
      Error in `validate_split()`:
      ! `split` must only have elements column, value, and operator, found extra extra.

# rect_split_to_expr() validates column

    Code
      rect_split_to_expr(list(column = "", value = 1, operator = "<"))
    Condition
      Error in `validate_split()`:
      ! column must be a non-empty character string.

---

    Code
      rect_split_to_expr(list(column = c("x", "y"), value = 1, operator = "<"))
    Condition
      Error in `validate_split()`:
      ! column must be a non-empty character string.

---

    Code
      rect_split_to_expr(list(column = 123, value = 1, operator = "<"))
    Condition
      Error in `validate_split()`:
      ! column must be a non-empty character string.

# rect_split_to_expr() validates operator

    Code
      rect_split_to_expr(list(column = "x", value = 1, operator = "!="))
    Condition
      Error in `validate_split()`:
      ! operator must be one of <, <=, >, >=, ==, or %in%, not "!=".

---

    Code
      rect_split_to_expr(list(column = "x", value = 1, operator = "<<"))
    Condition
      Error in `validate_split()`:
      ! operator must be one of <, <=, >, >=, ==, or %in%, not "<<".

# rect_split_to_expr() validates value for operator

    Code
      rect_split_to_expr(list(column = "x", value = 1, operator = "%in%"))
    Condition
      Error in `validate_split()`:
      ! value must be a character vector when operator is "%in%".

---

    Code
      rect_split_to_expr(list(column = "x", value = "red", operator = "%in%"))
    Condition
      Error in `validate_split()`:
      ! Single character values should use "==" operator, not "%in%".

---

    Code
      rect_split_to_expr(list(column = "x", value = c(1, 2), operator = "<"))
    Condition
      Error in `validate_split()`:
      ! value must be length 1 for operator "<".

---

    Code
      rect_split_to_expr(list(column = "x", value = c("a", "b"), operator = "=="))
    Condition
      Error in `validate_split()`:
      ! value must be length 1 for operator "==".

