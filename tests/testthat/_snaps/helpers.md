# combine_rule_elements() validates exprs is a list

    Code
      combine_rule_elements("not a list")
    Condition
      Error in `combine_rule_elements()`:
      ! `exprs` must be a list, not a string.

---

    Code
      combine_rule_elements(rlang::expr(x > 5))
    Condition
      Error in `combine_rule_elements()`:
      ! `exprs` must be a list, not a call.

---

    Code
      combine_rule_elements(123)
    Condition
      Error in `combine_rule_elements()`:
      ! `exprs` must be a list, not a number.

# combine_rule_elements() validates operator

    Code
      combine_rule_elements(list(expr), operator = "invalid")
    Condition
      Error in `combine_rule_elements()`:
      ! `operator` must be & or |, not "invalid".

---

    Code
      combine_rule_elements(list(expr), operator = "&&")
    Condition
      Error in `combine_rule_elements()`:
      ! `operator` must be & or |, not "&&".

---

    Code
      combine_rule_elements(list(expr), operator = "and")
    Condition
      Error in `combine_rule_elements()`:
      ! `operator` must be & or |, not "and".

# combine_rule_elements() validates list elements are expressions

    Code
      combine_rule_elements(list(expr, "not an expression"))
    Condition
      Error in `combine_rule_elements()`:
      ! `exprs` element 2 must be an expression, not a string.

---

    Code
      combine_rule_elements(list(expr, 123))
    Condition
      Error in `combine_rule_elements()`:
      ! `exprs` element 2 must be an expression, not a number.

---

    Code
      combine_rule_elements(list(expr, TRUE))
    Condition
      Error in `combine_rule_elements()`:
      ! `exprs` element 2 must be an expression, not `TRUE`.

# combine_rule_elements() validates against NULL elements

    Code
      combine_rule_elements(list(expr, NULL))
    Condition
      Error in `combine_rule_elements()`:
      ! `exprs` element 2 is "NULL".

---

    Code
      combine_rule_elements(list(NULL, expr))
    Condition
      Error in `combine_rule_elements()`:
      ! `exprs` element 1 is "NULL".

# rule_text() validates expr is an expression

    Code
      rule_text("not an expression")
    Condition
      Error in `rule_text()`:
      ! `expr` must be an expression, not a string.

---

    Code
      rule_text(123)
    Condition
      Error in `rule_text()`:
      ! `expr` must be an expression, not a number.

---

    Code
      rule_text(NULL)
    Condition
      Error in `rule_text()`:
      ! `expr` must be an expression, not NULL.

# rule_text() validates bullets is logical

    Code
      rule_text(expr, bullets = "true")
    Condition
      Error in `rule_text()`:
      ! `bullets` must be a single logical value, not a string.

---

    Code
      rule_text(expr, bullets = 1)
    Condition
      Error in `rule_text()`:
      ! `bullets` must be a single logical value, not a number.

---

    Code
      rule_text(expr, bullets = c(TRUE, FALSE))
    Condition
      Error in `rule_text()`:
      ! `bullets` must be a single logical value, not a logical vector.

---

    Code
      rule_text(expr, bullets = NA)
    Condition
      Error in `rule_text()`:
      ! `bullets` must be a single logical value, not `NA`.

# rule_text() validates digits is a positive integer

    Code
      rule_text(expr, digits = "4")
    Condition
      Error in `rule_text()`:
      ! `digits` must be a single positive integer, not a string.

---

    Code
      rule_text(expr, digits = -1)
    Condition
      Error in `rule_text()`:
      ! `digits` must be a single positive integer, not a number.

---

    Code
      rule_text(expr, digits = 0)
    Condition
      Error in `rule_text()`:
      ! `digits` must be a single positive integer, not a number.

---

    Code
      rule_text(expr, digits = 1.5)
    Condition
      Error in `rule_text()`:
      ! `digits` must be a single positive integer, not a number.

---

    Code
      rule_text(expr, digits = c(1, 2))
    Condition
      Error in `rule_text()`:
      ! `digits` must be a single positive integer, not a double vector.

---

    Code
      rule_text(expr, digits = NA)
    Condition
      Error in `rule_text()`:
      ! `digits` must be a single positive integer, not `NA`.

# rule_text() validates max_width is numeric and >= 4

    Code
      rule_text(expr, max_width = "10")
    Condition
      Error in `rule_text()`:
      ! `max_width` must be a single numeric value >= 4, not a string.

---

    Code
      rule_text(expr, max_width = 3)
    Condition
      Error in `rule_text()`:
      ! `max_width` must be a single numeric value >= 4, not a number.

---

    Code
      rule_text(expr, max_width = c(10, 20))
    Condition
      Error in `rule_text()`:
      ! `max_width` must be a single numeric value >= 4, not a double vector.

---

    Code
      rule_text(expr, max_width = NA)
    Condition
      Error in `rule_text()`:
      ! `max_width` must be a single numeric value >= 4, not `NA`.

