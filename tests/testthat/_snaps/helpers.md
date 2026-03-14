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

