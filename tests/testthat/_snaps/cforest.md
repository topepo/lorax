# extract_rules.cforest() validates tree argument

    Code
      extract_rules(cf, tree = "1")
    Condition
      Error:
      ! `tree` must be an integer vector, not a string.

---

    Code
      extract_rules(cf, tree = 1.5)
    Condition
      Error:
      ! `tree` must be an integer vector, not a number.

---

    Code
      extract_rules(cf, tree = 0L)
    Condition
      Error:
      ! `tree` values must be between 1 and 3.

---

    Code
      extract_rules(cf, tree = 10L)
    Condition
      Error:
      ! `tree` values must be between 1 and 3.

# active_predictors.cforest() validates tree argument

    Code
      active_predictors(cf, tree = "1")
    Condition
      Error:
      ! `tree` must be an integer vector, not a string.

---

    Code
      active_predictors(cf, tree = 1.5)
    Condition
      Error:
      ! `tree` must be an integer vector, not a number.

---

    Code
      active_predictors(cf, tree = 0L)
    Condition
      Error:
      ! `tree` values must be between 1 and 3.

---

    Code
      active_predictors(cf, tree = 10L)
    Condition
      Error:
      ! `tree` values must be between 1 and 3.

