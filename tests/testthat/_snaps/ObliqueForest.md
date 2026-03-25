# extract_rules.ObliqueForest() validates tree argument

    Code
      extract_rules(forest, tree = "1")
    Condition
      Error in `extract_rules()`:
      ! `tree` must be a single integer.

---

    Code
      extract_rules(forest, tree = c(1, 2))
    Condition
      Error in `extract_rules()`:
      ! `tree` must be a single integer.

---

    Code
      extract_rules(forest, tree = 1.5)
    Condition
      Error in `extract_rules()`:
      ! `tree` must be a single integer.

---

    Code
      extract_rules(forest, tree = 0)
    Condition
      Error in `extract_rules()`:
      ! `tree` must be between 1 and 10, not 0.

---

    Code
      extract_rules(forest, tree = 11)
    Condition
      Error in `extract_rules()`:
      ! `tree` must be between 1 and 10, not 11.

# active_predictors.ObliqueForest() validates tree argument

    Code
      active_predictors(forest, tree = "1")
    Condition
      Error:
      ! `tree` must be an integer vector, not a string.

---

    Code
      active_predictors(forest, tree = 1.5)
    Condition
      Error:
      ! `tree` must be an integer vector, not a number.

---

    Code
      active_predictors(forest, tree = 0L)
    Condition
      Error:
      ! `tree` values must be between 1 and 10, not 0.

---

    Code
      active_predictors(forest, tree = 11L)
    Condition
      Error:
      ! `tree` values must be between 1 and 10, not 11.

