# as.party.ranger validates tree parameter

    Code
      as.party(rf, tree = 0, data = penguins)
    Condition
      Error in `as.party()`:
      ! `tree` must be >= 1, not 0.

---

    Code
      as.party(rf, tree = 10, data = penguins)
    Condition
      Error in `as.party()`:
      ! `tree` must be between 1 and 5, not 10.

---

    Code
      as.party(rf, tree = c(1, 2), data = penguins)
    Condition
      Error in `as.party()`:
      ! `tree` must be a single integer, not a double vector.

---

    Code
      as.party(rf, tree = "1", data = penguins)
    Condition
      Error in `as.party()`:
      ! `tree` must be a single integer, not a string.

# as.party.ranger requires data parameter

    Code
      as.party(rf, tree = 1)
    Condition
      Error in `as.party()`:
      ! `data` is required for `as.party.ranger()`.

# as.party.ranger requires write.forest = TRUE

    Code
      as.party(rf, tree = 1, data = penguins)
    Condition
      Error in `as.party()`:
      ! ranger model must have `write.forest = TRUE` to extract trees.

# extract_rules.ranger() validates tree argument

    Code
      extract_rules(rf, tree = "1", data = data)
    Condition
      Error:
      ! `tree` must be an integer vector, not a string.

---

    Code
      extract_rules(rf, tree = 1.5, data = data)
    Condition
      Error:
      ! `tree` must be an integer vector, not a number.

---

    Code
      extract_rules(rf, tree = 0L, data = data)
    Condition
      Error:
      ! `tree` values must be between 1 and 3.

---

    Code
      extract_rules(rf, tree = 10L, data = data)
    Condition
      Error:
      ! `tree` values must be between 1 and 3.

# extract_rules.ranger() requires data parameter

    Code
      extract_rules(rf, tree = 1L)
    Condition
      Error in `extract_rules()`:
      ! `data` is required for `extract_rules.ranger()`.

---

    Code
      extract_rules(rf, tree = 1L, data = NULL)
    Condition
      Error in `extract_rules()`:
      ! `data` is required for `extract_rules.ranger()`.

# extract_rules.ranger() requires write.forest = TRUE

    Code
      extract_rules(rf, tree = 1L, data = data)
    Condition
      Error:
      ! ranger model must have `write.forest = TRUE` to extract rules.

# active_predictors.ranger() validates tree argument

    Code
      active_predictors(rf, tree = "1")
    Condition
      Error:
      ! `tree` must be an integer vector, not a string.

---

    Code
      active_predictors(rf, tree = 1.5)
    Condition
      Error:
      ! `tree` must be an integer vector, not a number.

---

    Code
      active_predictors(rf, tree = 0L)
    Condition
      Error:
      ! `tree` values must be between 1 and 5.

---

    Code
      active_predictors(rf, tree = 11L)
    Condition
      Error:
      ! `tree` values must be between 1 and 5.

# active_predictors.ranger() requires write.forest = TRUE

    Code
      active_predictors(rf, tree = 1L)
    Condition
      Error:
      ! ranger model must be fitted with `write.forest = TRUE` to extract active predictors.

# var_imp.ranger() errors when importance not calculated

    Code
      var_imp(rf)
    Condition
      Error in `var_imp()`:
      ! ranger model must be fitted with `importance` parameter to compute variable importance.
      i Use `importance = 'impurity'` or `importance = 'permutation'` when calling `ranger()`.

