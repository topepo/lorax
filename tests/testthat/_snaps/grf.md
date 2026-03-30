# as.party.grf validates tree parameter

    Code
      as.party(rf, tree = 0)
    Condition
      Error in `as.party()`:
      ! `tree` must be >= 1, not 0.

---

    Code
      as.party(rf, tree = c(1, 2))
    Condition
      Error in `as.party()`:
      ! `tree` must be a single integer, not a double vector.

---

    Code
      as.party(rf, tree = "1")
    Condition
      Error in `as.party()`:
      ! `tree` must be a single integer, not a string.

---

    Code
      as.party(rf, tree = 10)
    Condition
      Error in `as.party()`:
      ! `tree` must be between 1 and 6, not 10.

# extract_rules.grf() validates tree argument

    Code
      extract_rules(rf, tree = "1")
    Condition
      Error:
      ! `tree` must be an integer vector, not a string.

---

    Code
      extract_rules(rf, tree = 1.5)
    Condition
      Error:
      ! `tree` must be an integer vector, not a number.

---

    Code
      extract_rules(rf, tree = 0L)
    Condition
      Error:
      ! `tree` values must be between 1 and 4.

---

    Code
      extract_rules(rf, tree = 10L)
    Condition
      Error:
      ! `tree` values must be between 1 and 4.

# active_predictors.regression_forest() validates tree argument

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
      ! `tree` values must be between 1 and 6, not 0.

---

    Code
      active_predictors(rf, tree = 9999L)
    Condition
      Error:
      ! `tree` values must be between 1 and 6, not 9999.

