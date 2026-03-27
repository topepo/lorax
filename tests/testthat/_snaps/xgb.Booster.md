# as.party.xgb.Booster validates tree parameter

    Code
      as.party(bst, tree = 0, data = penguins)
    Condition
      Error in `as.party()`:
      ! `tree` must be >= 1, not 0.

---

    Code
      as.party(bst, tree = c(1, 2), data = penguins)
    Condition
      Error in `as.party()`:
      ! `tree` must be a single integer, not a double vector.

---

    Code
      as.party(bst, tree = "1", data = penguins)
    Condition
      Error in `as.party()`:
      ! `tree` must be a single integer, not a string.

---

    Code
      as.party(bst, tree = 100, data = penguins)
    Condition
      Error in `as.party()`:
      ! `tree` must be between 1 and 15, not 100.

# as.party.xgb.Booster requires response in data

    Code
      as.party(bst, tree = 1, data = penguins[, -1])
    Condition
      Error in `as.party()`:
      ! `data` must contain the response variable in addition to predictors.

# active_predictors.xgb.Booster() validates tree argument

    Code
      active_predictors(bst, tree = "1")
    Condition
      Error:
      ! `tree` must be an integer vector, not a string.

---

    Code
      active_predictors(bst, tree = 1.5)
    Condition
      Error:
      ! `tree` must be an integer vector, not a number.

---

    Code
      active_predictors(bst, tree = 0L)
    Condition
      Error:
      ! `tree` values must be between 1 and 5, not 0.

---

    Code
      active_predictors(bst, tree = 999L)
    Condition
      Error:
      ! `tree` values must be between 1 and 5, not 999.

