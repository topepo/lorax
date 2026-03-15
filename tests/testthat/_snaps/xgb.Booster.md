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

