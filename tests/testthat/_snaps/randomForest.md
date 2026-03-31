# as.party.randomForest validates tree parameter

    Code
      as.party(rf, tree = 0)
    Condition
      Error in `as.party()`:
      ! `tree` must be >= 1, not 0.

---

    Code
      as.party(rf, tree = 10)
    Condition
      Error in `as.party()`:
      ! `tree` must be between 1 and 5, not 10.

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

# as.party.randomForest requires keep.forest = TRUE

    Code
      as.party(rf, tree = 1)
    Condition
      Error in `as.party()`:
      ! randomForest model must have `keep.forest = TRUE` to extract trees.

# extract_rules.randomForest() validates tree argument

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
      ! `tree` values must be between 1 and 3.

---

    Code
      extract_rules(rf, tree = 10L)
    Condition
      Error:
      ! `tree` values must be between 1 and 3.

# extract_rules.randomForest() requires keep.forest = TRUE

    Code
      extract_rules(rf, tree = 1L)
    Condition
      Error:
      ! randomForest model must have `keep.forest = TRUE` to extract rules.

# active_predictors.randomForest() validates tree argument

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

# active_predictors.randomForest() requires keep.forest = TRUE

    Code
      active_predictors(rf, tree = 1L)
    Condition
      Error:
      ! randomForest model must be fitted with `keep.forest = TRUE` to extract active predictors.

# var_imp.randomForest() errors for invalid type

    Code
      var_imp(rf, type = "invalid")
    Condition
      Error in `var_imp()`:
      ! Importance type "invalid" not found.
      i Available options: "Adelie", "Chinstrap", "Gentoo", "MeanDecreaseAccuracy", and "MeanDecreaseGini"

