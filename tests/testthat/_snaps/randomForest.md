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

