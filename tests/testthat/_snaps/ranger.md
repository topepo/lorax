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

