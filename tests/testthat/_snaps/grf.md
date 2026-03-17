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

