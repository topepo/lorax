# as.party.bart validates tree parameter

    Code
      as.party(fit, tree = 0, chain = 1, data = penguins)
    Condition
      Error in `as.party()`:
      ! `tree` must be >= 1, not 0.

---

    Code
      as.party(fit, tree = c(1, 2), chain = 1, data = penguins)
    Condition
      Error in `as.party()`:
      ! `tree` must be a single integer, not a double vector.

---

    Code
      as.party(fit, tree = "1", chain = 1, data = penguins)
    Condition
      Error in `as.party()`:
      ! `tree` must be a single integer, not a string.

---

    Code
      as.party(fit, tree = 10, chain = 1, data = penguins)
    Condition
      Error in `as.party()`:
      ! `tree` must be between 1 and 5, not 10.

# as.party.bart validates chain parameter

    Code
      as.party(fit, tree = 1, chain = 0, data = penguins)
    Condition
      Error in `as.party()`:
      ! `chain` must be >= 1, not 0.

---

    Code
      as.party(fit, tree = 1, chain = 5, data = penguins)
    Condition
      Error in `as.party()`:
      ! `chain` must be 1 for single-chain models, not 5.

---

    Code
      as.party(fit, tree = 1, chain = "1", data = penguins)
    Condition
      Error in `as.party()`:
      ! `chain` must be a single integer, not a string.

# as.party.bart requires data parameter

    Code
      as.party(fit, tree = 1, chain = 1)
    Condition
      Error in `as.party.bart()`:
      ! argument "data" is missing, with no default

# as.party.bart requires keeptrees = TRUE

    Code
      as.party(fit, tree = 1, chain = 1, data = penguins)
    Condition
      Error in `as.party()`:
      ! dbarts model must be fitted with `keeptrees = TRUE` to extract trees.

# active_predictors.bart() validates tree argument

    Code
      active_predictors(fit, tree = "1")
    Condition
      Error:
      ! `tree` must be an integer vector, not a string.

---

    Code
      active_predictors(fit, tree = 1.5)
    Condition
      Error:
      ! `tree` must be an integer vector, not a number.

---

    Code
      active_predictors(fit, tree = 0L)
    Condition
      Error:
      ! `tree` values must be between 1 and 5.

---

    Code
      active_predictors(fit, tree = 11L)
    Condition
      Error:
      ! `tree` values must be between 1 and 5.

# active_predictors.bart() requires keeptrees = TRUE

    Code
      active_predictors(fit, tree = 1L)
    Condition
      Error:
      ! dbarts model must be fitted with `keeptrees = TRUE` to extract active predictors.

