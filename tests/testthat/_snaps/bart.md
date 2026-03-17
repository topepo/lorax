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

# reformat_data_bart validates inputs

    Code
      reformat_data_bart("not a formula", data)
    Condition
      Error in `reformat_data_bart()`:
      ! `formula` must be a formula object.

---

    Code
      reformat_data_bart(y ~ x, "not a data.frame")
    Condition
      Error in `reformat_data_bart()`:
      ! `data` must be a data.frame.

# extract_rules.bart validates tree parameter

    Code
      extract_rules(fit, tree = 1.5)
    Condition
      Error in `extract_rules()`:
      ! `tree` must be a single integer, not a number.

---

    Code
      extract_rules(fit, tree = "1")
    Condition
      Error in `extract_rules()`:
      ! `tree` must be a single integer, not a string.

---

    Code
      extract_rules(fit, tree = c(1, 2))
    Condition
      Error in `extract_rules()`:
      ! `tree` must be a single integer, not a double vector.

---

    Code
      extract_rules(fit, tree = 0)
    Condition
      Error in `extract_rules()`:
      ! `tree` must be >= 1, not 0.

---

    Code
      extract_rules(fit, tree = 10)
    Condition
      Error in `extract_rules()`:
      ! `tree` must be between 1 and 3, not 10.

# extract_rules.bart validates chain parameter

    Code
      extract_rules(fit, tree = 1, chain = 1.5)
    Condition
      Error in `extract_rules()`:
      ! `chain` must be a single integer, not a number.

---

    Code
      extract_rules(fit, tree = 1, chain = "1")
    Condition
      Error in `extract_rules()`:
      ! `chain` must be a single integer, not a string.

---

    Code
      extract_rules(fit, tree = 1, chain = c(1, 2))
    Condition
      Error in `extract_rules()`:
      ! `chain` must be a single integer, not a double vector.

---

    Code
      extract_rules(fit, tree = 1, chain = 0)
    Condition
      Error in `extract_rules()`:
      ! `chain` must be >= 1, not 0.

---

    Code
      extract_rules(fit, tree = 1, chain = 2)
    Condition
      Error in `extract_rules()`:
      ! `chain` must be 1 for single-chain models, not 2.

# extract_rules.bart requires keeptrees = TRUE

    Code
      extract_rules(fit, tree = 1)
    Condition
      Error in `extract_rules()`:
      ! dbarts model must be fitted with `keeptrees = TRUE` to extract rules.

