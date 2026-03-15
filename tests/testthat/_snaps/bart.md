# extract_rules.bart() validates tree parameter

    Code
      extract_rules(bart_fit, tree = 1.5)
    Condition
      Error in `extract_rules()`:
      ! `tree` must be a single integer, not a number.

---

    Code
      extract_rules(bart_fit, tree = c(1, 2))
    Condition
      Error in `extract_rules()`:
      ! `tree` must be a single integer, not a double vector.

---

    Code
      extract_rules(bart_fit, tree = 100)
    Condition
      Error in `extract_rules()`:
      ! `tree` must be between 1 and 10, not 100.

---

    Code
      extract_rules(bart_fit, tree = 0)
    Condition
      Error in `extract_rules()`:
      ! `tree` must be >= 1, not 0.

# extract_rules.bart() validates chain parameter

    Code
      extract_rules(bart_fit, chain = 1.5)
    Condition
      Error in `extract_rules()`:
      ! `chain` must be a single integer, not a number.

---

    Code
      extract_rules(bart_fit, chain = c(1, 2))
    Condition
      Error in `extract_rules()`:
      ! `chain` must be a single integer, not a double vector.

---

    Code
      extract_rules(bart_fit, chain = 100)
    Condition
      Error in `extract_rules()`:
      ! `chain` must be 1 for single-chain models, not 100.

---

    Code
      extract_rules(bart_fit, chain = 0)
    Condition
      Error in `extract_rules()`:
      ! `chain` must be >= 1, not 0.

# extract_rules.bart() requires keeptrees = TRUE

    Code
      extract_rules(bart_fit_no_trees, tree = 1)
    Condition
      Error in `extract_rules()`:
      ! BART model must be fitted with `keeptrees = TRUE` to extract rules.

