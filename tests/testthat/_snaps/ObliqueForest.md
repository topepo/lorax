# extract_rules.ObliqueForest() validates tree argument

    Code
      extract_rules(forest, tree = "1")
    Condition
      Error in `extract_rules()`:
      ! `tree` must be a single integer.

---

    Code
      extract_rules(forest, tree = c(1, 2))
    Condition
      Error in `extract_rules()`:
      ! `tree` must be a single integer.

---

    Code
      extract_rules(forest, tree = 1.5)
    Condition
      Error in `extract_rules()`:
      ! `tree` must be a single integer.

---

    Code
      extract_rules(forest, tree = 0)
    Condition
      Error in `extract_rules()`:
      ! `tree` must be between 1 and 10, not 0.

---

    Code
      extract_rules(forest, tree = 11)
    Condition
      Error in `extract_rules()`:
      ! `tree` must be between 1 and 10, not 11.

