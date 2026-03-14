# extract_rules.xgb.Booster() validates tree parameter

    Code
      extract_rules(bst, tree = 1.5)
    Condition
      Error in `extract_rules()`:
      ! `tree` must be a single integer, not a number.

---

    Code
      extract_rules(bst, tree = c(1, 2))
    Condition
      Error in `extract_rules()`:
      ! `tree` must be a single integer, not a double vector.

---

    Code
      extract_rules(bst, tree = 100)
    Condition
      Error in `extract_rules()`:
      ! `tree` must be between 1 and 3, not 100.

---

    Code
      extract_rules(bst, tree = 0)
    Condition
      Error in `extract_rules()`:
      ! `tree` must be >= 1, not 0.

