# extract_rules.xgb.Booster validates tree parameter

    Code
      extract_rules(bst, tree = 0)
    Condition
      Error in `extract_rules()`:
      ! `tree` must be >= 1, not 0.

---

    Code
      extract_rules(bst, tree = c(1, 2))
    Condition
      Error in `extract_rules()`:
      ! `tree` must be a single integer, not a double vector.

---

    Code
      extract_rules(bst, tree = "1")
    Condition
      Error in `extract_rules()`:
      ! `tree` must be a single integer, not a string.

---

    Code
      extract_rules(bst, tree = 100)
    Condition
      Error in `extract_rules()`:
      ! `tree` must be between 1 and 2, not 100.

# extract_rules.lgb.Booster validates tree parameter

    Code
      extract_rules(bst, tree = 0)
    Condition
      Error in `extract_rules()`:
      ! `tree` must be >= 1, not 0.

---

    Code
      extract_rules(bst, tree = c(1, 2))
    Condition
      Error in `extract_rules()`:
      ! `tree` must be a single integer, not a double vector.

---

    Code
      extract_rules(bst, tree = "1")
    Condition
      Error in `extract_rules()`:
      ! `tree` must be a single integer, not a string.

---

    Code
      extract_rules(bst, tree = 100)
    Condition
      Error in `extract_rules()`:
      ! `tree` must be between 1 and 2, not 100.

