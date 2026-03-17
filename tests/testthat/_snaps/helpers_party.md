# build_partysplit validates inputs

    Code
      build_partysplit(NA, 5.5)
    Condition
      Error in `build_partysplit()`:
      ! Invalid varid for partysplit: NA

---

    Code
      build_partysplit(integer(0), 5.5)
    Condition
      Error in `build_partysplit()`:
      ! Invalid varid for partysplit:

---

    Code
      build_partysplit(1, NA)
    Condition
      Error in `build_partysplit()`:
      ! Invalid threshold for partysplit: NA

---

    Code
      build_partysplit(1, numeric(0))
    Condition
      Error in `build_partysplit()`:
      ! Invalid threshold for partysplit:

---

    Code
      build_partysplit(1, "5.5")
    Condition
      Error in `build_partysplit()`:
      ! Invalid threshold for partysplit: "5.5"

# create_party_object validates inputs

    Code
      create_party_object(list(id = 1), data)
    Condition
      Error in `create_party_object()`:
      ! `node` must be a partynode object.

---

    Code
      create_party_object(node, list(x = 1:10))
    Condition
      Error in `create_party_object()`:
      ! `data` must be a data.frame.

# validate_and_select_data validates required variables

    Code
      validate_and_select_data(data, c("x1", "x3"))
    Condition
      Error in `validate_and_select_data()`:
      ! `data` must contain variables: x3.

---

    Code
      validate_and_select_data(data, c("missing", "also_missing"))
    Condition
      Error in `validate_and_select_data()`:
      ! `data` must contain variables: missing and also_missing.

# validate_party_node_info detects missing response

    Code
      validate_party_node_info(party_obj, action = "error")
    Condition
      Error in `validate_party_node_info()`:
      ! Party object will show asterisks (*) in terminal node summaries.
      i The party object has fitted values but no response variable.
      i Provide the `data` parameter with response variable included.

# build_partynode_from_tabular errors on missing node

    Code
      build_partynode_from_tabular(tree_df = tree_df, node_id = 0L, node_id_col = "node_id",
        left_child_col = "left_child", right_child_col = "right_child",
        split_var_col = "split_var", split_val_col = "split_val", is_leaf_col = NULL,
        prediction_col = "prediction", var_names = c("x1", "x2"), zero_indexed = TRUE)
    Condition
      Error in `build_partynode_from_tabular()`:
      ! Node 2 not found in tree structure.

# build_partynode_from_tabular errors on missing variable name

    Code
      build_partynode_from_tabular(tree_df = tree_df, node_id = 0L, node_id_col = "node_id",
        left_child_col = "left_child", right_child_col = "right_child",
        split_var_col = "split_var", split_val_col = "split_val", is_leaf_col = NULL,
        prediction_col = "prediction", var_names = c("x1", "x2"), zero_indexed = TRUE)
    Condition
      Error in `build_partynode_from_tabular()`:
      ! Variable missing_var not found in var_names.

