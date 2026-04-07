# skip("TOOOO LONG")

test_that("extract_rules.party() returns correct structure", {
  skip_if_not_installed("partykit")
  skip_if_not_installed("palmerpenguins")

  tree <- get_penguins_tree()
  rules <- extract_rules(tree)

  expect_s3_class(rules, "rule_set_party")
  expect_s3_class(rules, "rule_set")
  expect_s3_class(rules, "tbl_df")
  expect_named(rules, c("id", "rules"))
  expect_type(rules$id, "integer")
  expect_type(rules$rules, "list")
})

test_that("extract_rules.party() extracts all terminal nodes", {
  skip_if_not_installed("partykit")
  skip_if_not_installed("palmerpenguins")

  tree <- get_penguins_tree()
  rules <- extract_rules(tree)
  terminal_nodes <- partykit::nodeids(tree, terminal = TRUE)
  expect_equal(sort(rules$id), sort(terminal_nodes))
})

test_that("extract_rules.party() produces valid R expressions", {
  skip_if_not_installed("partykit")
  skip_if_not_installed("palmerpenguins")

  tree <- get_penguins_tree()
  rules <- extract_rules(tree)

  for (i in seq_len(nrow(rules))) {
    expect_true(is.language(rules$rules[[i]]))
  }
})

test_that("extract_rules.party() rules evaluate correctly", {
  skip_if_not_installed("partykit")
  skip_if_not_installed("palmerpenguins")

  tree <- get_penguins_tree()
  penguins <- palmerpenguins::penguins
  rules <- extract_rules(tree)

  for (i in seq_len(nrow(rules))) {
    rule_expr <- rules$rules[[i]]
    result <- eval(rule_expr, penguins)

    expect_type(result, "logical")
    expect_equal(length(result), nrow(penguins))
    expect_true(sum(result, na.rm = TRUE) > 0)
  }
})

test_that("extract_rules.party() works with numeric-only splits", {
  skip_if_not_installed("partykit")
  skip_if_not_installed("palmerpenguins")

  penguins <- palmerpenguins::penguins
  num_tree <- partykit::ctree(
    body_mass_g ~ bill_length_mm + bill_depth_mm,
    data = penguins
  )
  rules <- extract_rules(num_tree)

  expect_s3_class(rules, "rule_set_party")
  expect_true(nrow(rules) > 0)

  for (i in seq_len(nrow(rules))) {
    expect_true(is.language(rules$rules[[i]]))
  }
})

test_that("extract_rules.party() works with categorical splits", {
  skip_if_not_installed("partykit")
  skip_if_not_installed("palmerpenguins")

  penguins <- palmerpenguins::penguins
  cat_tree <- partykit::ctree(species ~ ., data = penguins)
  rules <- extract_rules(cat_tree)

  expect_s3_class(rules, "rule_set_party")
  expect_true(nrow(rules) > 0)
})

test_that("extract_rules.party() works with single-node tree", {
  skip_if_not_installed("partykit")
  skip_if_not_installed("palmerpenguins")

  # Create a tree with no splits
  penguins <- palmerpenguins::penguins
  single_tree <- partykit::ctree(
    body_mass_g ~ bill_length_mm,
    data = penguins,
    mincriterion = 1
  )
  rules <- extract_rules(single_tree)

  expect_s3_class(rules, "rule_set_party")
  expect_equal(nrow(rules), 1)
  expect_equal(rules$id, 1L)
  expect_equal(rules$rules[[1]], rlang::expr(TRUE))
})

test_that("extract_rules.party() integrates with rule_text()", {
  skip_if_not_installed("partykit")
  skip_if_not_installed("palmerpenguins")

  tree <- get_penguins_tree()
  rules <- extract_rules(tree)

  for (i in seq_len(nrow(rules))) {
    text <- rule_text(rules$rules[[i]])
    expect_type(text, "character")
    expect_true(nchar(text) > 0)

    text_bullets <- rule_text(rules$rules[[i]], bullets = TRUE)
    expect_type(text_bullets, "character")
    expect_true(nchar(text_bullets) > 0)
  }
})

test_that("extract_rules.party() works with wa_trees data", {
  skip_if_not_installed("partykit")

  load(system.file(package = "lorax", "wa_trees.RData"))
  wa_tree <- partykit::ctree(class ~ ., data = wa_trees)
  rules <- extract_rules(wa_tree)

  expect_s3_class(rules, "rule_set_party")
  expect_true(nrow(rules) > 0)

  # Check that rules are sorted by id
  expect_true(all(diff(rules$id) > 0))

  # Check that all rules evaluate correctly
  for (i in seq_len(nrow(rules))) {
    result <- eval(rules$rules[[i]], wa_trees)

    expect_type(result, "logical")
    expect_equal(length(result), nrow(wa_trees))
    expect_true(sum(result) > 0)
  }
})

test_that("extract_rules.party() handles no-split data", {
  skip_if_not_installed("partykit")

  # Data where no good split can be made
  null_data <- data.frame(y = 1:10, x = rep(1:5, each = 2))
  tree <- partykit::ctree(y ~ ., data = null_data)

  rules <- extract_rules(tree)

  expect_s3_class(rules, "rule_set_party")
  expect_equal(nrow(rules), 1)
  expect_equal(rules$id, 1L)
  expect_equal(rules$rules[[1]], rlang::expr(TRUE))
})

# Tests for active_predictors.party() -----------------------------------------

test_that("active_predictors.party() returns correct structure", {
  skip_if_not_installed("partykit")

  load(system.file(package = "lorax", "wa_trees.RData"))
  wa_tree <- partykit::ctree(class ~ ., data = wa_trees)
  result <- active_predictors(wa_tree)

  expect_s3_class(result, "tbl_df")
  expect_named(result, "active_predictors")
  expect_type(result$active_predictors, "list")
  expect_length(result$active_predictors, 1)
  expect_type(result$active_predictors[[1]], "character")
})

test_that("active_predictors.party() extracts correct variables", {
  skip_if_not_installed("partykit")

  load(system.file(package = "lorax", "wa_trees.RData"))
  wa_tree <- partykit::ctree(
    class ~ elevation + eastness + county,
    data = wa_trees
  )
  result <- active_predictors(wa_tree)

  # Get expected variables from non-terminal nodes
  non_term <- partykit::nodeids(wa_tree, terminal = FALSE)
  expected_vars <- character()
  for (node_id in non_term) {
    node <- partykit::nodeapply(wa_tree, ids = node_id, by_node = TRUE)[[1]]
    sp <- partykit::split_node(node)
    if (!is.null(sp)) {
      var_idx <- partykit::varid_split(sp)
      expected_vars <- c(expected_vars, names(wa_tree$data)[var_idx])
    }
  }
  expected <- unique(expected_vars)

  expect_equal(sort(result$active_predictors[[1]]), sort(expected))
})

test_that("active_predictors.party() works with numeric predictors", {
  skip_if_not_installed("partykit")

  load(system.file(package = "lorax", "wa_trees.RData"))
  num_tree <- partykit::ctree(
    elevation ~ year + roughness + dew_temp,
    data = wa_trees
  )
  result <- active_predictors(num_tree)

  expect_s3_class(result, "tbl_df")
  expect_type(result$active_predictors[[1]], "character")
  # All predictors should be from the model formula
  expect_true(all(
    result$active_predictors[[1]] %in%
      c("year", "roughness", "dew_temp")
  ))
})

test_that("active_predictors.party() works with factor predictors", {
  skip_if_not_installed("partykit")

  load(system.file(package = "lorax", "wa_trees.RData"))
  factor_tree <- partykit::ctree(
    county ~ class + elevation + roughness,
    data = wa_trees
  )
  result <- active_predictors(factor_tree)

  expect_s3_class(result, "tbl_df")
  expect_type(result$active_predictors[[1]], "character")
  expect_true(length(result$active_predictors[[1]]) >= 0)
  # All predictors should be from the model formula
  expect_true(all(
    result$active_predictors[[1]] %in%
      c("class", "elevation", "roughness")
  ))
})

test_that("active_predictors.party() works with mixed numeric and factor predictors", {
  skip_if_not_installed("partykit")

  load(system.file(package = "lorax", "wa_trees.RData"))
  mixed_tree <- partykit::ctree(
    class ~ elevation + county + roughness,
    data = wa_trees
  )
  result <- active_predictors(mixed_tree)

  expect_s3_class(result, "tbl_df")
  expect_type(result$active_predictors[[1]], "character")
  # Should have at least one predictor
  expect_true(length(result$active_predictors[[1]]) >= 1)
  # All predictors should be from the model formula
  expect_true(all(
    result$active_predictors[[1]] %in%
      c("elevation", "county", "roughness")
  ))
})

test_that("active_predictors.party() handles single-node tree", {
  skip_if_not_installed("partykit")

  load(system.file(package = "lorax", "wa_trees.RData"))
  single_tree <- partykit::ctree(
    class ~ elevation,
    data = wa_trees,
    mincriterion = 1
  )
  result <- active_predictors(single_tree)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 1)
  expect_type(result$active_predictors[[1]], "character")
  expect_length(result$active_predictors[[1]], 0)
})

test_that("active_predictors.party() handles tree with repeated variables", {
  skip_if_not_installed("partykit")

  load(system.file(package = "lorax", "wa_trees.RData"))
  tree <- partykit::ctree(class ~ ., data = wa_trees)
  result <- active_predictors(tree)

  # Should return unique variables only (no duplicates)
  expect_equal(
    length(result$active_predictors[[1]]),
    length(unique(result$active_predictors[[1]]))
  )
})

test_that("active_predictors.party() handles no-split data", {
  skip_if_not_installed("partykit")

  # Data where no good split can be made
  null_data <- data.frame(y = 1:10, x = rep(1:5, each = 2))
  tree <- partykit::ctree(y ~ ., data = null_data)
  result <- active_predictors(tree)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 1)
  expect_type(result$active_predictors[[1]], "character")
  expect_length(result$active_predictors[[1]], 0)
})

# Tests for var_imp.party() --------------------------------------------------

test_that("var_imp.party() returns correct structure", {
  skip_if_not_installed("partykit")

  wa_trees <- get_wa_trees_data()[1:200, ]
  set.seed(427)
  tree <- partykit::ctree(
    class ~ elevation + county,
    data = wa_trees
  )
  result <- var_imp(tree)

  expect_s3_class(result, "tbl_df")
  expect_named(result, c("term", "estimate"))
  expect_type(result$term, "character")
  expect_type(result$estimate, "double")
})

test_that("var_imp.party() extracts variable importance scores", {
  skip_if_not_installed("partykit")

  wa_trees <- get_wa_trees_data()[1:200, ]
  set.seed(638)
  tree <- partykit::ctree(
    class ~ elevation + county,
    data = wa_trees
  )
  result <- var_imp(tree)

  # Should have all predictors
  expect_true(nrow(result) > 0)

  # All estimates should be numeric
  expect_true(all(is.numeric(result$estimate)))
})

test_that("var_imp.party() with complete=TRUE fills missing predictors", {
  skip_if_not_installed("partykit")

  wa_trees <- get_wa_trees_data()[1:200, ]
  set.seed(749)
  tree <- partykit::ctree(
    class ~ elevation + roughness + dew_temp,
    data = wa_trees
  )
  result <- var_imp(tree, complete = TRUE)

  # Should have all 3 predictors
  expect_equal(nrow(result), 3)
  expect_setequal(result$term, c("elevation", "roughness", "dew_temp"))
})

test_that("var_imp.party() with complete=FALSE returns only used predictors", {
  skip_if_not_installed("partykit")

  wa_trees <- get_wa_trees_data()[1:200, ]
  set.seed(851)
  tree <- partykit::ctree(
    class ~ elevation + county,
    data = wa_trees
  )
  result <- var_imp(tree, complete = FALSE)

  # Should only have predictors with importance scores
  imp <- partykit::varimp(tree)
  expected_vars <- names(imp)
  expect_equal(nrow(result), length(expected_vars))
  expect_setequal(result$term, expected_vars)
})

test_that("var_imp.party() works with numeric predictors only", {
  skip_if_not_installed("partykit")

  data <- get_regression_data()
  set.seed(963)
  tree <- partykit::ctree(y ~ predictor_01 + predictor_02, data = data)
  result <- var_imp(tree, complete = TRUE)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2)
  expect_setequal(result$term, c("x1", "x2"))
})

test_that("var_imp.party() works with factor predictors", {
  skip_if_not_installed("partykit")

  wa_trees <- get_wa_trees_data()[1:200, ]
  set.seed(174)
  tree <- partykit::ctree(
    county ~ class + elevation,
    data = wa_trees
  )
  result <- var_imp(tree, complete = TRUE)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2)
  expect_setequal(result$term, c("class", "elevation"))
})

test_that("var_imp.party() works with mixed numeric and factor predictors", {
  skip_if_not_installed("partykit")

  wa_trees <- get_wa_trees_data()[1:200, ]
  set.seed(285)
  tree <- partykit::ctree(
    elevation ~ class + roughness,
    data = wa_trees
  )
  result <- var_imp(tree, complete = TRUE)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2)
  expect_setequal(result$term, c("class", "roughness"))
})

test_that("var_imp.party() returns all expected predictors", {
  skip_if_not_installed("partykit")

  wa_trees <- get_wa_trees_data()[1:200, ]
  set.seed(396)
  tree <- partykit::ctree(
    class ~ elevation + county,
    data = wa_trees
  )
  result <- var_imp(tree, complete = FALSE)

  # Check that all predictors from partykit::varimp() are present
  expected_names <- names(partykit::varimp(tree))
  expect_setequal(result$term, expected_names)

  # Check that estimates are numeric
  expect_true(all(is.numeric(result$estimate)))
})

test_that("var_imp.party() works with classification tree", {
  skip_if_not_installed("partykit")

  wa_trees <- get_wa_trees_data()[1:200, ]
  set.seed(507)
  tree <- partykit::ctree(
    class ~ elevation + county,
    data = wa_trees
  )
  result <- var_imp(tree)

  expect_s3_class(result, "tbl_df")
  expect_true(nrow(result) > 0)
})

test_that("var_imp.party() works with regression tree", {
  skip_if_not_installed("partykit")

  data <- get_regression_data()
  set.seed(618)
  tree <- partykit::ctree(y ~ predictor_01 + predictor_02, data = data)
  result <- var_imp(tree)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2)
  expect_setequal(result$term, c("x1", "x2"))
})

test_that("var_imp.party() handles tree with no splits", {
  skip_if_not_installed("partykit")

  # Data where tree may have no splits
  null_data <- data.frame(y = 1:10, x = rep(1:5, each = 2))
  set.seed(729)
  tree <- partykit::ctree(y ~ ., data = null_data)
  result <- var_imp(tree, complete = TRUE)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 1)
  expect_equal(result$term, "x")
  # Should have zero importance for unused predictor
  expect_equal(result$estimate, 0)
})

test_that("var_imp.party() handles many predictors", {
  skip_if_not_installed("partykit")

  set.seed(834)
  n <- 100
  p <- 5
  X <- as.data.frame(matrix(rnorm(n * p), nrow = n, ncol = p))
  colnames(X) <- paste0("x", 1:p)
  X$y <- X$x1 + X$x2 + rnorm(n)

  set.seed(941)
  tree <- partykit::ctree(y ~ ., data = X)
  result <- var_imp(tree)

  expect_equal(nrow(result), p)
  expect_setequal(result$term, paste0("x", 1:p))
})

test_that("var_imp.party() handles constrained splits", {
  skip_if_not_installed("partykit")

  # Force a constrained tree
  set.seed(152)
  small_data <- data.frame(
    y = rnorm(60),
    x1 = rnorm(60),
    x2 = rnorm(60)
  )

  set.seed(263)
  tree <- partykit::ctree(
    y ~ predictor_01 + predictor_02,
    data = small_data,
    control = partykit::ctree_control(minsplit = 20)
  )

  result <- var_imp(tree, complete = TRUE)

  expect_s3_class(result, "tbl_df")
  expect_named(result, c("term", "estimate"))
  expect_equal(nrow(result), 2)
  expect_setequal(result$term, c("x1", "x2"))
})

test_that("extract_rules.party() works with single numeric predictor", {
  skip_if_not_installed("partykit")

  data <- get_single_numeric_data()
  model <- partykit::ctree(y ~ x, data = data)
  rules <- extract_rules(model)

  expect_s3_class(rules, "rule_set")
  expect_true(nrow(rules) > 0)

  # Check that rules are valid expressions
  for (i in seq_len(nrow(rules))) {
    expect_true(is.language(rules$rules[[i]]))
  }
})

test_that("extract_rules.party() works with single factor predictor", {
  skip_if_not_installed("partykit")

  data <- get_single_factor_data()
  model <- partykit::ctree(y ~ x, data = data)
  rules <- extract_rules(model)

  expect_s3_class(rules, "rule_set")
  expect_true(nrow(rules) > 0)

  # Check that rules are valid expressions
  for (i in seq_len(nrow(rules))) {
    expect_true(is.language(rules$rules[[i]]))
  }
})

test_that("active_predictors.party() works with single numeric predictor", {
  skip_if_not_installed("partykit")

  data <- get_single_numeric_data()
  model <- partykit::ctree(y ~ x, data = data)
  active <- active_predictors(model)

  expect_s3_class(active, "tbl_df")
  # If the tree made a split, should have "x" as active predictor
  if (length(unique(unlist(active$active_predictors))) > 0) {
    expect_setequal(unique(unlist(active$active_predictors)), "x")
  }
})

test_that("active_predictors.party() works with single factor predictor", {
  skip_if_not_installed("partykit")

  data <- get_single_factor_data()
  model <- partykit::ctree(y ~ x, data = data)
  active <- active_predictors(model)

  expect_s3_class(active, "tbl_df")
  # If the tree made a split, should have "x" as active predictor
  if (length(unique(unlist(active$active_predictors))) > 0) {
    expect_setequal(unique(unlist(active$active_predictors)), "x")
  }
})

test_that("var_imp.party() works with single numeric predictor", {
  skip_if_not_installed("partykit")

  data <- get_single_numeric_data()
  model <- partykit::ctree(y ~ x, data = data)
  importance <- var_imp(model)

  expect_s3_class(importance, "tbl_df")
  # If the tree made a split, "x" should have importance
  if (nrow(importance) > 0) {
    expect_equal(importance$term, "x")
    expect_true(importance$estimate >= 0)
  }
})

test_that("var_imp.party() works with single factor predictor", {
  skip_if_not_installed("partykit")

  data <- get_single_factor_data()
  model <- partykit::ctree(y ~ x, data = data)
  importance <- var_imp(model)

  expect_s3_class(importance, "tbl_df")
  # If the tree made a split, "x" should have importance
  if (nrow(importance) > 0) {
    expect_equal(importance$term, "x")
    expect_true(importance$estimate >= 0)
  }
})
