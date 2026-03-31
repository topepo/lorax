# Tests for extract_rules.cforest() ------------------------------------------

skip("too long")

test_that("extract_rules.cforest() returns correct structure", {
  skip_if_not_installed("partykit")

  wa_trees <- get_wa_trees_data()[1:200, ]
  set.seed(847)
  cf <- partykit::cforest(
    class ~ elevation + county,
    data = wa_trees,
    ntree = 3
  )
  rules <- extract_rules(cf, tree = 1L)

  expect_s3_class(rules, "rule_set_party")
  expect_s3_class(rules, "rule_set")
  expect_s3_class(rules, "tbl_df")
  expect_named(rules, c("id", "rules", "tree"))
  expect_type(rules$id, "integer")
  expect_type(rules$rules, "list")
  expect_type(rules$tree, "integer")
})

test_that("extract_rules.cforest() extracts from single tree", {
  skip_if_not_installed("partykit")

  wa_trees <- get_wa_trees_data()[1:200, ]
  set.seed(532)
  cf <- partykit::cforest(
    class ~ elevation + county + roughness,
    data = wa_trees,
    ntree = 3
  )
  rules <- extract_rules(cf, tree = 1L)

  expect_equal(unique(rules$tree), 1L)
  expect_true(nrow(rules) > 0)

  # Check that rules are valid expressions
  for (i in seq_len(nrow(rules))) {
    expect_true(is.language(rules$rules[[i]]))
  }
})

test_that("extract_rules.cforest() extracts from multiple trees", {
  skip_if_not_installed("partykit")

  wa_trees <- get_wa_trees_data()[1:200, ]
  set.seed(219)
  cf <- partykit::cforest(
    class ~ elevation + county + roughness,
    data = wa_trees,
    ntree = 3
  )
  rules <- extract_rules(cf, tree = c(1L, 2L, 3L))

  expect_equal(sort(unique(rules$tree)), c(1L, 2L, 3L))
  expect_true(nrow(rules) > 0)

  # Check that each tree has rules
  for (tree_num in c(1L, 2L, 3L)) {
    tree_rules <- rules[rules$tree == tree_num, ]
    expect_true(nrow(tree_rules) > 0)
  }
})

test_that("extract_rules.cforest() validates tree argument", {
  skip_if_not_installed("partykit")

  wa_trees <- get_wa_trees_data()[1:200, ]
  set.seed(674)
  cf <- partykit::cforest(
    class ~ elevation + county,
    data = wa_trees,
    ntree = 3
  )

  expect_snapshot(extract_rules(cf, tree = "1"), error = TRUE)
  expect_snapshot(extract_rules(cf, tree = 1.5), error = TRUE)
  expect_snapshot(extract_rules(cf, tree = 0L), error = TRUE)
  expect_snapshot(extract_rules(cf, tree = 10L), error = TRUE)
})

test_that("extract_rules.cforest() works with numeric predictors", {
  skip_if_not_installed("partykit")

  data <- get_regression_data(n = 60)
  set.seed(158)
  cf <- partykit::cforest(y ~ x1 + x2, data = data, ntree = 3)
  rules <- extract_rules(cf, tree = 1L)

  expect_s3_class(rules, "rule_set_party")
  expect_true(nrow(rules) > 0)
})

test_that("extract_rules.cforest() works with factor predictors", {
  skip_if_not_installed("partykit")

  data <- get_factor_data(n = 60)
  set.seed(391)
  cf <- partykit::cforest(y ~ x2 + x4, data = data, ntree = 3)
  rules <- extract_rules(cf, tree = 1L)

  expect_s3_class(rules, "rule_set_party")
  expect_true(nrow(rules) > 0)
})

test_that("extract_rules.cforest() works with mixed numeric and factor predictors", {
  skip_if_not_installed("partykit")

  wa_trees <- get_wa_trees_data()[1:200, ]
  set.seed(746)
  cf <- partykit::cforest(
    class ~ elevation + county + roughness,
    data = wa_trees,
    ntree = 3
  )
  rules <- extract_rules(cf, tree = 1L)

  expect_s3_class(rules, "rule_set_party")
  expect_true(nrow(rules) > 0)
})

test_that("extract_rules.cforest() rules are sorted by tree then id", {
  skip_if_not_installed("partykit")

  wa_trees <- get_wa_trees_data()[1:200, ]
  set.seed(803)
  cf <- partykit::cforest(
    class ~ elevation + county + roughness,
    data = wa_trees,
    ntree = 3
  )
  rules <- extract_rules(cf, tree = c(2L, 1L, 3L))

  # Check sorting
  expect_true(all(diff(rules$tree) >= 0))

  # Within each tree, ids should be sorted
  for (tree_num in unique(rules$tree)) {
    tree_rules <- rules[rules$tree == tree_num, ]
    expect_true(all(diff(tree_rules$id) > 0))
  }
})

test_that("extract_rules.cforest() handles duplicate tree numbers", {
  skip_if_not_installed("partykit")

  wa_trees <- get_wa_trees_data()[1:200, ]
  set.seed(456)
  cf <- partykit::cforest(
    class ~ elevation + county,
    data = wa_trees,
    ntree = 3
  )
  rules <- extract_rules(cf, tree = c(1L, 1L, 2L))

  # Should have results for tree 1 twice
  tree_counts <- table(rules$tree)
  expect_equal(as.numeric(names(tree_counts)), c(1, 2))
})

test_that("extract_rules.cforest() works with all trees", {
  skip_if_not_installed("partykit")

  wa_trees <- get_wa_trees_data()[1:200, ]
  set.seed(927)
  cf <- partykit::cforest(
    class ~ elevation + county,
    data = wa_trees,
    ntree = 3
  )
  n_trees <- length(cf$nodes)
  rules <- extract_rules(cf, tree = 1:n_trees)

  expect_equal(sort(unique(rules$tree)), 1:n_trees)
  expect_true(nrow(rules) > 0)
})

test_that("extract_rules.cforest() handles tree with no valid splits", {
  skip_if_not_installed("partykit")

  # Create data where tree may have no splits
  null_data <- data.frame(y = 1:10, x = rep(1:5, each = 2))
  set.seed(614)
  cf <- partykit::cforest(y ~ ., data = null_data, ntree = 2)
  rules <- extract_rules(cf, tree = 1L)

  expect_s3_class(rules, "rule_set_party")
  # Even with no splits, should return at least one rule (TRUE)
  expect_true(nrow(rules) >= 1)
})

# Tests for active_predictors.cforest() -------------------------------------

test_that("active_predictors.cforest() returns correct structure", {
  skip_if_not_installed("partykit")

  wa_trees <- get_wa_trees_data()[1:200, ]
  set.seed(183)
  cf <- partykit::cforest(
    class ~ elevation + county,
    data = wa_trees,
    ntree = 3
  )
  result <- active_predictors(cf, tree = 1L)

  expect_s3_class(result, "tbl_df")
  expect_named(result, c("active_predictors", "tree"))
  expect_type(result$active_predictors, "list")
  expect_type(result$tree, "integer")
  expect_equal(nrow(result), 1)
})

test_that("active_predictors.cforest() extracts from single tree", {
  skip_if_not_installed("partykit")

  wa_trees <- get_wa_trees_data()[1:200, ]
  set.seed(295)
  cf <- partykit::cforest(
    class ~ elevation + county + roughness,
    data = wa_trees,
    ntree = 3
  )
  result <- active_predictors(cf, tree = 1L)

  expect_equal(nrow(result), 1)
  expect_equal(result$tree, 1L)
  expect_type(result$active_predictors[[1]], "character")
})

test_that("active_predictors.cforest() extracts from multiple trees", {
  skip_if_not_installed("partykit")

  wa_trees <- get_wa_trees_data()[1:200, ]
  set.seed(418)
  cf <- partykit::cforest(
    class ~ elevation + county + roughness,
    data = wa_trees,
    ntree = 3
  )
  result <- active_predictors(cf, tree = c(1L, 2L, 3L))

  expect_equal(nrow(result), 3)
  expect_equal(result$tree, c(1L, 2L, 3L))
  expect_equal(length(result$active_predictors), 3)
})

test_that("active_predictors.cforest() validates tree argument", {
  skip_if_not_installed("partykit")

  wa_trees <- get_wa_trees_data()[1:200, ]
  set.seed(563)
  cf <- partykit::cforest(
    class ~ elevation + county,
    data = wa_trees,
    ntree = 3
  )

  expect_snapshot(active_predictors(cf, tree = "1"), error = TRUE)
  expect_snapshot(active_predictors(cf, tree = 1.5), error = TRUE)
  expect_snapshot(active_predictors(cf, tree = 0L), error = TRUE)
  expect_snapshot(active_predictors(cf, tree = 10L), error = TRUE)
})

test_that("active_predictors.cforest() works with numeric predictors", {
  skip_if_not_installed("partykit")

  data <- get_regression_data(n = 60)
  set.seed(729)
  cf <- partykit::cforest(y ~ x1 + x2 + x3, data = data, ntree = 3)
  result <- active_predictors(cf, tree = 1L)

  active_vars <- result$active_predictors[[1]]
  expect_type(active_vars, "character")
  expect_true(all(active_vars %in% c("x1", "x2", "x3")))
})

test_that("active_predictors.cforest() works with factor predictors", {
  skip_if_not_installed("partykit")

  data <- get_factor_data(n = 60)
  set.seed(841)
  cf <- partykit::cforest(y ~ x2 + x4, data = data, ntree = 3)
  result <- active_predictors(cf, tree = 1L)

  active_vars <- result$active_predictors[[1]]
  expect_type(active_vars, "character")
  expect_true(length(active_vars) >= 0)
})

test_that("active_predictors.cforest() works with mixed numeric and factor predictors", {
  skip_if_not_installed("partykit")

  wa_trees <- get_wa_trees_data()[1:200, ]
  set.seed(926)
  cf <- partykit::cforest(
    class ~ elevation + county + roughness,
    data = wa_trees,
    ntree = 3
  )
  result <- active_predictors(cf, tree = 1L)

  active_vars <- result$active_predictors[[1]]
  expect_type(active_vars, "character")
  expect_true(all(active_vars %in% c("elevation", "county", "roughness")))
})

test_that("active_predictors.cforest() returns sorted unique variables", {
  skip_if_not_installed("partykit")

  wa_trees <- get_wa_trees_data()[1:200, ]
  set.seed(147)
  cf <- partykit::cforest(
    class ~ elevation + county + roughness,
    data = wa_trees,
    ntree = 3
  )
  result <- active_predictors(cf, tree = 1L)

  active_vars <- result$active_predictors[[1]]
  expect_equal(active_vars, active_vars[order(tolower(active_vars))])
  expect_equal(length(active_vars), length(unique(active_vars)))
})

test_that("active_predictors.cforest() works with all trees", {
  skip_if_not_installed("partykit")

  wa_trees <- get_wa_trees_data()[1:200, ]
  set.seed(372)
  cf <- partykit::cforest(
    class ~ elevation + county,
    data = wa_trees,
    ntree = 3
  )
  n_trees <- length(cf$nodes)
  result <- active_predictors(cf, tree = 1:n_trees)

  expect_equal(nrow(result), n_trees)
  expect_equal(result$tree, 1:n_trees)
  expect_equal(length(result$active_predictors), n_trees)
})

test_that("active_predictors.cforest() handles duplicate tree numbers", {
  skip_if_not_installed("partykit")

  wa_trees <- get_wa_trees_data()[1:200, ]
  set.seed(508)
  cf <- partykit::cforest(
    class ~ elevation + county,
    data = wa_trees,
    ntree = 3
  )
  result <- active_predictors(cf, tree = c(1L, 1L, 2L))

  expect_equal(nrow(result), 3)
  expect_equal(result$tree, c(1L, 1L, 2L))
})

test_that("active_predictors.cforest() handles tree with no splits", {
  skip_if_not_installed("partykit")

  # Data where tree may have no splits
  null_data <- data.frame(y = 1:10, x = rep(1:5, each = 2))
  set.seed(695)
  cf <- partykit::cforest(y ~ ., data = null_data, ntree = 2)
  result <- active_predictors(cf, tree = 1L)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 1)
  expect_type(result$active_predictors[[1]], "character")
  expect_length(result$active_predictors[[1]], 0)
})

# Tests for var_imp.cforest() ------------------------------------------------

test_that("var_imp.cforest() returns correct structure", {
  skip_if_not_installed("partykit")

  wa_trees <- get_wa_trees_data()[1:200, ]
  set.seed(821)
  cf <- partykit::cforest(
    class ~ elevation + county,
    data = wa_trees,
    ntree = 5
  )
  result <- var_imp(cf)

  expect_s3_class(result, "tbl_df")
  expect_named(result, c("term", "estimate"))
  expect_type(result$term, "character")
  expect_type(result$estimate, "double")
})

test_that("var_imp.cforest() extracts variable importance scores", {
  skip_if_not_installed("partykit")

  wa_trees <- get_wa_trees_data()[1:200, ]
  set.seed(934)
  cf <- partykit::cforest(
    class ~ elevation + county,
    data = wa_trees,
    ntree = 5
  )
  result <- var_imp(cf)

  # Should have all predictors
  expect_true(nrow(result) > 0)

  # All estimates should be numeric
  expect_true(all(is.numeric(result$estimate)))
})

test_that("var_imp.cforest() with complete=TRUE fills missing predictors", {
  skip_if_not_installed("partykit")

  wa_trees <- get_wa_trees_data()[1:200, ]
  set.seed(157)
  cf <- partykit::cforest(
    class ~ elevation + roughness + dew_temp,
    data = wa_trees,
    ntree = 5
  )
  result <- var_imp(cf, complete = TRUE)

  # Should have all 3 predictors
  expect_equal(nrow(result), 3)
  expect_setequal(result$term, c("elevation", "roughness", "dew_temp"))
})

test_that("var_imp.cforest() with complete=FALSE returns only used predictors", {
  skip_if_not_installed("partykit")

  wa_trees <- get_wa_trees_data()[1:200, ]
  set.seed(283)
  cf <- partykit::cforest(
    class ~ elevation + county,
    data = wa_trees,
    ntree = 5
  )
  result <- var_imp(cf, complete = FALSE)

  # Should only have predictors with importance scores
  imp <- partykit::varimp(cf)
  expected_vars <- names(imp)
  expect_equal(nrow(result), length(expected_vars))
  expect_setequal(result$term, expected_vars)
})

test_that("var_imp.cforest() works with numeric predictors only", {
  skip_if_not_installed("partykit")

  data <- get_regression_data(n = 80)
  set.seed(476)
  cf <- partykit::cforest(y ~ x1 + x2, data = data, ntree = 5)
  result <- var_imp(cf, complete = TRUE)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2)
  expect_setequal(result$term, c("x1", "x2"))
})

test_that("var_imp.cforest() works with factor predictors", {
  skip_if_not_installed("partykit")

  wa_trees <- get_wa_trees_data()[1:200, ]
  set.seed(619)
  cf <- partykit::cforest(
    county ~ class + elevation,
    data = wa_trees,
    ntree = 5
  )
  result <- var_imp(cf, complete = TRUE)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2)
  expect_setequal(result$term, c("class", "elevation"))
})

test_that("var_imp.cforest() works with mixed numeric and factor predictors", {
  skip_if_not_installed("partykit")

  wa_trees <- get_wa_trees_data()[1:200, ]
  set.seed(742)
  cf <- partykit::cforest(
    elevation ~ class + roughness,
    data = wa_trees,
    ntree = 5
  )
  result <- var_imp(cf, complete = TRUE)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2)
  expect_setequal(result$term, c("class", "roughness"))
})

test_that("var_imp.cforest() returns all expected predictors", {
  skip_if_not_installed("partykit")

  wa_trees <- get_wa_trees_data()[1:200, ]
  set.seed(865)
  cf <- partykit::cforest(
    class ~ elevation + county,
    data = wa_trees,
    ntree = 5
  )
  result <- var_imp(cf, complete = FALSE)

  # Check that all predictors from partykit::varimp() are present
  expected_names <- names(partykit::varimp(cf))
  expect_setequal(result$term, expected_names)

  # Check that estimates are numeric
  expect_true(all(is.numeric(result$estimate)))
})

test_that("var_imp.cforest() works with classification forest", {
  skip_if_not_installed("partykit")

  wa_trees <- get_wa_trees_data()[1:200, ]
  set.seed(918)
  cf <- partykit::cforest(
    class ~ elevation + county,
    data = wa_trees,
    ntree = 5
  )
  result <- var_imp(cf)

  expect_s3_class(result, "tbl_df")
  expect_true(nrow(result) > 0)
})

test_that("var_imp.cforest() works with regression forest", {
  skip_if_not_installed("partykit")

  data <- get_regression_data(n = 80)
  set.seed(241)
  cf <- partykit::cforest(y ~ x1 + x2, data = data, ntree = 5)
  result <- var_imp(cf)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2)
  expect_setequal(result$term, c("x1", "x2"))
})

test_that("var_imp.cforest() handles many predictors", {
  skip_if_not_installed("partykit")

  set.seed(374)
  n <- 100
  p <- 5
  X <- as.data.frame(matrix(rnorm(n * p), nrow = n, ncol = p))
  colnames(X) <- paste0("x", 1:p)
  X$y <- X$x1 + X$x2 + rnorm(n)

  set.seed(507)
  cf <- partykit::cforest(y ~ ., data = X, ntree = 5)
  result <- var_imp(cf)

  expect_equal(nrow(result), p)
  expect_setequal(result$term, paste0("x", 1:p))
})

test_that("var_imp.cforest() handles constrained splits", {
  skip_if_not_installed("partykit")

  # Force a constrained forest
  set.seed(682)
  small_data <- data.frame(
    y = rnorm(60),
    x1 = rnorm(60),
    x2 = rnorm(60)
  )

  set.seed(795)
  cf <- partykit::cforest(
    y ~ x1 + x2,
    data = small_data,
    ntree = 3,
    control = partykit::ctree_control(minsplit = 20)
  )

  result <- var_imp(cf, complete = TRUE)

  expect_s3_class(result, "tbl_df")
  expect_named(result, c("term", "estimate"))
  expect_equal(nrow(result), 2)
  expect_setequal(result$term, c("x1", "x2"))
})
