test_that("extract_rules.rpart() returns correct structure", {
  skip_if_not_installed("rpart")

  tree <- get_iris_rpart_tree()
  rules <- extract_rules(tree)

  expect_s3_class(rules, "rule_set_rpart")
  expect_s3_class(rules, "rule_set")
  expect_s3_class(rules, "tbl_df")
  expect_named(rules, c("id", "rules"))
  expect_type(rules$id, "integer")
  expect_type(rules$rules, "list")
})

test_that("extract_rules.rpart() extracts all terminal nodes", {
  skip_if_not_installed("rpart")

  tree <- get_iris_rpart_tree()
  rules <- extract_rules(tree)
  terminal_nodes <- as.integer(rownames(tree$frame[
    tree$frame$var == "<leaf>",
  ]))
  expect_equal(sort(rules$id), sort(terminal_nodes))
})

test_that("extract_rules.rpart() produces valid R expressions", {
  skip_if_not_installed("rpart")

  tree <- get_iris_rpart_tree()
  rules <- extract_rules(tree)

  for (i in seq_len(nrow(rules))) {
    expect_true(is.language(rules$rules[[i]]))
  }
})

test_that("extract_rules.rpart() rules evaluate correctly", {
  skip_if_not_installed("rpart")

  tree <- get_iris_rpart_tree()
  rules <- extract_rules(tree)

  for (i in seq_len(nrow(rules))) {
    rule_expr <- rules$rules[[i]]
    result <- eval(rule_expr, iris)

    expect_type(result, "logical")
    expect_equal(length(result), nrow(iris))
    expect_true(sum(result) > 0)
  }
})

test_that("extract_rules.rpart() works with numeric-only splits", {
  skip_if_not_installed("rpart")

  num_tree <- rpart::rpart(Petal.Length ~ Sepal.Length + Sepal.Width, data = iris)
  rules <- extract_rules(num_tree)

  expect_s3_class(rules, "rule_set_rpart")
  expect_true(nrow(rules) > 0)

  for (i in seq_len(nrow(rules))) {
    expect_true(is.language(rules$rules[[i]]))
  }
})

test_that("extract_rules.rpart() works with categorical splits", {
  skip_if_not_installed("rpart")

  cat_tree <- rpart::rpart(Species ~ ., data = iris)
  rules <- extract_rules(cat_tree)

  expect_s3_class(rules, "rule_set_rpart")
  expect_true(nrow(rules) > 0)
})

test_that("extract_rules.rpart() works with single-node tree", {
  skip_if_not_installed("rpart")

  # Create a tree with no splits
  single_tree <- rpart::rpart(Sepal.Length ~ Sepal.Width, data = iris, cp = 1)
  rules <- extract_rules(single_tree)

  expect_s3_class(rules, "rule_set_rpart")
  expect_equal(nrow(rules), 1)
  expect_equal(rules$id, 1L)
  expect_equal(rules$rules[[1]], rlang::expr(TRUE))
})

test_that("extract_rules.rpart() integrates with rule_text()", {
  skip_if_not_installed("rpart")

  tree <- get_iris_rpart_tree()
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

test_that("extract_rules.rpart() works with wa_trees data", {
  skip_if_not_installed("rpart")

  load(system.file(package = "lorax", "wa_trees.RData"))
  wa_tree <- rpart::rpart(class ~ ., data = wa_trees)
  rules <- extract_rules(wa_tree)

  expect_s3_class(rules, "rule_set_rpart")
  expect_true(nrow(rules) > 0)

  # Check that rules are sorted by id
  expect_true(all(diff(rules$id) > 0))

  # Check that all rules evaluate and match expected counts
  for (i in seq_len(nrow(rules))) {
    result <- eval(rules$rules[[i]], wa_trees)
    expected_n <- wa_tree$frame[as.character(rules$id[i]), "n"]

    expect_type(result, "logical")
    expect_equal(length(result), nrow(wa_trees))
    expect_equal(sum(result), expected_n)
  }
})

test_that("extract_rules.rpart() handles no-split data", {
  skip_if_not_installed("rpart")

  # Data where no good split can be made
  null_data <- data.frame(y = 1:10, x = rep(1:5, each = 2))
  tree <- rpart::rpart(y ~ ., data = null_data)

  rules <- extract_rules(tree)

  expect_s3_class(rules, "rule_set_rpart")
  expect_equal(nrow(rules), 1)
  expect_equal(rules$id, 1L)
  expect_equal(rules$rules[[1]], rlang::expr(TRUE))
})

# Tests for active_predictors.rpart() -----------------------------------------

test_that("active_predictors.rpart() returns correct structure", {
  skip_if_not_installed("rpart")

  load(system.file(package = "lorax", "wa_trees.RData"))
  wa_tree <- rpart::rpart(class ~ ., data = wa_trees)
  result <- active_predictors(wa_tree)

  expect_s3_class(result, "tbl_df")
  expect_named(result, "active_predictors")
  expect_type(result$active_predictors, "list")
  expect_length(result$active_predictors, 1)
  expect_type(result$active_predictors[[1]], "character")
})

test_that("active_predictors.rpart() extracts correct variables", {
  skip_if_not_installed("rpart")

  load(system.file(package = "lorax", "wa_trees.RData"))
  wa_tree <- rpart::rpart(class ~ ., data = wa_trees)
  result <- active_predictors(wa_tree)

  # Expected: unique variables from frame excluding "<leaf>"
  expected <- unique(wa_tree$frame$var[wa_tree$frame$var != "<leaf>"])

  expect_equal(sort(result$active_predictors[[1]]), sort(expected))
})

test_that("active_predictors.rpart() excludes competing and surrogate splits", {
  skip_if_not_installed("rpart")

  load(system.file(package = "lorax", "wa_trees.RData"))
  wa_tree <- rpart::rpart(
    class ~ elevation + eastness + northness + roughness + county,
    data = wa_trees
  )
  result <- active_predictors(wa_tree)
  active_vars <- result$active_predictors[[1]]

  # Only variables used in actual splits should be included
  # Not those in competing or surrogate splits
  all_frame_vars <- unique(wa_tree$frame$var[wa_tree$frame$var != "<leaf>"])
  expect_equal(sort(active_vars), sort(all_frame_vars))
})

test_that("active_predictors.rpart() handles single-node tree", {
  skip_if_not_installed("rpart")

  load(system.file(package = "lorax", "wa_trees.RData"))
  single_tree <- rpart::rpart(class ~ elevation, data = wa_trees, cp = 1)
  result <- active_predictors(single_tree)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 1)
  expect_type(result$active_predictors[[1]], "character")
  expect_length(result$active_predictors[[1]], 0)
})

test_that("active_predictors.rpart() handles tree with repeated variables", {
  skip_if_not_installed("rpart")

  load(system.file(package = "lorax", "wa_trees.RData"))
  deep_tree <- rpart::rpart(class ~ ., data = wa_trees, cp = 0.01)
  result <- active_predictors(deep_tree)

  # Should return unique variables only (no duplicates)
  expect_equal(
    length(result$active_predictors[[1]]),
    length(unique(result$active_predictors[[1]]))
  )
})

test_that("active_predictors.rpart() works with numeric predictors", {
  skip_if_not_installed("rpart")

  load(system.file(package = "lorax", "wa_trees.RData"))
  num_tree <- rpart::rpart(elevation ~ year + roughness + dew_temp, data = wa_trees)
  result <- active_predictors(num_tree)

  expect_s3_class(result, "tbl_df")
  expect_type(result$active_predictors[[1]], "character")
  # All predictors should be from the model formula
  expect_true(all(
    result$active_predictors[[1]] %in%
      c("year", "roughness", "dew_temp")
  ))
})

test_that("active_predictors.rpart() works with factor predictors", {
  skip_if_not_installed("rpart")

  load(system.file(package = "lorax", "wa_trees.RData"))
  factor_tree <- rpart::rpart(county ~ class + elevation + roughness, data = wa_trees)
  result <- active_predictors(factor_tree)

  expect_s3_class(result, "tbl_df")
  expect_type(result$active_predictors[[1]], "character")
  expect_true(length(result$active_predictors[[1]]) >= 0)
})
