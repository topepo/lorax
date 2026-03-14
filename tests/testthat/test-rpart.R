library(rpart)

# Test data setup
tree <- rpart(Species ~ ., data = iris)

test_that("extract_rules.rpart() returns correct structure", {
  rules <- extract_rules(tree)

  expect_s3_class(rules, "rule_set_rpart")
  expect_s3_class(rules, "rule_set")
  expect_s3_class(rules, "tbl_df")
  expect_named(rules, c("id", "rules"))
  expect_type(rules$id, "integer")
  expect_type(rules$rules, "list")
})

test_that("extract_rules.rpart() extracts all terminal nodes", {
  rules <- extract_rules(tree)
  terminal_nodes <- as.integer(rownames(tree$frame[
    tree$frame$var == "<leaf>",
  ]))
  expect_equal(sort(rules$id), sort(terminal_nodes))
})

test_that("extract_rules.rpart() produces valid R expressions", {
  rules <- extract_rules(tree)

  for (i in seq_len(nrow(rules))) {
    expect_true(is.language(rules$rules[[i]]))
  }
})

test_that("extract_rules.rpart() rules evaluate correctly", {
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
  num_tree <- rpart(Petal.Length ~ Sepal.Length + Sepal.Width, data = iris)
  rules <- extract_rules(num_tree)

  expect_s3_class(rules, "rule_set_rpart")
  expect_true(nrow(rules) > 0)

  for (i in seq_len(nrow(rules))) {
    expect_true(is.language(rules$rules[[i]]))
  }
})

test_that("extract_rules.rpart() works with categorical splits", {
  cat_tree <- rpart(Species ~ ., data = iris)
  rules <- extract_rules(cat_tree)

  expect_s3_class(rules, "rule_set_rpart")
  expect_true(nrow(rules) > 0)
})

test_that("extract_rules.rpart() works with single-node tree", {
  # Create a tree with no splits
  single_tree <- rpart(Sepal.Length ~ Sepal.Width, data = iris, cp = 1)
  rules <- extract_rules(single_tree)

  expect_s3_class(rules, "rule_set_rpart")
  expect_equal(nrow(rules), 1)
  expect_equal(rules$id, 1L)
  expect_equal(rules$rules[[1]], rlang::expr(TRUE))
})

test_that("extract_rules.rpart() integrates with rule_text()", {
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
  load(system.file(package = "lorax", "wa_trees.RData"))
  wa_tree <- rpart(class ~ ., data = wa_trees)
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

test_that("extract_rules.rpart() handles single-node tree (already exists)", {
  # This test already exists above, just confirming it works
  single_tree <- rpart(Sepal.Length ~ Sepal.Width, data = iris, cp = 1)
  rules <- extract_rules(single_tree)

  expect_s3_class(rules, "rule_set_rpart")
  expect_equal(nrow(rules), 1)
  expect_equal(rules$id, 1L)
  expect_equal(rules$rules[[1]], rlang::expr(TRUE))
})

test_that("extract_rules.rpart() handles no-split data", {
  # Data where no good split can be made
  null_data <- data.frame(y = 1:10, x = rep(1:5, each = 2))
  tree <- rpart(y ~ ., data = null_data)

  rules <- extract_rules(tree)

  expect_s3_class(rules, "rule_set_rpart")
  expect_equal(nrow(rules), 1)
  expect_equal(rules$id, 1L)
  expect_equal(rules$rules[[1]], rlang::expr(TRUE))
})

test_that("extract_rules.rpart() handles no-split data", {
  # Data where no good split can be made
  null_data <- data.frame(y = 1:10, x = rep(1:5, each = 2))
  tree <- rpart(y ~ ., data = null_data)

  rules <- extract_rules(tree)

  expect_s3_class(rules, "rule_set_rpart")
  expect_equal(nrow(rules), 1)
  expect_equal(rules$id, 1L)
  expect_equal(rules$rules[[1]], rlang::expr(TRUE))
})
