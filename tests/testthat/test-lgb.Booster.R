library(lightgbm)

# Test data setup
data(agaricus.train, package = "lightgbm")
dtrain <- lgb.Dataset(agaricus.train$data, label = agaricus.train$label)
bst <- lgb.train(
  params = list(
    objective = "binary",
    max_depth = 3
  ),
  data = dtrain,
  nrounds = 3
)

test_that("extract_rules.lgb.Booster() returns correct structure", {
  rules <- extract_rules(bst, tree = 1)

  expect_s3_class(rules, "rule_set_lgb.Booster")
  expect_s3_class(rules, "rule_set")
  expect_s3_class(rules, "tbl_df")
  expect_named(rules, c("tree", "rules", "id"))
  expect_type(rules$tree, "integer")
  expect_type(rules$rules, "list")
  expect_type(rules$id, "integer")
})

test_that("extract_rules.lgb.Booster() validates tree parameter", {
  expect_snapshot(extract_rules(bst, tree = 1.5), error = TRUE)
  expect_snapshot(extract_rules(bst, tree = c(1, 2)), error = TRUE)
  expect_snapshot(extract_rules(bst, tree = 100), error = TRUE)
  expect_snapshot(extract_rules(bst, tree = 0), error = TRUE)
})

test_that("extract_rules.lgb.Booster() extracts all terminal nodes", {
  rules <- extract_rules(bst, tree = 1)

  # Get terminals directly from lightgbm
  tree_dt <- as.data.frame(lightgbm::lgb.model.dt.tree(bst))
  tree_dt_filtered <- tree_dt[tree_dt$tree_index == 0, ]
  expected_terminals <- tree_dt_filtered$leaf_index[
    !is.na(tree_dt_filtered$leaf_index)
  ] +
    1L

  expect_equal(sort(rules$id), sort(expected_terminals))
})

test_that("extract_rules.lgb.Booster() produces valid R expressions", {
  rules <- extract_rules(bst, tree = 1)

  for (i in seq_len(nrow(rules))) {
    expect_true(is.language(rules$rules[[i]]))
  }
})

test_that("extract_rules.lgb.Booster() rules evaluate correctly", {
  rules <- extract_rules(bst, tree = 1)

  # Convert sparse matrix to data frame for evaluation
  train_df <- as.data.frame(as.matrix(agaricus.train$data))

  for (i in seq_len(nrow(rules))) {
    rule_expr <- rules$rules[[i]]
    result <- eval(rule_expr, train_df)

    expect_type(result, "logical")
    expect_equal(length(result), nrow(train_df))
  }

  # At least one rule should match some observations
  total_matches <- sum(sapply(rules$rules, function(r) sum(eval(r, train_df))))
  expect_true(total_matches > 0)
})

test_that("extract_rules.lgb.Booster() works with shallow tree", {
  # Create shallow tree
  bst_shallow <- lgb.train(
    params = list(
      objective = "binary",
      max_depth = 1
    ),
    data = dtrain,
    nrounds = 1
  )

  rules <- extract_rules(bst_shallow, tree = 1)

  expect_s3_class(rules, "rule_set_lgb.Booster")
  expect_true(nrow(rules) >= 2) # At least 2 leaves from one split
  expect_true(all(sapply(rules$rules, is.language)))
})

test_that("extract_rules.lgb.Booster() rules are sorted by id", {
  rules <- extract_rules(bst, tree = 1)
  expect_true(all(diff(rules$id) > 0))
})

test_that("extract_rules.lgb.Booster() works with different trees", {
  rules1 <- extract_rules(bst, tree = 1)
  rules2 <- extract_rules(bst, tree = 2)
  rules3 <- extract_rules(bst, tree = 3)

  expect_s3_class(rules1, "rule_set_lgb.Booster")
  expect_s3_class(rules2, "rule_set_lgb.Booster")
  expect_s3_class(rules3, "rule_set_lgb.Booster")

  # All tree columns should match their input
  expect_true(all(rules1$tree == 1))
  expect_true(all(rules2$tree == 2))
  expect_true(all(rules3$tree == 3))
})

test_that("extract_rules.lgb.Booster() integrates with rule_text()", {
  rules <- extract_rules(bst, tree = 1)

  for (i in seq_len(nrow(rules))) {
    text <- rule_text(rules$rules[[i]])
    expect_type(text, "character")
    expect_true(nchar(text) > 0)

    text_bullets <- rule_text(rules$rules[[i]], bullets = TRUE)
    expect_type(text_bullets, "character")
    expect_true(nchar(text_bullets) > 0)
  }
})

test_that("extract_rules.lgb.Booster() tree column matches input", {
  rules1 <- extract_rules(bst, tree = 1)
  rules2 <- extract_rules(bst, tree = 2)

  expect_true(all(rules1$tree == 1))
  expect_true(all(rules2$tree == 2))
})
