library(xgboost)

# Test data setup
data(agaricus.train, package = "xgboost")
dtrain <- xgb.DMatrix(agaricus.train$data, label = agaricus.train$label)
bst <- xgb.train(
  data = dtrain,
  params = list(
    max_depth = 3,
    objective = "binary:logistic"
  ),
  nrounds = 3
)

test_that("extract_rules.xgb.Booster() returns correct structure", {
  rules <- extract_rules(bst, tree = 1)

  expect_s3_class(rules, "rule_set_xgb.Booster")
  expect_s3_class(rules, "rule_set")
  expect_s3_class(rules, "tbl_df")
  expect_named(rules, c("tree", "rules", "id"))
  expect_type(rules$tree, "integer")
  expect_type(rules$rules, "list")
  expect_type(rules$id, "integer")
})

test_that("extract_rules.xgb.Booster() validates tree parameter", {
  expect_snapshot(extract_rules(bst, tree = 1.5), error = TRUE)
  expect_snapshot(extract_rules(bst, tree = c(1, 2)), error = TRUE)
  expect_snapshot(extract_rules(bst, tree = 100), error = TRUE)
  expect_snapshot(extract_rules(bst, tree = 0), error = TRUE)
})

test_that("extract_rules.xgb.Booster() extracts all terminal nodes", {
  rules <- extract_rules(bst, tree = 1)

  # Get terminals directly from xgboost
  tree_dt <- as.data.frame(xgboost::xgb.model.dt.tree(
    bst,
    trees = 1,
    use_int_id = TRUE
  ))
  expected_terminals <- tree_dt$Node[tree_dt$Feature == "Leaf"] + 1L

  expect_equal(sort(rules$id), sort(expected_terminals))
})

test_that("extract_rules.xgb.Booster() produces valid R expressions", {
  rules <- extract_rules(bst, tree = 1)

  for (i in seq_len(nrow(rules))) {
    expect_true(is.language(rules$rules[[i]]))
  }
})

test_that("extract_rules.xgb.Booster() rules evaluate correctly", {
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

test_that("extract_rules.xgb.Booster() works with shallow tree", {
  # Create shallow tree
  bst_shallow <- xgb.train(
    data = dtrain,
    params = list(
      max_depth = 1,
      objective = "binary:logistic"
    ),
    nrounds = 1
  )

  rules <- extract_rules(bst_shallow, tree = 1)

  expect_s3_class(rules, "rule_set_xgb.Booster")
  expect_true(nrow(rules) >= 2) # At least 2 leaves from one split
  expect_true(all(sapply(rules$rules, is.language)))
})

test_that("extract_rules.xgb.Booster() rules are sorted by id", {
  rules <- extract_rules(bst, tree = 1)
  expect_true(all(diff(rules$id) > 0))
})

test_that("extract_rules.xgb.Booster() works with different trees", {
  rules1 <- extract_rules(bst, tree = 1)
  rules2 <- extract_rules(bst, tree = 2)

  expect_s3_class(rules1, "rule_set_xgb.Booster")
  expect_s3_class(rules2, "rule_set_xgb.Booster")

  # Trees should generally be different
  expect_false(identical(rules1$rules, rules2$rules))
})

test_that("extract_rules.xgb.Booster() integrates with rule_text()", {
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

test_that("extract_rules.xgb.Booster() tree column matches input", {
  rules1 <- extract_rules(bst, tree = 1)
  rules2 <- extract_rules(bst, tree = 2)

  expect_true(all(rules1$tree == 1))
  expect_true(all(rules2$tree == 2))
})
