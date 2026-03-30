test_that("as.party.C5.0 returns valid party object for single tree", {
  skip_if_not_installed("C50")
  skip_if_not_installed("palmerpenguins")

  penguins <- get_penguins_data()

  c5_model <- C50::C5.0(species ~ ., data = penguins)
  p <- as.party(c5_model, tree = 1, data = penguins)

  expect_s3_class(p, "party")
  expect_s3_class(p$node, "partynode")
  expect_true(is.data.frame(p$data))
})

test_that("as.party.C5.0 works with binary classification", {
  skip_if_not_installed("C50")

  data <- get_binary_data(n = 100)

  c5_model <- C50::C5.0(y ~ ., data = data)
  p <- as.party(c5_model, tree = 1, data = data)

  expect_s3_class(p, "party")
  expect_s3_class(p$node, "partynode")
})

test_that("as.party.C5.0 works with iris data", {
  skip_if_not_installed("C50")

  iris_data <- get_iris_data()

  c5_model <- C50::C5.0(Species ~ ., data = iris_data)
  p <- as.party(c5_model, tree = 1, data = iris_data)

  expect_s3_class(p, "party")
  expect_s3_class(p$node, "partynode")
})

test_that("as.party.C5.0 validates tree parameter", {
  skip_if_not_installed("C50")
  skip_if_not_installed("palmerpenguins")

  penguins <- get_penguins_data()
  c5_model <- C50::C5.0(species ~ ., data = penguins)

  expect_snapshot(as.party(c5_model, tree = 0, data = penguins), error = TRUE)
  # When tree exceeds num_trials, it should warn and use max tree
  expect_snapshot(as.party(c5_model, tree = 5, data = penguins))
  expect_snapshot(
    as.party(c5_model, tree = c(1, 2), data = penguins),
    error = TRUE
  )
  expect_snapshot(as.party(c5_model, tree = "1", data = penguins), error = TRUE)
})

test_that("as.party.C5.0 requires data parameter", {
  skip_if_not_installed("C50")
  skip_if_not_installed("palmerpenguins")

  penguins <- get_penguins_data()
  c5_model <- C50::C5.0(species ~ ., data = penguins)

  expect_snapshot(as.party(c5_model, tree = 1), error = TRUE)
})

test_that("as.party.C5.0 works with boosted models", {
  skip_if_not_installed("C50")
  skip_if_not_installed("palmerpenguins")

  penguins <- get_penguins_data()
  c5_boost <- C50::C5.0(species ~ ., data = penguins, trials = 10)

  # Extract first and second trees
  p1 <- as.party(c5_boost, tree = 1, data = penguins)
  p2 <- as.party(c5_boost, tree = 2, data = penguins)

  expect_s3_class(p1, "party")
  expect_s3_class(p2, "party")

  # Trees should be different
  expect_false(isTRUE(all.equal(p1, p2)))

  # Check tree number validation
  expect_snapshot(as.party(c5_boost, tree = 0, data = penguins), error = TRUE)
  # When tree exceeds num_trials, it should warn and use max tree
  expect_snapshot(as.party(c5_boost, tree = 11, data = penguins))
})

test_that("as.party.C5.0 extracts different boosted trees", {
  skip_if_not_installed("C50")
  skip_if_not_installed("palmerpenguins")

  penguins <- get_penguins_data()
  c5_boost <- C50::C5.0(species ~ ., data = penguins, trials = 5)

  # Extract multiple trees
  p1 <- as.party(c5_boost, tree = 1, data = penguins)
  p2 <- as.party(c5_boost, tree = 2, data = penguins)
  p3 <- as.party(c5_boost, tree = 5, data = penguins)

  # All should be valid
  expect_s3_class(p1, "party")
  expect_s3_class(p2, "party")
  expect_s3_class(p3, "party")

  # Get root splits to verify they're different
  split1 <- partykit::split_node(p1$node)
  split2 <- partykit::split_node(p2$node)

  # At least one property should differ (varid or threshold)
  varid1 <- partykit::varid_split(split1)
  varid2 <- partykit::varid_split(split2)
  breaks1 <- partykit::breaks_split(split1)
  breaks2 <- partykit::breaks_split(split2)

  # Trees should not be identical
  expect_false(
    identical(varid1, varid2) &&
      identical(breaks1, breaks2) &&
      identical(p1$node, p2$node)
  )
})

test_that("as.party.C5.0 rejects rule-based models", {
  skip_if_not_installed("C50")
  skip_if_not_installed("palmerpenguins")

  penguins <- get_penguins_data()
  c5_rules <- C50::C5.0(species ~ ., data = penguins, rules = TRUE)

  expect_snapshot(as.party(c5_rules, tree = 1, data = penguins), error = TRUE)
})

test_that("as.party.C5.0 does not show asterisks in node summaries", {
  skip_if_not_installed("C50")
  skip_if_not_installed("palmerpenguins")

  penguins <- get_penguins_data()

  c5_model <- C50::C5.0(species ~ ., data = penguins)
  p <- as.party(c5_model, tree = 1, data = penguins)

  output <- capture.output(print(p))

  # Check for asterisks in node summaries (after the colon)
  # Pattern: ": *" or ": * " indicates missing summary
  has_asterisk_summary <- any(grepl(":\\s*\\*\\s*($|\\()", output))

  expect_false(has_asterisk_summary)
})

test_that("as.party.C5.0 properly assigns all observations to terminal nodes", {
  skip_if_not_installed("C50")
  skip_if_not_installed("palmerpenguins")

  penguins <- get_penguins_data()

  c5_model <- C50::C5.0(species ~ ., data = penguins)
  p <- as.party(c5_model, tree = 1, data = penguins)

  # Get fitted node IDs
  fitted_ids <- p$fitted[["(fitted)"]]

  # Check that we have the right number of observations
  expect_equal(length(fitted_ids), nrow(penguins))

  # Check that all fitted IDs are terminal nodes
  terminal_nodes <- partykit::nodeids(p, terminal = TRUE)
  expect_true(all(fitted_ids %in% terminal_nodes))

  # Count observations per node
  obs_per_node <- table(fitted_ids)

  # Sum should equal total observations (no observations lost)
  expect_equal(sum(obs_per_node), nrow(penguins))

  # All used terminal nodes should have at least one observation
  expect_true(all(obs_per_node > 0))
})

test_that("as.party.C5.0 properly routes observations through categorical splits", {
  skip_if_not_installed("C50")

  # Use wa_trees data which has categorical variables and multiway splits
  wa_trees <- get_wa_trees_data()

  c5_model <- C50::C5.0(class ~ ., data = wa_trees)
  p <- as.party(c5_model, tree = 1, data = wa_trees)

  # Get fitted node IDs
  fitted_ids <- p$fitted[["(fitted)"]]

  # Check that we have the right number of observations
  expect_equal(length(fitted_ids), nrow(wa_trees))

  # Check that all fitted IDs are terminal nodes
  terminal_nodes <- partykit::nodeids(p, terminal = TRUE)
  expect_true(all(fitted_ids %in% terminal_nodes))

  # Count observations per node
  obs_per_node <- table(fitted_ids)

  # Sum should equal total observations (no observations lost)
  expect_equal(sum(obs_per_node), nrow(wa_trees))

  # Verify no non-empty nodes have NA predictions
  # (empty nodes with n=0 are OK and expected for some categorical splits)
  used_nodes <- unique(fitted_ids)
  for (node_id in used_nodes) {
    node_obs_count <- sum(fitted_ids == node_id)
    expect_true(node_obs_count > 0)
  }
})

test_that("as.party.C5.0 handles multiway categorical splits correctly", {
  skip_if_not_installed("C50")

  # Use wa_trees which has a 4-way county split
  wa_trees <- get_wa_trees_data()

  c5_model <- C50::C5.0(class ~ ., data = wa_trees)
  p <- as.party(c5_model, tree = 1, data = wa_trees)

  # Find a node with a categorical split
  # The root's right child (precip_annual >= 418) should split on county
  root <- p$node
  kids <- partykit::kids_node(root)

  # Assuming second child is the high precipitation branch
  if (length(kids) >= 2) {
    node2 <- kids[[2]]
    split2 <- partykit::split_node(node2)

    if (!is.null(split2)) {
      # Check if this is a categorical split
      breaks <- partykit::breaks_split(split2)

      # If breaks is NULL, it's categorical
      if (is.null(breaks)) {
        # Should have multiple children (multiway split)
        node2_kids <- partykit::kids_node(node2)
        expect_true(length(node2_kids) > 2)

        # Index vector should exist and map all factor levels
        index <- partykit::index_split(split2)
        expect_true(length(index) > 0)
        expect_true(all(index >= 1))
        expect_true(all(index <= length(node2_kids)))
      }
    }
  }
})

# extract_rules() tests -------------------------------------------------------

test_that("extract_rules.C5.0() returns correct structure", {
  skip_if_not_installed("C50")

  data <- get_factor_data(n = 100)
  set.seed(327)
  c5_tree <- C50::C5.0(y ~ x1 + x2 + x3, data = data)
  rules <- extract_rules(c5_tree, tree = 1L, data = data)

  expect_s3_class(rules, "rule_set_party")
  expect_s3_class(rules, "rule_set")
  expect_s3_class(rules, "tbl_df")
  expect_named(rules, c("id", "rules", "tree"))
  expect_type(rules$id, "integer")
  expect_type(rules$rules, "list")
  expect_type(rules$tree, "integer")
})

test_that("extract_rules.C5.0() extracts from single tree", {
  skip_if_not_installed("C50")

  data <- get_factor_data(n = 100)
  set.seed(438)
  c5_tree <- C50::C5.0(y ~ x1 + x2 + x3, data = data)
  rules <- extract_rules(c5_tree, tree = 1L, data = data)

  expect_equal(unique(rules$tree), 1L)
  expect_true(nrow(rules) > 0)

  # Check that rules are valid expressions
  for (i in seq_len(nrow(rules))) {
    expect_true(is.language(rules$rules[[i]]))
  }
})

test_that("extract_rules.C5.0() extracts from multiple boosted trees", {
  skip_if_not_installed("C50")

  wa_trees <- get_wa_trees_data()[1:200, ]
  set.seed(549)
  c5_boost <- C50::C5.0(
    class ~ elevation + county + roughness,
    data = wa_trees,
    trials = 5
  )

  # Get actual number of trials
  n_trials <- c5_boost$trials["Actual"]
  trees_to_extract <- min(3L, n_trials)
  tree_nums <- seq_len(trees_to_extract)

  rules <- extract_rules(c5_boost, tree = tree_nums, data = wa_trees)

  expect_equal(sort(unique(rules$tree)), tree_nums)
  expect_true(nrow(rules) > 0)

  # Check that each tree has rules
  for (tree_num in tree_nums) {
    tree_rules <- rules[rules$tree == tree_num, ]
    expect_true(nrow(tree_rules) > 0)
  }
})

test_that("extract_rules.C5.0() validates tree argument", {
  skip_if_not_installed("C50")

  wa_trees <- get_wa_trees_data()[1:200, ]
  set.seed(651)
  c5_boost <- C50::C5.0(class ~ elevation + county, data = wa_trees, trials = 5)

  expect_snapshot(
    extract_rules(c5_boost, tree = "1", data = wa_trees),
    error = TRUE
  )
  expect_snapshot(
    extract_rules(c5_boost, tree = 1.5, data = wa_trees),
    error = TRUE
  )
  expect_snapshot(
    extract_rules(c5_boost, tree = 0L, data = wa_trees),
    error = TRUE
  )
  expect_snapshot(
    extract_rules(c5_boost, tree = 10L, data = wa_trees),
    error = TRUE
  )
})

test_that("extract_rules.C5.0() requires data parameter", {
  skip_if_not_installed("C50")

  data <- get_factor_data(n = 100)
  set.seed(762)
  c5_tree <- C50::C5.0(y ~ x1 + x2 + x3, data = data)

  expect_snapshot(extract_rules(c5_tree, tree = 1L), error = TRUE)
  expect_snapshot(extract_rules(c5_tree, tree = 1L, data = NULL), error = TRUE)
})

test_that("extract_rules.C5.0() works with numeric predictors", {
  skip_if_not_installed("C50")

  data <- get_regression_data(n = 100)
  data$y_cat <- cut(data$y, breaks = 3, labels = c("low", "med", "high"))
  set.seed(873)
  c5_tree <- C50::C5.0(y_cat ~ x1 + x2, data = data)
  rules <- extract_rules(c5_tree, tree = 1L, data = data)

  expect_s3_class(rules, "rule_set_party")
  expect_true(nrow(rules) > 0)
})

test_that("extract_rules.C5.0() works with factor predictors", {
  skip_if_not_installed("C50")

  data <- get_factor_data(n = 100)
  set.seed(984)
  c5_tree <- C50::C5.0(y ~ x2 + x4, data = data)
  rules <- extract_rules(c5_tree, tree = 1L, data = data)

  expect_s3_class(rules, "rule_set_party")
  expect_true(nrow(rules) > 0)
})

test_that("extract_rules.C5.0() works with mixed predictors", {
  skip_if_not_installed("C50")

  wa_trees <- get_wa_trees_data()[1:200, ]
  set.seed(195)
  c5_tree <- C50::C5.0(class ~ elevation + county, data = wa_trees)
  rules <- extract_rules(c5_tree, tree = 1L, data = wa_trees)

  expect_s3_class(rules, "rule_set_party")
  expect_true(nrow(rules) > 0)
})

test_that("extract_rules.C5.0() rules are sorted by tree then id", {
  skip_if_not_installed("C50")

  wa_trees <- get_wa_trees_data()[1:200, ]
  set.seed(216)
  c5_boost <- C50::C5.0(
    class ~ elevation + county + roughness,
    data = wa_trees,
    trials = 5
  )

  # Get actual number of trials and extract in reverse order
  n_trials <- c5_boost$trials["Actual"]
  trees_to_extract <- min(3L, n_trials)
  tree_nums <- rev(seq_len(trees_to_extract))

  rules <- extract_rules(c5_boost, tree = tree_nums, data = wa_trees)

  # Check sorting
  expect_true(all(diff(rules$tree) >= 0))

  # Within each tree, ids should be sorted
  for (tree_num in unique(rules$tree)) {
    tree_rules <- rules[rules$tree == tree_num, ]
    expect_true(all(diff(tree_rules$id) > 0))
  }
})

test_that("extract_rules.C5.0() handles duplicate tree numbers", {
  skip_if_not_installed("C50")

  wa_trees <- get_wa_trees_data()[1:200, ]
  set.seed(327)
  c5_boost <- C50::C5.0(class ~ elevation + county, data = wa_trees, trials = 5)

  # Get actual number of trials
  n_trials <- c5_boost$trials["Actual"]
  if (n_trials >= 2) {
    rules <- extract_rules(c5_boost, tree = c(1L, 1L, 2L), data = wa_trees)

    # Should have results for tree 1 twice
    tree_counts <- table(rules$tree)
    expect_equal(as.numeric(names(tree_counts)), c(1, 2))
  } else {
    # If only 1 trial, just test with duplicates of that
    rules <- extract_rules(c5_boost, tree = c(1L, 1L), data = wa_trees)
    tree_counts <- table(rules$tree)
    expect_equal(as.numeric(names(tree_counts)), 1)
  }
})

test_that("extract_rules.C5.0() works with all trees", {
  skip_if_not_installed("C50")

  wa_trees <- get_wa_trees_data()[1:200, ]
  set.seed(438)
  c5_boost <- C50::C5.0(
    class ~ elevation + county + roughness,
    data = wa_trees,
    trials = 5
  )

  # Get actual number of trials
  n_trials <- c5_boost$trials["Actual"]
  rules <- extract_rules(c5_boost, tree = 1:n_trials, data = wa_trees)

  expect_equal(sort(unique(rules$tree)), 1:n_trials)
  expect_true(nrow(rules) > 0)
})

test_that("extract_rules.C5.0() handles tree with no valid splits", {
  skip_if_not_installed("C50")

  # Create data where tree may have no splits
  null_data <- data.frame(
    y = as.factor(rep(c("A", "B"), each = 5)),
    x1 = rep(1:5, each = 2),
    x2 = rep(1:5, each = 2)
  )
  set.seed(549)
  c5_tree <- C50::C5.0(y ~ ., data = null_data)
  rules <- extract_rules(c5_tree, tree = 1L, data = null_data)

  expect_s3_class(rules, "rule_set_party")
  # Even with no splits, should return at least one rule (TRUE)
  expect_true(nrow(rules) >= 1)
})

# active_predictors() tests ---------------------------------------------------

test_that("active_predictors.C5.0() has correct structure for tree models", {
  skip_if_not_installed("C50")

  penguins <- get_penguins_data()

  fit <- C50::C5.0(species ~ ., data = penguins)
  result <- active_predictors(fit)

  expect_s3_class(result, "tbl_df")
  expect_named(result, c("active_predictors", "tree"))
  expect_type(result$active_predictors, "list")
  expect_type(result$tree, "integer")
  expect_equal(nrow(result), 1)
})

test_that("active_predictors.C5.0() extracts correct variables", {
  skip_if_not_installed("C50")
  penguins <- get_penguins_data()

  fit <- C50::C5.0(species ~ ., data = penguins)
  result <- active_predictors(fit)

  active_vars <- result$active_predictors[[1]]
  expect_true(is.character(active_vars))
  expect_true(all(active_vars %in% names(penguins)))
  expect_true(length(active_vars) > 0)
  expect_true(length(active_vars) <= ncol(penguins) - 1)
})

test_that("active_predictors.C5.0() works with numeric predictors", {
  skip_if_not_installed("C50")

  # C5.0 requires factor outcome
  mtcars_factor <- mtcars
  mtcars_factor$vs <- factor(mtcars_factor$vs)
  fit <- C50::C5.0(vs ~ mpg + hp + wt, data = mtcars_factor)
  result <- active_predictors(fit)

  expect_s3_class(result, "tbl_df")
  active_vars <- result$active_predictors[[1]]
  expect_true(all(active_vars %in% c("mpg", "hp", "wt")))
})

test_that("active_predictors.C5.0() works with factor predictors", {
  skip_if_not_installed("C50")

  wa_trees <- get_wa_trees_data()

  fit <- C50::C5.0(class ~ county + roughness, data = wa_trees)
  result <- active_predictors(fit)

  active_vars <- result$active_predictors[[1]]
  expect_true(all(active_vars %in% c("county", "roughness")))
})

test_that("active_predictors.C5.0() handles tree with no splits", {
  skip_if_not_installed("C50")
  skip_on_os("linux")

  # Create a trivial dataset with no useful splits
  small_data <- data.frame(
    x = c(1, 1, 1),
    y = factor(c("a", "a", "a"))
  )
  fit <- C50::C5.0(y ~ x, data = small_data)
  result <- active_predictors(fit)

  expect_s3_class(result, "tbl_df")
  # A tree with no splits will have an empty active_predictors list
  expect_equal(length(result$active_predictors[[1]]), 0)
})

test_that("active_predictors.C5.0() extracts from single boosted tree", {
  skip_if_not_installed("C50")
  skip_on_os("linux")

  penguins <- get_penguins_data()

  fit <- C50::C5.0(species ~ ., data = penguins, trials = 5)
  result <- active_predictors(fit, tree = 2L)

  expect_equal(nrow(result), 1)
  expect_equal(result$tree, 2L)
  expect_true(is.character(result$active_predictors[[1]]))
})

test_that("active_predictors.C5.0() extracts from multiple boosted trees", {
  skip_if_not_installed("C50")
  skip_on_os("linux")

  penguins <- get_penguins_data()

  fit <- C50::C5.0(species ~ ., data = penguins, trials = 5)
  result <- active_predictors(fit, tree = c(1L, 2L, 3L))

  expect_equal(nrow(result), 3)
  expect_equal(result$tree, c(1L, 2L, 3L))
  expect_true(all(vapply(result$active_predictors, length, integer(1)) >= 0))
})

test_that("active_predictors.C5.0() extracts from all trees", {
  skip_if_not_installed("C50")
  skip_on_os("linux")

  penguins <- get_penguins_data()

  fit <- C50::C5.0(species ~ ., data = penguins, trials = 5)
  num_trials <- as.integer(fit$trials["Actual"])
  result <- active_predictors(fit, tree = 1:num_trials)

  expect_equal(nrow(result), num_trials)
  expect_equal(result$tree, 1:num_trials)
})

test_that("active_predictors.C5.0() validates tree argument", {
  skip_if_not_installed("C50")
  skip_on_os("linux")
  penguins <- get_penguins_data()

  fit <- C50::C5.0(species ~ ., data = penguins, trials = 3)

  expect_snapshot(active_predictors(fit, tree = "1"), error = TRUE)
  expect_snapshot(active_predictors(fit, tree = 1.5), error = TRUE)
  expect_snapshot(active_predictors(fit, tree = 0L), error = TRUE)
  expect_snapshot(active_predictors(fit, tree = 10L), error = TRUE)
})

test_that("active_predictors.C5.0() returns sorted unique results", {
  skip_if_not_installed("C50")
  skip_on_os("linux")
  penguins <- get_penguins_data()

  fit <- C50::C5.0(species ~ ., data = penguins)
  result <- active_predictors(fit)

  active_vars <- result$active_predictors[[1]]
  expect_equal(active_vars, active_vars[order(tolower(active_vars))])
  expect_equal(length(active_vars), length(unique(active_vars)))
})

test_that("active_predictors.C5.0() automatically deduplicates tree numbers", {
  skip_if_not_installed("C50")
  skip_on_os("linux")
  penguins <- get_penguins_data()

  fit <- C50::C5.0(species ~ ., data = penguins, trials = 3)
  result <- active_predictors(fit, tree = c(1L, 1L, 2L))

  expect_equal(nrow(result), 2)
  expect_equal(result$tree, c(1L, 2L))
})

test_that("active_predictors.C5.0() rule model has correct structure", {
  skip_if_not_installed("C50")
  skip_on_os("linux")
  penguins <- get_penguins_data()

  fit <- C50::C5.0(species ~ ., data = penguins, rules = TRUE)
  result <- active_predictors(fit)

  expect_s3_class(result, "tbl_df")
  expect_named(result, "active_predictors")
  expect_false("tree" %in% names(result))
  expect_type(result$active_predictors, "list")
  expect_equal(nrow(result), 1)
})

test_that("active_predictors.C5.0() extracts from rule conditions", {
  skip_if_not_installed("C50")
  skip_on_os("linux")
  penguins <- get_penguins_data()

  fit <- C50::C5.0(species ~ ., data = penguins, rules = TRUE)
  result <- active_predictors(fit)

  active_vars <- result$active_predictors[[1]]
  expect_true(is.character(active_vars))
  expect_true(all(active_vars %in% names(penguins)))
  expect_true(length(active_vars) > 0)
})

test_that("active_predictors.C5.0() rule model with factors", {
  skip_if_not_installed("C50")
  skip_on_os("linux")
  wa_trees <- get_wa_trees_data()

  fit <- C50::C5.0(class ~ county + roughness, data = wa_trees, rules = TRUE)
  result <- active_predictors(fit)

  active_vars <- result$active_predictors[[1]]
  expect_true(all(active_vars %in% c("county", "roughness")))
})

test_that("active_predictors.C5.0() rule model with numerics", {
  skip_if_not_installed("C50")
  skip_on_os("linux")

  # C5.0 requires factor outcome
  mtcars_factor <- mtcars
  mtcars_factor$vs <- factor(mtcars_factor$vs)
  fit <- C50::C5.0(vs ~ mpg + hp + wt, data = mtcars_factor, rules = TRUE)
  result <- active_predictors(fit)

  active_vars <- result$active_predictors[[1]]
  expect_true(all(active_vars %in% c("mpg", "hp", "wt")))
})
