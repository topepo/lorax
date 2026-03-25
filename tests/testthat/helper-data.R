# Test data helpers
# These functions create test datasets for various scenarios

# Get penguins data (classification with numeric predictors)
get_penguins_data <- function() {
  skip_if_not_installed("palmerpenguins")
  data("penguins", package = "palmerpenguins", envir = environment())
  penguins <- get("penguins", envir = environment())
  na.omit(penguins[, c(
    "species",
    "bill_length_mm",
    "bill_depth_mm",
    "flipper_length_mm",
    "body_mass_g"
  )])
}

# Get mtcars data (regression with numeric predictors)
get_mtcars_data <- function() {
  mtcars
}

# Get iris data (classification with numeric predictors)
get_iris_data <- function() {
  iris
}

# Get wa_trees data (mixed predictors with factors)
get_wa_trees_data <- function() {
  load(system.file(package = "lorax", "wa_trees.RData"), envir = environment())
  get("wa_trees", envir = environment())
}

# Create binary classification data
get_binary_data <- function(n = 100) {
  set.seed(123)
  data.frame(
    y = factor(sample(c("A", "B"), n, replace = TRUE)),
    x1 = rnorm(n),
    x2 = rnorm(n),
    x3 = rnorm(n)
  )
}

# Create regression data
get_regression_data <- function(n = 100) {
  set.seed(123)
  data.frame(
    y = rnorm(n),
    x1 = rnorm(n),
    x2 = rnorm(n),
    x3 = rnorm(n)
  )
}

# Create data with factors
get_factor_data <- function(n = 100) {
  set.seed(123)
  data.frame(
    y = factor(sample(c("A", "B", "C"), n, replace = TRUE)),
    x1 = rnorm(n),
    x2 = factor(sample(c("Low", "Medium", "High"), n, replace = TRUE)),
    x3 = rnorm(n),
    x4 = factor(sample(c("Red", "Blue"), n, replace = TRUE))
  )
}

# Get penguins forest for ObliqueForest tests
get_penguins_forest <- function() {
  skip_if_not_installed("aorsf")
  skip_if_not_installed("palmerpenguins")
  penguins <- palmerpenguins::penguins[complete.cases(palmerpenguins::penguins), ]
  aorsf::orsf(species ~ ., data = penguins, n_tree = 10)
}

# Get penguins tree for party tests
get_penguins_tree <- function() {
  skip_if_not_installed("partykit")
  skip_if_not_installed("palmerpenguins")
  penguins <- palmerpenguins::penguins
  partykit::ctree(species ~ ., data = penguins)
}

# Get iris rpart tree for rpart tests
get_iris_rpart_tree <- function() {
  skip_if_not_installed("rpart")
  rpart::rpart(Species ~ ., data = iris)
}
