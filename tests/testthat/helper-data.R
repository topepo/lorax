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

# Get wa_trees data (mixed predictors with factors)
get_wa_trees_data <- function() {
  load(system.file(package = "lorax", "wa_trees.RData"), envir = environment())
  get("wa_trees", envir = environment())
}

# Create binary classification data with predictive ability
get_binary_data <- function() {
  skip_if_not_installed("modeldata")
  set.seed(487)
  data <- modeldata::sim_classification(100, num_linear = 3)
  # Rename class to y for consistency
  names(data)[names(data) == "class"] <- "y"
  data
}

# Create regression data with predictive ability
get_regression_data <- function() {
  skip_if_not_installed("modeldata")
  set.seed(487)
  data <- modeldata::sim_regression(100)
  # Rename outcome to y for consistency
  names(data)[names(data) == "outcome"] <- "y"
  data
}

# Get penguins data with factor predictors
get_factor_data <- function() {
  skip_if_not_installed("palmerpenguins")
  data("penguins", package = "palmerpenguins", envir = environment())
  penguins <- get("penguins", envir = environment())
  # Use species as outcome and island, sex as predictors
  data <- na.omit(penguins[, c(
    "species",
    "island",
    "sex",
    "bill_length_mm",
    "bill_depth_mm"
  )])
  names(data)[names(data) == "species"] <- "y"
  data
}

# Get penguins forest for ObliqueForest tests
get_penguins_forest <- function() {
  skip_if_not_installed("aorsf")
  skip_if_not_installed("palmerpenguins")
  penguins <- palmerpenguins::penguins[
    complete.cases(palmerpenguins::penguins),
  ]
  set.seed(487) # Ensure reproducible forest structure
  aorsf::orsf(species ~ ., data = penguins, n_tree = 10)
}

# Get penguins tree for party tests
get_penguins_tree <- function() {
  skip_if_not_installed("partykit")
  skip_if_not_installed("palmerpenguins")
  penguins <- palmerpenguins::penguins
  partykit::ctree(species ~ ., data = penguins)
}

# Get penguins rpart tree for rpart tests
get_penguins_rpart_tree <- function() {
  skip_if_not_installed("rpart")
  skip_if_not_installed("palmerpenguins")
  data("penguins", package = "palmerpenguins", envir = environment())
  penguins <- get("penguins", envir = environment())
  penguins <- na.omit(penguins)
  rpart::rpart(species ~ ., data = penguins)
}

# Get penguins data with single numeric predictor
get_single_numeric_data <- function() {
  skip_if_not_installed("palmerpenguins")
  data("penguins", package = "palmerpenguins", envir = environment())
  penguins <- get("penguins", envir = environment())
  # Use species as outcome and flipper_length_mm as single numeric predictor
  data <- na.omit(penguins[, c("species", "flipper_length_mm")])
  names(data)[names(data) == "species"] <- "y"
  names(data)[names(data) == "flipper_length_mm"] <- "x"
  data
}

# Get penguins data with single factor predictor
get_single_factor_data <- function() {
  skip_if_not_installed("palmerpenguins")
  data("penguins", package = "palmerpenguins", envir = environment())
  penguins <- get("penguins", envir = environment())
  # Use species as outcome and island as single factor predictor
  data <- na.omit(penguins[, c("species", "island")])
  names(data)[names(data) == "species"] <- "y"
  names(data)[names(data) == "island"] <- "x"
  data
}
