
<!-- README.md is generated from README.Rmd. Please edit that file -->

# lorax

<!-- badges: start -->

[![R-CMD-check](https://github.com/topepo/lorax/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/topepo/lorax/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/topepo/lorax/graph/badge.svg)](https://app.codecov.io/gh/topepo/lorax)
<!-- badges: end -->

The goal of lorax is to help look at different aspects of tree- and
rule-based models.

## Installation

You can install the development version of lorax like so:

``` r
require(pak)
pak::pak("topepo/lorax)
```

## Example

``` r
library(tibble)
library(lorax)
library(ranger)
library(palmerpenguins)
#> 
#> Attaching package: 'palmerpenguins'
#> The following objects are masked from 'package:datasets':
#> 
#>     penguins, penguins_raw
library(partykit)
#> Loading required package: grid
#> Loading required package: libcoin
#> Loading required package: mvtnorm

set.seed(822)
rngr_fit <- ranger(species ~ ., data = penguins, max.depth = 3, num.trees = 10)
```

``` r
rngr_party <- as.party(rngr_fit, tree = 1)
rngr_party
#> 
#> Model formula:
#> ~island + bill_length_mm + bill_depth_mm + flipper_length_mm + 
#>     body_mass_g + sex + year
#> 
#> Fitted party:
#> [1] root
#> |   [2] flipper_length_mm <= 206.5
#> |   |   [3] body_mass_g <= 2800: Adelie (n = 3, err = 66.7%)
#> |   |   [4] body_mass_g > 2800
#> |   |   |   [5] bill_length_mm <= 44.15: Adelie (n = 152, err = 3.9%)
#> |   |   |   [6] bill_length_mm > 44.15: Chinstrap (n = 60, err = 6.7%)
#> |   [7] flipper_length_mm > 206.5
#> |   |   [8] bill_depth_mm <= 18.15: Gentoo (n = 123, err = 0.8%)
#> |   |   [9] bill_depth_mm > 18.15: Chinstrap (n = 6, err = 16.7%)
#> 
#> Number of inner nodes:    4
#> Number of terminal nodes: 5

plot(rngr_party)
```

<img src="man/figures/README-ranger-party-1.png" alt="" width="100%" />

``` r
all_rules <- extract_rules(rngr_party, trees = 10)

# An expression
all_rules$rules[[1]]
#> flipper_length_mm <= 206.5 & body_mass_g <= 2800

# Text
all_rules$rules[[1]] |> rule_text()
#> [1] "flipper_length_mm <= 206.5 & body_mass_g <= 2800"

# Substitutions
new_names <- 
 tribble(
  ~ original, ~ label,
  "flipper_length_mm", "Flipper Length",
  "body_mass_g", "Body Mass"
 )
 
all_rules$rules[[1]] |> rule_text(key = new_names)
#> [1] "Flipper Length <= 206.5 & Body Mass <= 2800"

# Bullets:
all_rules$rules[[1]] |> 
 rule_text(key = new_names, bullets = TRUE) |> 
 cat()
#> * Flipper Length <= 206.5
#> * Body Mass <= 2800
```

## Code of Conduct

Please note that the lorax project is released with a [Contributor Code
of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
