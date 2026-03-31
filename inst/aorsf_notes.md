# aorsf Rule Extraction Investigation Notes

## Overview

This document details the investigation into extracting decision rules from aorsf `ObliqueForest` objects, including a critical unresolved issue where extracted rules do not perfectly match aorsf's internal node assignments.

## Background

The aorsf package (Oblique Random Survival Forests) creates random forests using oblique splits - linear combinations of multiple variables instead of axis-aligned splits. For example:

- **Rectangular split**: `x < 5`
- **Oblique split**: `2*x - 3*y >= 10`

Goal: Extract interpretable rules from aorsf trees similar to existing `extract_rules.rpart()` and `extract_rules.party()` methods.

## aorsf Internal Structure

### Tree Storage Format

aorsf stores trees in a compact format using these key components (all 0-indexed):

```r
forest$forest$child_left[[tree_num]][node_idx]    # Left child index (or 0 if terminal)
forest$forest$coef_indices[[tree_num]][[node_idx]] # Variable indices (0-indexed)
forest$forest$coef_values[[tree_num]][[node_idx]]  # Coefficients for linear combination
forest$forest$cutpoint[[tree_num]][node_idx]       # Threshold value
forest$forest$leaf_summary[[tree_num]][[node_idx]] # Prediction value for terminal nodes
```

### Node Structure

- **Node indexing**: 0-based (node 0 = root)
- **Terminal nodes**: `child_left[i] == 0`
- **Children**: If `child_left[i] = k` (k > 0):
  - Left child at index `k`
  - Right child at index `k + 1`

### Split Logic (from Tree.cpp)

```cpp
lincomb = sum(coef_values[i] * x[coef_indices[i]])

if (lincomb <= cutpoint[i]) {
    pred_leaf[obs] = child_left[i];      // Go LEFT
} else {
    pred_leaf[obs] = child_left[i] + 1;  // Go RIGHT
}
```

## Implementation Details

### What We Built

1. **`extract_rules.ObliqueForest()`**: Main S3 method
2. **Helper functions**:
   - `oblique_get_terminal_nodes()`: Find leaf nodes
   - `oblique_find_parent()`: Find parent of a node
   - `oblique_build_node_path()`: Build path from root to leaf
   - `oblique_get_split_info()`: Extract split details
   - `oblique_unscale_split()`: Reverse scaling transformation
   - `oblique_replace_indicators()`: Convert factor indicators to comparisons
   - `oblique_get_var_names()`: Get expanded variable names (with one-hot encoding)

### Key Features Implemented

#### 1. Scaling and Unscaling

aorsf can optionally scale predictors via `scale_x` parameter in `orsf_control_*()`:

```r
# When scale_x = TRUE (default):
scaled_x = (x - mean) / sd

# Our unscaling reverses this:
# coef * scaled_x = coef * (x - mean) / sd
# = (coef / sd) * x - (coef * mean / sd)
# So: unscaled_coef = coef / sd
#     threshold_adjustment = sum(coef * mean / sd)
```

**Critical finding**: `get_means()` and `get_stdev()` are ALWAYS populated regardless of `scale_x`. Must check `x$control$lincomb_scale` to determine if scaling was actually used.

#### 2. Factor Reference Coding

aorsf uses **reference coding** (k-1 indicators for k-level factors):

```r
# Factor with levels ["red", "blue", "green"] becomes:
# color_blue, color_green (red is the reference)
# When both indicators are 0, it represents red
```

This prevents collinearity in internal regression computations.
Factor indicators are NOT scaled (remain 0/1).

**Enhancement**: We automatically convert indicators back to factor comparisons for interpretability:
- Internal: `2.1 * county_adams`
- Output: `2.1 * (county == "adams")`

Implementation in `oblique_replace_indicators()` recursively walks expression tree replacing indicator symbols with factor comparison expressions.

#### 3. Indexing Conventions

**User-facing (1-based)**:
- `tree = 1`: first tree
- `id = 1`: first terminal node

**Internal (0-based)**:
- Node 0 = root
- Automatic conversion: `user_id = internal_id + 1`

## The Critical Issue: Node Assignment Mismatch

### Problem Statement

Extracted rules do NOT match aorsf's predictions. When evaluating rules on training data, observations are assigned to different nodes than aorsf assigns them.

### Concrete Example

```r
set.seed(42)
forest <- orsf(body_mass_g ~ ., data = penguins_numeric, n_tree = 1,
               control = orsf_control_regression(scale_x = FALSE))

# Observation 179 characteristics:
# bill_length_mm=59.6, bill_depth_mm=17, flipper_length_mm=230, body_mass_g=6050

# aorsf says:
leaf_ids <- predict(forest, pred_type = "leaf")
leaf_ids[179, 1]  # = 14 (0-indexed)

# Prediction value:
predict(forest)[179]  # = 5825
forest$forest$leaf_summary[[1]][[15]]  # = 5825 (matches node 14)

# Our manual trace through tree:
# Node 0: 728.58*230 + 64.73*17 = 168673.6 > 400.23 → RIGHT to node 2
# Node 2: 200.08*59.6 + 650.41*230 = 161519.5 > 1097.05 → RIGHT to node 6
# Node 6: -45.73*230 + 465.82*17 = -2597.99 <= -299.30 → LEFT to node 13
# Node 13: ... → eventually reaches node 48

# Our extracted rule for node 15 (1-indexed = node 14 0-indexed):
eval(rules$rules[[which(rules$id == 15)]], obs_179)  # = FALSE

# Observation 179 should match node 15's rule but doesn't
```

### Investigation Steps

#### Step 1: Verified Path Building

```r
# Manual path construction for node 14:
# 0 -> 2 -> 6 -> 14
# Verified using oblique_find_parent() logic
```

Confirmed: Path building correctly traces parent-child relationships.

#### Step 2: Verified Split Direction

```r
# At node 6:
child_left[7] = 13  # So LEFT=13, RIGHT=14
# Want node 14, so should use operator ">"
is_left <- (child_left[7] == 14)  # FALSE
operator <- ifelse(is_left, "<=", ">")  # ">"
```

Confirmed: Direction logic correctly determines LEFT vs RIGHT.

#### Step 3: Verified Coefficients

```r
# Node 6 split:
coef_indices: [2, 1] → [flipper_length_mm, bill_depth_mm]
coef_values: [-45.73, 465.82]
cutpoint: -299.30
```

Confirmed: Coefficient extraction matches aorsf's internal values exactly.

#### Step 4: Verified Unscaling

```r
# scale_x = FALSE, so lincomb_scale = FALSE
# No unscaling applied (correctly)
```

Confirmed: Unscaling logic only applies when needed.

#### Step 5: Manual Evaluation

```r
# At node 6 for observation 179:
lincomb = -45.73 * 230 + 465.82 * 17
        = -10517.45 + 7918.94
        = -2598.51

cutpoint = -299.30

# Should go:
lincomb <= cutpoint?  -2598.51 <= -299.30?  TRUE → LEFT (node 13)
lincomb > cutpoint?   -2598.51 > -299.30?   FALSE

# But aorsf assigns to node 14 (RIGHT child)
```

**Key finding**: Our manual calculation shows observation should go LEFT to node 13, but aorsf assigns it to node 14 (RIGHT child).

#### Step 6: Examined aorsf C++ Source

From `src/Tree.cpp`:

```cpp
void Tree::predict_leaf(Data* prediction_data, bool oobag) {
    for(uword i = 0; i < coef_values.size(); i++){
        if(child_left[i] != 0){
            lincomb = prediction_data->x_submat_mult_beta(obs_in_node,
                                                          coef_indices[i],
                                                          coef_values[i]);

            if(lincomb[j] <= cutpoint[i]) {
                pred_leaf[*it] = child_left[i];     // LEFT
            } else {
                pred_leaf[*it] = child_left[i]+1;   // RIGHT
            }
        }
    }
}
```

Logic appears straightforward: `lincomb <= cutpoint` goes LEFT, otherwise RIGHT.

### Hypothesis: Possible Root Causes

1. **`x_submat_mult_beta()` does something unexpected**: Maybe this matrix multiplication has special behavior we're not replicating?

2. **Data transformation**: Perhaps prediction data is transformed differently than training data?

3. **Out-of-bag handling**: Some conditional logic based on OOB status?

4. **Coefficient/index mismatch**: Despite matching numerically, perhaps there's an indexing subtlety?

5. **Floating point precision**: Unlikely but possible rounding differences?

6. **Tree structure interpretation**: Missing some aspect of how nodes are actually organized?

### What We Tried

- Predicted with `new_data` parameter vs without: Same results
- Predicted single observation vs full dataset: Same assignment
- Checked with `scale_x = TRUE` vs `FALSE`: Different assignments but still mismatched
- Verified column index mapping: Correct
- Manually computed linear combinations: Matches our formula but not aorsf's decision

### Current Status

**Tests**: 539 pass, 2 skipped (validation tests that would verify node assignments)

**Skipped tests**:
```r
test_that("extract_rules.ObliqueForest() rules match aorsf node assignments", {
  skip("TODO: Debug rule extraction - node assignments don't match aorsf")
  # Test that each obs matches exactly one rule
  # Test that matching rule ID equals aorsf's leaf assignment
})

test_that("extract_rules.ObliqueForest() node assignments are consistent", {
  skip("TODO: Debug rule extraction - node assignments don't match aorsf")
  # Test that obs assigned to each node match between aorsf and rules
})
```

**Documentation**: Added "Known Limitation" section warning users that rules don't perfectly match predictions.

## What Works Well

Despite the node assignment issue, the implementation provides value:

1. **Rules extract successfully** with proper oblique split structure
2. **Factor conversion**: `county_adams` → `(county == "adams")`
3. **Unscaling**: Automatic when `lincomb_scale = TRUE`
4. **Interpretability**: Rules show which variables and combinations matter
5. **1-based indexing**: User-friendly interface
6. **Integration**: Works with `rule_text()` for formatting

## Recommendations for Resolution

### Immediate Next Steps

1. **Instrument aorsf C++ code**: Add logging to `Tree::predict_leaf()` to see actual linear combination values computed during prediction

2. **Compare step-by-step**: Run same observation through both implementations logging every intermediate value

3. **Check `x_submat_mult_beta()` implementation**: Understand exactly what this function does

4. **Test with simpler tree**: Create minimal example (3-4 nodes) and trace by hand

### Long-term Solution Paths

**Option A: Direct API** - Request aorsf add a function to export complete rules directly

**Option B: Node path extraction** - If aorsf could export the actual path taken for each observation, we could verify our reconstruction

**Option C: Minimal reproduction** - Create smallest possible example that demonstrates the mismatch and share with aorsf maintainer

### For aorsf Maintainer

If investigating this issue:

1. Check if `x_submat_mult_beta()` has any special handling of:
   - Missing values
   - Factor indicators
   - Scaling/centering
   - Data storage format

2. Verify the split condition is truly `lincomb <= cutpoint` for LEFT in all cases

3. Consider if node traversal differs between training and prediction

## Code Locations

### lorax Package

- **Implementation**: `R/ObliqueForest.R`
- **Tests**: `tests/testthat/test-ObliqueForest.R`
- **Documentation**: `man/extract_rules.ObliqueForest.Rd`

### aorsf Package

- **Tree traversal**: `src/Tree.cpp` - `Tree::predict_leaf()`
- **Matrix multiplication**: Likely in `src/Data.cpp` - `x_submat_mult_beta()`
- **Factor encoding**: `R/fctr.R` - `fctr_info()`
- **Scaling**: `src/Coxph.cpp` - `cph_scale()`

## Session Info for Reproduction

```r
library(aorsf)
library(lorax)

penguins <- palmerpenguins::penguins[complete.cases(palmerpenguins::penguins), ]
penguins_numeric <- penguins[, c('bill_length_mm', 'bill_depth_mm',
                                  'flipper_length_mm', 'body_mass_g')]

set.seed(42)
forest <- orsf(body_mass_g ~ ., data = penguins_numeric, n_tree = 1,
               control = orsf_control_regression(scale_x = FALSE))

# Verify the issue:
leaf_ids <- predict(forest, pred_type = "leaf", new_data = penguins_numeric)
rules <- extract_rules(forest, tree = 1)

# Check observation 179
obs_179 <- penguins_numeric[179, , drop = FALSE]
aorsf_node <- leaf_ids[179, 1]  # Should be 14
rule_matches <- sapply(rules$rules, function(r) eval(r, obs_179)[1])
our_node <- rules$id[rule_matches] - 1  # Convert to 0-indexed

print(paste("aorsf assigns to node:", aorsf_node))
print(paste("Our rules assign to node:", our_node))
# These will differ
```

## Timeline

- **2026-03-14**: Initial implementation completed
- **2026-03-14**: Node assignment mismatch discovered during validation test creation
- **2026-03-14**: Extensive debugging conducted, issue documented but unresolved
- **2026-03-30**: Root causes identified and fixed

## Resolution

### Root Cause #1: Reference Coding

**Problem**: The implementation incorrectly assumed aorsf uses full one-hot encoding (k indicators for k levels), but aorsf actually uses reference coding (k-1 indicators, with the first level as reference).

**Evidence**: In `aorsf/R/ref_code.R` line 39: `seq(length(fi$keys[[i]]) - 1)` creates only k-1 indicators, and line 84: `fi$keys[[i]][-1]` excludes the first level.

**Impact**: Variable indices were off by one for each factor, causing coefficients to map to wrong variables.

**Fix**: Modified three functions in `R/ObliqueForest.R`:
- `oblique_get_var_names()`: Use `fctr_info$keys[[var_name]][-1]`
- `oblique_replace_indicators()`: Use `[-1]` for both keys and levels
- `oblique_collapse_factor_names()`: Use `fctr_info$keys[[factor_name]][-1]`

### Root Cause #2: Floating Point Precision Loss

**Problem**: The `oblique_unscale_split()` function pre-computed unscaled coefficients and thresholds. Multiple floating-point operations accumulated precision errors, causing comparisons like `5238.959... <= 5238.959...` to fail due to tiny differences (9e-13).

**Evidence**: Manual tracing with scaled data matched aorsf predictions, but rules with unscaled coefficients did not. Direct evaluation showed threshold values differing in the last decimal places.

**Key insight**: aorsf ALWAYS scales data during prediction (line 3168 in `orsf_R6.R`), regardless of `scale_x` setting. Coefficients in trees are for SCALED data.

**Fix**: Instead of pre-computing unscaled coefficients, include scaling transformations directly in rule expressions:
```r
# Before (loses precision):
51.98 * flipper_length_mm + 32.87 * bill_depth_mm > 11411.28

# After (full precision):
728.58 * ((flipper_length_mm - 200.97) / 14.02) +
  64.73 * ((bill_depth_mm - 17.16) / 1.97) > 400.23
```

Implementation: New function `oblique_split_to_scaled_expr()` builds expressions with scaling included, avoiding pre-computation errors.

### Validation

All 17 tests pass, including the two that were previously skipped:
- `extract_rules.ObliqueForest() rules match aorsf node assignments`
- `extract_rules.ObliqueForest() node assignments are consistent`

Rules now exactly replicate aorsf's internal predictions.

## Contact

This investigation was conducted as part of the lorax package development. For questions or to contribute to solving this issue, see the lorax GitHub repository.
