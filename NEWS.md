# lorax (development version)

* `extract_rules()` now supports C5.0 rule-based models (created with `rules = TRUE` parameter). Previously only tree-based C5.0 models were supported. The implementation handles all condition types including numeric comparisons, single categorical values, and multiple categorical values (e.g., `county %in% c("asotin", "columbia", "garfield", "island")`).

# lorax 0.1.0

Initial version
