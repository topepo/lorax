# lorax (development version)

* `as.party()` gains methods for bart, C5.0, grf, lgb.Booster, randomForest, ranger, and xgb.Booster objects, enabling conversion of these models to partykit's party structure for visualization and analysis.
* `extract_rules()` gains a `bart` method for extracting rules from BART models.
* `extract_rules()` gains an `lgb.Booster` method for extracting rules from LightGBM models.
* `extract_rules()` gains an `xgb.Booster` method for extracting rules from XGBoost models.
