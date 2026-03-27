# lorax (development version)

* `active_predictors()` gains a `C5.0` method for extracting the set of predictors used in tree splits from C5.0 decision tree and boosted models, as well as from rule conditions in rule-based models. For tree-based models, the `tree` argument supports extracting from multiple boosting trials.
* `active_predictors()` gains an `ObliqueForest` method for extracting the set of predictors used in oblique tree splits from aorsf models. Supports extracting from multiple trees via the `tree` argument.
* `active_predictors()` gains a `party` method for extracting the set of predictors used in tree splits from partykit objects.
* `active_predictors()` gains an `rpart` method for extracting the set of predictors used in actual tree splits (excluding competing and surrogate splits).
* `as.party()` gains methods for bart, C5.0, grf, lgb.Booster, randomForest, ranger, and xgb.Booster objects, enabling conversion of these models to partykit's party structure for visualization and analysis.
* `extract_rules()` gains a `bart` method for extracting rules from BART models.
* `extract_rules()` gains an `lgb.Booster` method for extracting rules from LightGBM models.
* `extract_rules()` gains an `xgb.Booster` method for extracting rules from XGBoost models.
* `var_imp()` gains methods for grf, lgb.Booster, ObliqueForest, randomForest, ranger, rpart, and xgb.Booster objects for extracting variable importance scores. The randomForest method supports selecting different importance metrics via the `type` parameter, and the lgb.Booster and xgb.Booster methods support specifying the complete feature set via the `feature_names` parameter.
