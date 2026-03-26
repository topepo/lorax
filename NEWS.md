# lorax (development version)

* `active_predictors()` gains methods for bart, ObliqueForest, party, randomForest, ranger, and rpart objects. Forest-based methods (bart, ObliqueForest, randomForest, ranger) support extracting from multiple trees via the `tree` argument.
* `as.party()` gains methods for bart, C5.0, grf, lgb.Booster, randomForest, ranger, and xgb.Booster objects, enabling conversion of these models to partykit's party structure for visualization and analysis.
* `extract_rules()` gains methods for bart, lgb.Booster, and xgb.Booster objects.
