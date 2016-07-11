
####Introduction####

This project contains an R model for the classification of Electronic Parts. The model is an ensemble of
3 tree-based models (1 random forest trained on mean square error, 1 random forest trained on mean quartic
error and 1 boosted forest trained on logloss error)




####Dependencies####

The project depends on the following R libraries:

(caret) - for unified interfaces for training and testing models
(Metrics) - for loss functions (logloss in particular)
(randomForest) - random forest
(gbm) - boosted forest equivalent

These can be installed using

install.packages(c("caret", "Metrics", "randomForest", "gbm"))



####Outputs####

The code will generate train and test features for the given data, as well as final predictions including pseudo-probabilities for the 
considered classes.
