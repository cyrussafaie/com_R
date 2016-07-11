#Functions for training models (random forest and boosted forest)


#Summary functions

#mean quartic error
mqeSummary <- function(data, lev, model){
    out <- mean((data$obs - data$pred)^4)  
    names(out) <- "MQE"
    out
}


#logloss
loglossSummary <- function(data, lev, model){
    data[,'pred'] = 1/(1+exp(-data[,'pred']))
    ll = logLoss(data[, "obs"], data[, "pred"]) 
    ret = ll; names(ret) = 'logloss'
    return(ret)
}





#Train an ensemble of 2 random forests and 1 boosted forest
#train(trainfv, gt$V1 , method = "gbm",tuneGrid = newGrid, trControl = mControl, metric = "logloss")
train_ensemble = function(trainfeats, groundtruth){

    #Random Forest Mean Square Error
    RFMSEGrid = expand.grid(mtry = c(10))
    RFMSEctrl <- trainControl(method="boot", verboseIter = TRUE)

    RFMSE = train(trainfeats, groundtruth , importance = FALSE, method = "rf", ntree = 500,tuneGrid = RFMSEGrid, trControl = RFMSEctrl)



    #Random Forest Mean Quartic Error
    RFMQEGrid = expand.grid(mtry = c(10))
    RFMQEctrl <- trainControl(method="boot", verboseIter = TRUE,summaryFunction = mqeSummary)

    RFMQE = train(trainfeats, groundtruth , importance = FALSE, method = "rf", ntree = 500,tuneGrid = RFMQEGrid, trControl = RFMQEctrl, metric = "MQE")



    #Boosted Forest Log Loss Error
    BFLLGrid = expand.grid(n.trees = c(6000), interaction.depth = c(1), shrinkage = c(0.001), n.minobsinnode = c(10))
    BFLLctrl <- trainControl(method="boot", verboseIter = TRUE, summaryFunction = loglossSummary)

    BFLL = train(trainfeats, groundtruth , method = "gbm", tuneGrid = BFLLGrid, trControl = BFLLctrl, metric = "logloss")


    return (list("RFMSE" = RFMSE, "RFMQE" = RFMQE, "BFLL" = BFLL))
}



predict_ensemble <- function(testfeats, ensemble){

    PredictionRFMSE <- predict(ensemble$RFMSE, newdata=testfeats)
    PredictionRFMQE <- predict(ensemble$RFMQE, newdata=testfeats)
    PredictionBFLL <- predict(ensemble$BFLL, newdata=testfeats)


    Prediction <- (PredictionRFMSE + PredictionRFMQE + PredictionBFLL)/3

    return(Prediction)
}
