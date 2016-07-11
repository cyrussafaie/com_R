
#Dependencies
library(caret)
library(Metrics)
library(randomForest)
library(gbm)

source("extract_features.R")
source("train_models.R")

train = read.csv("train.csv")
test = read.csv("test.csv")

test_ID = unique(test[c("PRODUCT_NUMBER", "CUSTOMER_SEGMENT1")])


prediction = read.csv("output/probabilities.csv")
prediction = prediction[,-1] #remove first column (row numbers)


load("EnsembleNO.Rdata")
load("EnsembleMAYBE.Rdata")
load("EnsembleYES.Rdata")




output = cbind(test_ID, prediction)
output = data.frame(matrix(unlist(output), nrow=nrow(output), byrow=F))

colnames(output) <- c("PRODUCT_NUMBER", "CUSTOMER_SEGMENT1", "NO_PSEUDO", "MAYBE_PSEUDO", "YES_PSEUDO")
write.csv(output, "output/FinalOutput.csv", row.names=FALSE)