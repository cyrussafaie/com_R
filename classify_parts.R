
#Dependencies
library(caret)
library(Metrics)
library(randomForest)
library(gbm)


source("extract_features.R")
source("train_models.R")


AUGMENT_SIZE = 200

set.seed(1606)


#generate mean over each individual augment collection
aggregate_mean <- function(data, window = 200){
    return(unname(tapply(data, (seq_along(data)-1) %/% window, mean)))
}

#generate augments to supplement records for each electronic_part
generate_augments <- function(set){

    augments = list()

    #Iterate over set_ID - unique Product,customer_segment pairs
    set_ID = unique(set[c("PRODUCT_NUMBER", "CUSTOMER_SEGMENT1")])
    for (i in 1:nrow((set_ID))){
        #records for each part
        print(paste(i,paste("/"),dim(set_ID)[1]));
        group <- set[ which(set$PRODUCT_NUMBER==set_ID[i,1] & set$CUSTOMER_SEGMENT1 == set_ID[i,2]),]
        #sort group by date to get sliding difference
        group$TRANSACTION_DATE = substr(group$TRANSACTION_DATE,1,10)
        group = group[(order(as.Date(group$TRANSACTION_DATE))),]


        for (t in 1:AUGMENT_SIZE){
            #generate random number between 0 and 0.95 and pass it to feature extractor
            r <- runif(1, 0.0, 0.95)
            augment_featurelist = extractFeatures(group, r)
            augments = rbind(augments, augment_featurelist)
        }
    }
    
    aug = data.frame(matrix(unlist(augments), nrow=nrow(augments), byrow=F))
    return(aug)
}



train = read.csv("train.csv")
test = read.csv("test.csv")

train_ID = unique(train[c("PRODUCT_NUMBER", "CUSTOMER_SEGMENT1")])
test_ID = unique(test[c("PRODUCT_NUMBER", "CUSTOMER_SEGMENT1")])


#Generate ground truth for augments
gtNO = list()
gtMAYBE = list()
gtYES = list()
ones = rep(1.0,AUGMENT_SIZE)
zeros = rep(0.0,AUGMENT_SIZE)

gt_ID = unique(train[c("PRODUCT_NUMBER", "CUSTOMER_SEGMENT1","SPECIAL_PART")])
for (i in 1:nrow(gt_ID)){
    if (gt_ID[i,3] == "No"){
        gtNO = c(gtNO , ones)
        gtMAYBE = c(gtMAYBE , zeros)
        gtYES = c(gtYES , zeros)
    }
    if (gt_ID[i,3] == "Maybe"){
        gtNO = c(gtNO , zeros)
        gtMAYBE = c(gtMAYBE , ones)
        gtYES = c(gtYES , zeros)
    }
    if (gt_ID[i,3] == "Yes"){
        gtNO = c(gtNO , zeros)
        gtMAYBE = c(gtMAYBE , zeros)
        gtYES = c(gtYES , ones)
    }
}

gt = cbind(as.numeric(gtNO), as.numeric(gtMAYBE), as.numeric(gtYES))

trainfv = generate_augments(train)
testfv = generate_augments(test)

write.csv(trainfv, "output/Train_features.csv")
write.csv(testfv, "output/Test_features.csv")

print("Training Models")

modelsNO = train_ensemble(trainfv, gt[,1])
modelsMAYBE = train_ensemble(trainfv, gt[,2])
modelsYES = train_ensemble(trainfv, gt[,3])

print("Generating predictions")

augment_prediction_NO = predict_ensemble(testfv, modelsNO)
augment_prediction_MAYBE = predict_ensemble(testfv, modelsMAYBE)
augment_prediction_YES = predict_ensemble(testfv, modelsYES)


part_prediction_NO = aggregate_mean(augment_prediction_NO)
part_prediction_MAYBE = aggregate_mean(augment_prediction_MAYBE)
part_prediction_YES = aggregate_mean(augment_prediction_YES)


test_ID = unique(test[c("PRODUCT_NUMBER", "CUSTOMER_SEGMENT1")])


prediction = cbind(part_prediction_NO,part_prediction_MAYBE,part_prediction_YES)

write.csv(prediction, "output/probabilities.csv")

save(modelsNO, file = "EnsembleNO.Rdata")
save(modelsMAYBE, file = "EnsembleMAYBE.Rdata")
save(modelsYES, file = "EnsembleYES.Rdata")


output = cbind(test_ID, prediction)
output = data.frame(matrix(unlist(output), nrow=nrow(output), byrow=F))

colnames(output) <- c("PRODUCT_NUMBER", "CUSTOMER_SEGMENT1", "NO_PSEUDO", "MAYBE_PSEUDO", "YES_PSEUDO")
write.csv(output, "output/FinalOutput.csv", row.names=FALSE)

