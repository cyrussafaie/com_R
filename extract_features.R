

safediv <- function(v1, v2){
    if (v2 == 0) return(0)
    return (v1/v2)
}

#convert strings to integer values similar to original code for consistency
intval <- function(str){
    sub = substr(str,1,1)
    v = strtoi(sub,36)
    return (v - strtoi("A",36))
}
extractFeatures <- function(group, r){
    empty = 0
    subset <- list()
    for (i in 1:nrow(group)){
        rd <- runif(1, 0.0, 1)
        if (rd < r) next
        if (group$TOTAL_BOXES_SOLD[i] == 0){
            empty++
            next
        }
        
        subset <- rbind(subset, group[i,])

    }

    #make sure there's at least 2 records
    if(length(subset)==0||nrow(subset)<2){
        return (extractFeatures(group, max(r - 0.1, 0.0)))
    }    
    #print(paste(i,paste(" ",nrow(subset))))
    #subsetting done already in chronological order (sorted during grouping)
    if (subset$PRODUCT_SALES_UNIT[1] == "Y") amount = subset$SHIPPING_WEIGHT
    else amount = subset$TOTAL_BOXES_SOLD
    costRatio = (safediv(subset$PRODUCT_COST1, amount))
    salesRatio = (safediv(subset$GROSS_SALES, amount))

    markup = subset$GROSS_SALES - subset$PRODUCT_COST1
    markupAvg = safediv(markup, amount)
    markupRatio = safediv(markup, subset$GROSS_SALES)

    costChanges = sum(abs(diff(costRatio,1)))
    
    uniqueCosts = length(unique(costRatio))
    uniqueSales = length(unique(salesRatio))
    
    n = nrow(subset)

    #generate features
    features =
       cbind(
            (costChanges / mean(costRatio)),
	        nrow(group),
	        intval(group$CUSTOMER_SEGMENT1[1]),
            group$PRODUCT_NUMBER[1],
	        safediv(uniqueCosts, n),
	        safediv(uniqueSales, n),
	        safediv(max(salesRatio), min(salesRatio)),
	        safediv(max(costRatio), min(costRatio)),
	        cor(salesRatio, costRatio),
	        safediv(empty, nrow(group)),
	        mean(subset$GROSS_SALES),
            mean(subset$PRODUCT_PRICE),
	        mean(amount),
            mean(subset$PRODUCT_COST1),
	        mean(markup),
	        mean(markupAvg),
	        mean(markupRatio),
	        mean(subset$PRICE_METHOD),
	        sd(subset$PRICE_METHOD),
	        mean(intval(subset$ORDER_SOURCE)),
	        mean(intval(subset$CUSTOMER_ACCOUNT_TYPE)),
	        mean(subset$CUSTOMER_TYPE1),
	        mean(intval(subset$CUSTOMER_TYPE2)),
	        sd(subset$GROSS_SALES),
            sd(subset$PRODUCT_PRICE),
	        sd(amount),
            sd(subset$PRODUCT_COST1),
	        sd(markup),
	        sd(markupAvg),
	        sd(markupRatio),
	        sd(intval(subset$ORDER_SOURCE)),
	        sd(intval(subset$CUSTOMER_ACCOUNT_TYPE)),
	        sd(subset$CUSTOMER_TYPE1),
	        sd(intval(subset$CUSTOMER_TYPE2)),
	        group$PRODUCT_ATTRIBUTE_X[1],
	        as.numeric(group$BRAND[1]=="IN_HOUSE"),
	        group$PRODUCT_CLASS_ID1[1],
	        group$PRODUCT_CLASS_ID2[1],
	        group$PRODUCT_CLASS_ID3[1],
	        group$PRODUCT_CLASS_ID4[1]
        )
    #correlation coefficient sometimes gives NA for 2 identical (x,y) pairs
    features[is.na(features)] <- 0
    return(features)
}
