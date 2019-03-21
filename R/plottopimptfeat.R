#' megaR analysis
#'
#' @param RF_state_classify the random forest classifier
#' @param noOfFeature number of important feature to display in  barplot 
plotimportantfeatures <- function(RF_state_classify, noOffeature){
fc<- colorRampPalette(rev(brewer.pal(n = 9, "Blues")))
RF_state_classify_imp <- as.data.frame( RF_state_classify$importance )
RF_state_classify_imp$features <- rownames( RF_state_classify_imp )
RF_state_classify_imp_sorted <- arrange( RF_state_classify_imp  , desc(MeanDecreaseAccuracy)  )
barplot(RF_state_classify_imp_sorted[1:10,"MeanDecreaseAccuracy"],
        names.arg= str_replace(RF_state_classify_imp_sorted[1:10,"features"], "X", "OTU_") ,
        ylab="Mean Decrease in Accuracy", las=2, ylim=c(0,0.015), main="Top ten important variables for classification", 
        cex.axis = .75, cex.names = .75, col= fc(10))  

}
