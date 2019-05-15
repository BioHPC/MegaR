#' megaR analysis
#' This function plots the top 10 important features for random forest
#' classification.
#'
#' @param RF_state_classify the random forest model
#' @param noOffeature no. of feature =10
#' @export
#'
plotimptfeatures <- function(RF_state_classify, noOffeature){
    fc<- grDevices::colorRampPalette(rev(brewer.pal(n = 9, "Blues")))
    RF_state_classify_imp <- as.data.frame( RF_state_classify$importance )
    with(RF_state_classify_imp,{
    RF_state_classify_imp$features <- rownames( RF_state_classify_imp )
    RF_state_classify_imp_sorted <- plyr::arrange( RF_state_classify_imp  ,
                        plyr::desc(MeanDecreaseAccuracy))
    a <- graphics::barplot(RF_state_classify_imp_sorted[1:10,
                                                        "MeanDecreaseAccuracy"],
            names.arg=stringr::str_replace(RF_state_classify_imp_sorted[
                1:10,"features"],
                                   "X", "OTU_") ,
            ylab="Mean Decrease in Accuracy", las=2, ylim=c(0,0.015),
            main="Top ten important variables for classification",
            cex.axis = .75, cex.names = .75, col= fc(10))
    })
    return(a)
}
