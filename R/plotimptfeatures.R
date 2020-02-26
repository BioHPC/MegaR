#' MegaR analysis
#' This function plots the top 10 important features for random forest
#' classification.
#'
#' @param RF_state_classify the random forest model
#' @param noOffeature no. of feature =10
#' @export
#'
plotimptfeatures <- function(RF_state_classify, noOffeature){
lk<- varImp(RF_state_classify)
tablelk<-lk$importance
tablelk$first <- rownames(tablelk)
new <-tablelk[order(tablelk$Overall, decreasing = TRUE),]
fc <- grDevices::colorRampPalette(rev(RColorBrewer::brewer.pal(n = 9, 
                                                              "Blues")))
a <- graphics::barplot(new[1:10,]$Overall,names.arg= substr(stringr::str_remove
                                             (new[1:10,]$first, ".*s__" ),1,20),
        cex.axis = .75, cex.names = .75, col = fc(10))
return(a)
}
