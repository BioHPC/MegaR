#' megaR analysis
#'
#' This is an internal function used to predict the given dataset.
#'
#' @param unknormdata unknown dataset
#' @param a model list with elements ******
getunknpredict <- function(unknormdata, a){
  #unknormdata <- getGoodfeature(getLevelDataM(x,level), threshold, samplePercent , normval )
  
  new_leveled_ukn.s1 <- unknormdata[match(rownames(t(a[[2]])) , rownames(unknormdata)), ] ### TO DO
  new_leveled_ukn.s1 <- t(new_leveled_ukn.s1)
  new_leveled_ukn.s1[is.na(new_leveled_ukn.s1)] <- 0
  #prediction of unknown data
  RF.predict <- predict(a[[3]] , new_leveled_ukn.s1)
  return(RF.predict)
}
