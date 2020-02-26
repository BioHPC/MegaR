#' MegaR analysis
#'
#' This is an internal function used to predict the given dataset.
#'
#' @param unknormdata unknown dataset
#' @param a model list with elements ******
#' @export
getunknpredict <- function(unknormdata, a){
    rownames(unknormdata)<- gsub("|", ".", rownames(unknormdata), fixed = TRUE)
    new_leveled_ukn.s1 <- unknormdata[match(colnames(a[[1]]),
                                            rownames(unknormdata)),]
    rownames(new_leveled_ukn.s1) <- colnames(a[[1]])
    new_leveled_ukn.s1[is.na(new_leveled_ukn.s1)] <- 0
    #prediction of unknown data
    RF.predict <- stats::predict(a[[3]] , t(new_leveled_ukn.s1))
    tablemy <- data.frame( rownames(t(new_leveled_ukn.s1)), RF.predict)
    return(tablemy)
}
