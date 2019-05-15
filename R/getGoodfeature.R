#'megaR getGoodfeature
#'
#' This is an internal function used to collect good features
#'
#' @param alltable2  table containing all the features analyzed
#' @param threshold threshold of the value of feature that should
#'  be across the sample
#' @param samplePercent percentage of sample that should contain
#'  the threshold amount of value in its feature
#' @param normval wheather cumulative sum normalization is to be used or not
#' @export

getGoodfeature <- function(alltable2, threshold, samplePercent, normval){
    row2keep <- c()
    cutoff <- ceiling( (samplePercent/100) * ncol(alltable2) )
    #updateProgress(detail = "Calculating processing-value")
    for ( i in 1:nrow(alltable2)) {
        row_nonzero <- length( which( alltable2[ i , ]  > threshold ) )
        if ( row_nonzero > cutoff ) {
            row2keep <- c( row2keep , i)
        }
    }
    #updateProgress(detail = "doing normalization")
    a<-alltable2[ row2keep , , drop=F ]
    if (normval=="none"){
        # rownames(a) <- str_remove(rownames(a), "[.*__g]")
        return(a)
    }
    else{
        normdata <- sweep(a, 2, colSums(a) , '/')*100
        #rownames(normdata) <- str_remove(rownames(normdata), "[.*__g]")
        return(normdata)
    }
    #updateProgress(detail = "Displaying the result")
}
