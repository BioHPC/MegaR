#'MegaR getGoodfeature
#'
#' This is an internal function used to collect good features
#'
#' @param alltable2  table containing all the features analyzed
#' @param threshold threshold of the value of feature that should
#'  be across the sample
#' @param samplePercent percentage of sample that should contain
#'  the threshold amount of value in its feature
#' @param normval wheather cumulative sum normalization(default), Trimmed mean-median normalization, quantile normalization or no normalization should be used
#' @export
quantile_normalisation <- function(df){
    df_rank <- apply(df,2,rank,ties.method="min")
    df_sorted <- data.frame(apply(df, 2, sort))
    df_mean <- apply(df_sorted, 1, mean)
    
    index_to_mean <- function(my_index, my_mean){
        return(my_mean[my_index])
    }
    
    df_final <- apply(df_rank, 2, index_to_mean, my_mean=df_mean)
    rownames(df_final) <- rownames(df)
    return(df_final)
}

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
    a <- alltable2[ row2keep , , drop=F ]
    newmatrix <- matrix(1:length(a), nrow = nrow(a), ncol = ncol(a))
    if (normval=="none"){
        # rownames(a) <- str_remove(rownames(a), "[.*__g]")
        return(a)
    }
    else if (normval=="quantile")
    {
        return(quantile_normalisation(a))
    }
    else if (normval == "TMM"){
        dg <- edgeR::DGEList(a)
        dfm <- edgeR::calcNormFactors(dg, method = "TMM")
        newmatrix <- edgeR::cpm(dfm, log=FALSE)
        return(newmatrix)
    }
    else{
        normdata <- sweep(a, 2, colSums(a) , '/')*100
        #rownames(normdata) <- str_remove(rownames(normdata), "[.*__g]")
        return(normdata)
    }
    #updateProgress(detail = "Displaying the result")
}
