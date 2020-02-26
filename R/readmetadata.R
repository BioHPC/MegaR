#' MegaR analysis
#' This is internal function to read data

#' @param x the path to the file

#' @export
readmetadata <- function(x){
    mymetaData<- utils::read.table(x,
                            header = TRUE, sep = "\t", stringsAsFactors = FALSE)
    return(mymetaData)
}

#' MegaR analysis
#' @param x the path to the file
#' @export
readmydata <- function(x){
    success <- try(otufromqiime <- biomformat::read_biom(x), silent = TRUE)
    is.error <- function(x) inherits(x, "try-error")

    if (is.error(success)==TRUE){
        qiimedatamatrix <- utils::read.table(x,header = TRUE, sep= "\t",
                                             row.names = 1,
                                      quote = "", stringsAsFactors = FALSE)
        colnames(qiimedatamatrix) <- stringr::str_remove(colnames(
            qiimedatamatrix), "X")
    }
    else if(is.error(success)==FALSE){
        qiimedata <- biomformat::read_biom(x)
        qiimedatamatrix <- as.data.frame(as.matrix(biomformat::biom_data(
            qiimedata)))
        rownames(qiimedatamatrix) <- make.names(
            gsub(" ", "", apply(biomformat::observation_metadata(qiimedata), 1,
                                function(x)
                                {paste(x, collapse="|")}), fixed = TRUE),
            unique = TRUE)
    }
    return(qiimedatamatrix)
}


