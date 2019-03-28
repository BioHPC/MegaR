#' megaR analysis
#' This is internal function to read data 

#' @param x the path to the file 

readmetadata <- function(x){
  mymetaData<- read.table(x,
                          header = TRUE, sep = "\t", stringsAsFactors = FALSE)
  return(mymetaData)
}

readmydata <- function(x){
  success <- try(otufromqiime <- read_biom(x), silent = TRUE)
  is.error <- function(x) inherits(x, "try-error")
  
  if (is.error(success)==TRUE){
    qiimedatamatrix <- read.table(x,header = TRUE, sep= "\t", row.names = 1, quote = "", stringsAsFactors = FALSE)
    colnames(qiimedatamatrix) <- str_remove(colnames(qiimedatamatrix), "X")
  }
  else if(is.error(success)==FALSE){
    qiimedata <- read_biom(x)
    qiimedatamatrix <- as.data.frame(as.matrix(biom_data(qiimedata)))
    rownames(qiimedatamatrix) <- make.names(gsub(" ", "", apply(observation_metadata(qiimedata), 1,
                                                                function(x) {paste(x, collapse="|")}), fixed = TRUE), unique = TRUE)
  }
  return(qiimedatamatrix)
}


