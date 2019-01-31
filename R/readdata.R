
readmetaphlan <- function(x){
  metaphlan<- read.table(x,
                         header = TRUE, sep = "\t", row.names = 1, quote = "", stringsAsFactors = FALSE)
  return(metaphlan)
}


readmetadata <- function(x){
  mymetaData<- read.table(x,
                          header = TRUE, sep = "\t", stringsAsFactors = FALSE)
  return(mymetaData)
}

readbiom <- function(x){
  #try(qiimedtaraw <- read_biom(x), silent = TRUE )
  qiimedtaraw <- read.table(x,
                         header = TRUE, sep = "\t", row.names = 1, quote = "", stringsAsFactors = FALSE)
  return(qiimedtaraw)
}