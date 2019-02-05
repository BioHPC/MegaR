readmetadata <- function(x){
  mymetaData<- read.table(x,
                          header = TRUE, sep = "\t", stringsAsFactors = FALSE)
  return(mymetaData)
}

readmydata<- function(x){
  success <- try(otufromqiime <- read_biom(x), silent = TRUE)
  is.error <- function(x) inherits(x, "try-error")
  
  if (is.error(success)==TRUE){
    qiimedatamatrix <- read.table(x,header = TRUE, sep= "\t", row.names = 1, quote = "", stringsAsFactors = FALSE)
  }
  else if(is.error(success)==FALSE){
    qiimedata <- read_biom(x)
    qiimedatamatrix <- as.data.frame(as.matrix(biom_data(qiimedata)))
    rownames(qiimedatamatrix) <- make.names(gsub(" ", "", apply(observation_metadata(qiimedata), 1,
                                                                function(x) {paste(x, collapse="|")}), fixed = TRUE), unique = TRUE)
  }
  return(qiimedatamatrix)
}
getLevelData <- function(alltable, leveld){
  #if data is biomfile
  if(all(grepl("s__", rownames(alltable))) == TRUE){
    if (leveld == "Genus Level"){
      alltable$genus <- str_remove(rownames(alltable), "s__.*")
      x <- aggregate( . ~ genus, alltable, sum)
      rownames(x) <- x$genus
      x <- x[,-1]
      return(x)
    }
    else{
      return(alltable)
    }
  }
  else if (any(grepl("t__", rownames(alltable))) == TRUE){##normal metaphlan
    if (leveld == "Genus Level"){
      x <- grep("s__.*"   , rownames(alltable),invert = T, value = T)
      y <- grep("g__.*" , x, value = TRUE)
    }
    else{
      x <- grep("t__.*"   , rownames(alltable),invert = T, value = T)
      y <- grep("s__.*" , x, value = TRUE)
    }
    new_count_table <- alltable[match(y, rownames(alltable)),]
    return(new_count_table)
  }
  else{
    if (leveld == "Genus Level"){
      x <- grep("s__.*"   , rownames(alltable),invert = T, value = T)
      y <- grep("g__.*" , x, value = TRUE)
    }
    else{
      x <- grep("[0-9].*"   , rownames(alltable),invert = T, value = T)
      y <- grep("s__.*" , x, value = TRUE)
    }
    new_count_table <- alltable[match(y, rownames(alltable)),]
    return(new_count_table)
  }
}





