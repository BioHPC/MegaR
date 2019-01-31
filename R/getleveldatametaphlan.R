getLevelDataM <- function(alltable, leveld){ # 
  
  if (leveld == "Genus Level"){
    
    x <- grep("s__.*"   , rownames(alltable),invert = T, value = T)
    y <- grep("g__.*" , x, value = TRUE)
  }
  else{
    # x <- grep("[0-9].*"   , rownames(alltable),invert = T, value = T) ## if qiime otu
    x <- grep("t__.*"   , rownames(alltable),invert = T, value = T)
    y <- grep("s__.*" , x, value = TRUE)
  }
  new_count_table <- alltable[match(y, rownames(alltable)),]
  return(new_count_table)
}
########### if qiime biom
if(FALSE){
getLevelDataQ <- function(biomfile, level){
  
  if (level == "Species Level"){
    return(as.data.frame(as.matrix(biom_data(biomfile))))
  }
  else{
    b<- as.data.frame(as.matrix(biom_data(biomfile)))
    taxonomy <- gsub(" ", "", apply(observation_metadata(biomfile), 1, function(x) {paste(x, collapse="|")}), fixed = TRUE)
    z<- make.names(taxonomy, unique = T)
    b$genus <- str_remove(z, "s__.*")
    x <- aggregate(.~genus, b, sum)
    rownames(x) <- x$genus
    x<- x[,-1]
    return(x)
  }
}
}
