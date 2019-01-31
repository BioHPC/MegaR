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