getLevelData <- function(alltable, leveld){
  
  if (leveld == "All Level") {
    
    tble <- NULL
    ##unclassified family## all classified
    x <- grep("g__.*"   , rownames(alltable),invert = T, value = T)
    y <- grep("f__.*" , x, value = TRUE)
    new_count_table <- alltable[match(y, rownames(alltable)),]
    family_unclass <-grep("unclass", rownames(new_count_table), value = T)
    if (length(family_unclass) != 0){                       ## no unclassified
      fmily <- alltable[match(family_unclass, rownames(alltable)), ]
      tble <- rbind(fmily)
    }
    
    ## unclassified genus # 2 unclassified
    x <- grep("s__.*"   , rownames(alltable),invert = T, value = T)
    y <- grep("g__.*" , x, value = TRUE)
    new_count_table <- alltable[match(y, rownames(alltable)),]
    genus_unclass<- grep("unclass", rownames(new_count_table), value = T)
    
    if(length(genus_unclass)!= 0){
      genus <- alltable[match(genus_unclass, rownames(alltable)), ]
      tble <- rbind(genus, tble)
    }
    
    ## unclassified species # 48 unclassififed
    x <- grep("t__.*"   , rownames(alltable),invert = T, value = T)
    y <- grep("s__.*" , x, value = TRUE)
    new_count_table <- alltable[match(y, rownames(alltable)),]
    speci_unclass<-grep("unclass", rownames(new_count_table), value = T)
    if(length(speci_unclass)!=0){
      species <- alltable[match(speci_unclass, rownames(alltable)), ]
      tble <- rbind(tble, species)}
    
    ### all type # 309 type
    #x <- grep("t__.*"   , rownames(alltable),invert = T, value = T)
    y <- grep("t__" , rownames(alltable), value = TRUE)
    alltype <- alltable[match(y, rownames(alltable)),]
    
    #359 features
    tble <- rbind(tble, alltype)
  }
  else{
  
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
}
