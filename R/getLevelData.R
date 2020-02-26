#' MegaR getLevelData
#'
#' This is an internal function used to extract either the species or genus
#' level of information
#' @param alltable the taxonomic table
#' @param leveld the taxonomic level at which to select the feature
#' @export

getLevelData <- function(alltable, leveld){
    if (leveld == "All Level") {
        if(all(grepl("s__", rownames(alltable))) == TRUE){ #biom file
            genus <-  stringr::str_remove(rownames(alltable), ".[0-9]+$")
            alltable$all <- stringr::str_remove(
                genus,
                ".s__$|.g__.s__$|.g__.s__$|.f__.g__.s__$|.o__.f__.g__.s__$")
            x <- stats::aggregate( . ~ all, alltable, sum )
            rownames(x) <- x$all
            x <- x[-1]
            return(x)
        }
        else{
            tble <- NULL
            ##unclassified family## all classified
            x <- grep("g__.*"   , rownames(alltable),invert = T, value = T)
            y <- grep("f__.*" , x, value = TRUE)
            new_count_table <- alltable[match(y, rownames(alltable)),]
            family_unclass <-grep("unclass", rownames(new_count_table),value=T)
            if (length(family_unclass) != 0){
                fmily <- alltable[match(family_unclass, rownames(alltable)), ]
                tble <- rbind(fmily)
            }

            x <- grep("s__.*"   , rownames(alltable),invert = T, value = T)
            y <- grep("g__.*" , x, value = TRUE)
            new_count_table <- alltable[match(y, rownames(alltable)),]
            genus_unclass<- grep("unclass", rownames(new_count_table),value = T)

            if(length(genus_unclass)!= 0){
                genus <- alltable[match(genus_unclass, rownames(alltable)), ]
                tble <- rbind(genus, tble)
            }

            x <- grep("t__.*"   , rownames(alltable),invert = T, value = T)
            y <- grep("s__.*" , x, value = TRUE)
            new_count_table <- alltable[match(y, rownames(alltable)),]
            speci_unclass<-grep("unclass", rownames(new_count_table), value = T)
            if(length(speci_unclass)!=0){
                species <- alltable[match(speci_unclass, rownames(alltable)), ]
                tble <- rbind(tble, species)}

            #x <- grep("t__.*"   , rownames(alltable),invert = T, value = T)
            y <- grep("t__" , rownames(alltable), value = TRUE)
            alltype <- alltable[match(y, rownames(alltable)),]

            tble <- rbind(tble, alltype)
        }
    }
    else{
        #if data is biomfile
        if(all(grepl("s__", rownames(alltable))) == TRUE){
            if (leveld == "Species Level"){
                alltable$species <- stringr::str_remove(rownames(alltable),
                                                        ".[0-9]+$")
                x <- stats::aggregate( . ~ species, alltable, sum)
                rownames(x) <- x$species
                x <- x[grep ("s__[a-z]", ignore.case = TRUE, rownames(x)),]
                x <- x[-1]
                return(x)
            }
            else{
                alltable$genus <- stringr::str_remove(rownames(alltable),
                                                      "s__.*")
                x <- stats::aggregate( . ~ genus, alltable, sum)
                rownames(x) <- x$genus
                x <-x[grep ("g__[a-z]", ignore.case = T, rownames(x)),]
                x<- x[-1]
                return(x)
            }
        }
        else if (any(grepl("t__", rownames(alltable))) == TRUE){
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
                new_count_table <- alltable[match(y, rownames(alltable)),]
                new_count_table<-new_count_table[grep("g__unclassified",
                                                rownames(new_count_table),
                                                invert = TRUE),]
            }
            else{
                x <- grep("[0-9]+$"   , rownames(alltable),invert = T, value =T)
                y <- grep("s__.*" , x, value = TRUE)
                new_count_table <- alltable[match(y, rownames(alltable)),]
                new_count_table<-new_count_table[grep("s__unclassified",
                                                      rownames(new_count_table),
                                                      invert = TRUE),]
            }
            return(new_count_table)
        }
    }
}
