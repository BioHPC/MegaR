#' MegaR gettrainingdonerf
#' @param mytable3 processed input file with features
#' @param classid the column number in metadata file in which the class of
#' input data is stored
#' @param ruleout the class which is to be removed from classification model
#' @param sampleid the column number of metadata file which contain sample ids
#'  that match with input data
#' @param psd the percentage of data to be split into training set
#' @param metadat the metadata path
#'
#'  @export
gettrainingdonerf <- function(mytable3, classid, sampleid,ruleout,psd,metadat,mrange){
    otu_table_scaled <- mytable3
    otu_table_scaled_state <- data.frame(t(otu_table_scaled))
    otu_table_scaled_state$country <- metadat[,classid][match(
        stringr::str_remove(rownames(otu_table_scaled_state), "_.*"),
        metadat[,sampleid])]
    otu_table_scaled_state <- stats::na.omit(otu_table_scaled_state)
    otu_table_scaled_state$country <- factor(otu_table_scaled_state$country, levels = ruleout) # add this lin
    otu_table_scaled_state1 <- stats::na.omit(droplevels(otu_table_scaled_state))
    set.seed(60)
    smp_size <- floor((psd/100) * nrow(otu_table_scaled_state1))
    train_ind <- sample(seq_len(nrow(otu_table_scaled_state1)), size = smp_size)
    train <- otu_table_scaled_state1[train_ind, ]
    train<-droplevels(train)
    test <- otu_table_scaled_state1[-train_ind,]
    tunegrid <- expand.grid(.mtry=c(mrange[[1]]:mrange[[2]]))
    rf_gridsearch <- caret::train(as.factor(country)~., data=train, method="rf",
                           tuneGrid=tunegrid, trControl = caret::trainControl(savePredictions = T, classProbs = T, verboseIter = T))
    
    return(list(train, test, rf_gridsearch))
}