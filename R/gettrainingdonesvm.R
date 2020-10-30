#' MegaR gettrainingdonesvm
#'
#' This is the function to get class information for the input data from the
#' metadata file and build the support vector machines as predictive models.
#' @param mytable3 processed input file with features
#' @param classid the column number in metadata file in which the class of
#' input data is stored
#' @param ruleout the class which is to be removed from classification model
#' @param sampleid the column number of metadata file which contain sample ids
#'  that match with input data
#' @param psd the percentage of data to be split into training set
#' @param metadat the metadata path
#' @param svmmethod one of the many svm method available in caret
#' @export

gettrainingdonesvm <- function(mytable3, classid, sampleid, ruleout, psd,
                               metadat,svmmethod, mrange){
    otu_table_scaled <- mytable3
    otu_table_scaled_state <- data.frame(t(otu_table_scaled))
    otu_table_scaled_state$country <- metadat[,classid][match(
        rownames(otu_table_scaled_state), metadat[,sampleid])]
    otu_table_scaled_state <- stats::na.omit(otu_table_scaled_state)
    otu_table_scaled_state$country <- factor(otu_table_scaled_state$country, levels = ruleout)
    otu_table_scaled_state1 <- stats::na.omit(droplevels( otu_table_scaled_state))
    tunegrid <- expand.grid(.C = seq(mrange[[1]],mrange[[2]], 0.01))
    
    set.seed(60)
    smp_size <- floor((psd/100) * nrow(otu_table_scaled_state1))
    train_ind <- sample(seq_len(nrow(otu_table_scaled_state1)), size = smp_size)
    train <- otu_table_scaled_state1[train_ind, ]
    train<-droplevels(train)
    test <- otu_table_scaled_state1[-train_ind,]
    RF_state_classify <- caret::train(as.factor(country)~. ,
                                      data =train,method = "svmLinear",  tuneGrid = tunegrid,trControl = caret::trainControl(savePredictions = T, classProbs = T, verboseIter = T))
    return(list(train, test, RF_state_classify))
}
