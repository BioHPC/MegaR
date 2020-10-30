#' MegaR validation
#'
#' This function conducts 10 fold cross validation on N set of data
#' @param Num No. of sets to run for validation
#' @param modelclas one of the model, randomforest, supportvector machine or
#' generalized linear model
#' @param mytable3 processed input file with features
#' @param classid the column number in metadata file in which the class of
#' input data is stored
#' @param ruleout the class which is to be removed from classification model
#' @param sampleid the column number of metadata file which contain sample ids
#'  that match with input data
#' @param psd the percentage of data to be split into training set
#' @param metadat the metadata path
#'
#' @export

validation<-function(Num,modelclas,mytable3,classid,sampleid,ruleout,psd,
                     metadat, optparam){
    otu_table_scaled <- mytable3
    otu_table_scaled_state <- data.frame(t(otu_table_scaled))
    otu_table_scaled_state$country <- metadat[,classid][match(
        rownames (otu_table_scaled_state),
        metadat[,sampleid])]
    otu_table_scaled_state <- stats::na.omit(otu_table_scaled_state)
    otu_table_scaled_state$country <- factor(otu_table_scaled_state$country, levels = ruleout) # add this lin
    otu_table_scaled_state1 <- stats::na.omit(droplevels( otu_table_scaled_state))
    fit_control <- caret::trainControl(method = "LOOCV")
    progress <- shiny::Progress$new(style = 'notification')
    progress$set(message = "Working...",value = 0)
    Acc3<- NULL
    Kpp3 <- NULL
    for (i in 1:Num) {
        smp_size <- floor((psd/100) * nrow(otu_table_scaled_state1))
        train_ind <- sample(seq_len(nrow(otu_table_scaled_state1)),
                            size = smp_size)
        mtrain <- otu_table_scaled_state1[train_ind, ]
        #mtrain<-droplevels(mtrain)
        test <- otu_table_scaled_state1[-train_ind,]
        if(modelclas == "RF"){
            #RF_state_classify <- randomForest::randomForest(
             #   as.factor(country)~. ,data =train,importance = T,proximities=T)
            RF_state_classify_loocv <- caret::train(
                as.factor(country)~. , data = mtrain,method="rf",ntree= 501,
                 .mtry = optparam, trControl=fit_control)
            Acc3[i] <- RF_state_classify_loocv$results$Accuracy
            Kpp3[i] <- RF_state_classify_loocv$results$Kappa
            #optparam
        }
        else if(modelclas == "svmmodel"){
            RF_state_classify_loocv <- caret::train(as.factor(country)~. ,
                                             data =mtrain , method="svmLinear",
                                             trControl=fit_control, .C=optparam)
            Acc3[i] <- RF_state_classify_loocv$results$Accuracy
            Kpp3[i] <- RF_state_classify_loocv$results$Kappa
        }
        else {
            RF_state_classify_loocv <- caret::train(as.factor(country)~. ,
                                             data =mtrain , method="glm"  ,
                                             trControl=fit_control)
            Acc3[i] <- RF_state_classify_loocv$results$Accuracy
            Kpp3[i] <- RF_state_classify_loocv$results$Kappa
        }
        progress$inc(i/Num, detail=paste("Validation set", i+1))
    }
    on.exit(progress$close())
    sprintf("The 10 fold cross validated obtained from the average of %i
            independent run is  %f. ", Num , sum(Acc3)/Num )
}
