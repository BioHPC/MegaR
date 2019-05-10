

validation <- function(N,  modelclas ,mytable3, classid, sampleid, ruleout, psd, metadat ){  
  otu_table_scaled <- mytable3
  otu_table_scaled_state <- data.frame(t(otu_table_scaled))  
  #otu_table_scaled_state$country <- mymetadata()[,input$classid][match(str_remove(rownames(otu_table_scaled_state), "_profile"), mymetadata()[,input$sampleid])]  
  otu_table_scaled_state$country <- metadat[,classid][match(rownames(otu_table_scaled_state), metadat[,sampleid])]
  otu_table_scaled_state <- na.omit(otu_table_scaled_state)
  otu_table_scaled_state <- otu_table_scaled_state[otu_table_scaled_state$country != ruleout,]
  otu_table_scaled_state1 <-droplevels( otu_table_scaled_state)
  fit_control <- caret::trainControl(method = "LOOCV")
  
  Acc3<- NULL
  Kpp3 <- NULL
  for (i in 1:N) {
    smp_size <- floor((psd/100) * nrow(otu_table_scaled_state1))
    train_ind <- sample(seq_len(nrow(otu_table_scaled_state1)), size = smp_size)
    train <- otu_table_scaled_state1[train_ind, ]
    train<-droplevels(train)
    test <- otu_table_scaled_state1[-train_ind,]
    if(modelclas == "rfmodel"){
      RF_state_classify <- randomForest::randomForest(as.factor(country)~. , data =train, importance = T, proximities = T)
      RF_state_classify_loocv <- train(as.factor(country)~. , data =train , method="rf",ntree= 501,  tuneGrid=data.frame( mtry=RF_state_classify$mtry ), trControl=fit_control)
      Acc3[i] <- RF_state_classify_loocv$results$Accuracy
      Kpp3[i] <- RF_state_classify_loocv$results$Kappa
    }
    else if(modelclas == "svmmodel"){
      RF_state_classify_loocv <- train(as.factor(country)~. , data =train , method="svmLinear", trControl=fit_control)
      Acc3[i] <- RF_state_classify_loocv$results$Accuracy
      Kpp3[i] <- RF_state_classify_loocv$results$Kappa
    }
    else {
      RF_state_classify_loocv <- train(as.factor(country)~. , data =train , method="glm"  , trControl=fit_control)
      Acc3[i] <- RF_state_classify_loocv$results$Accuracy
      Kpp3[i] <- RF_state_classify_loocv$results$Kappa
    }
  }
  sprintf("The 10 fold cross validated obtained from the average of %i independent run is  %f. ", N , sum(Acc3)/N )
}
