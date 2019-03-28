library(randomForest)
gettrainingdonerf <- function(mytable3, classid, sampleid, ruleout, psd, metadat){
  otu_table_scaled <- mytable3
  otu_table_scaled_state <- data.frame(t(otu_table_scaled))  
  #otu_table_scaled_state$country <- mymetadata()[,input$classid][match(str_remove(rownames(otu_table_scaled_state), "_profile"), mymetadata()[,input$sampleid])]  
  otu_table_scaled_state$country <- metadat[,classid][match(str_remove(rownames(otu_table_scaled_state), "_.*"), metadat[,sampleid])]
  otu_table_scaled_state <- na.omit(otu_table_scaled_state)
  otu_table_scaled_state <- otu_table_scaled_state[otu_table_scaled_state$country != ruleout,]
  otu_table_scaled_state1 <-droplevels( otu_table_scaled_state)
  
  set.seed(60)
  smp_size <- floor((psd/100) * nrow(otu_table_scaled_state1))
  train_ind <- sample(seq_len(nrow(otu_table_scaled_state1)), size = smp_size)
  train <- otu_table_scaled_state1[train_ind, ]
  train<-droplevels(train)
  test <- otu_table_scaled_state1[-train_ind,]
  RF_state_classify <- randomForest::randomForest(as.factor(country)~. , data =train, importance = T, proximities = T)
  return(list(train, test, RF_state_classify))
}
