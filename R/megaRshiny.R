##check prediction
library(shiny)
library(shinythemes)
library(shinydashboard)
library(randomForest)
library(stringr)
library(plyr)
library(ggplot2)
library(RColorBrewer)
library(biomformat)
options(warn=-1)
source("glm.R")
source("getgoodfea.R")
source("getleveldata.R")
source("training.R")
source("plotconfumat.R")
source("readdata.R")
source("prediction.R")
source("plottopimptfeat.R")
source("svmmodeltrain.R")
options(shiny.maxRequestSize=30*1024^2)

ui <- dashboardPage(
  dashboardHeader(disable = TRUE),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Metaphlan", tabName = "metaphlan", icon = icon("dashboard")),
      menuItem("Qiime", tabName = "qiime", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "metaphlan",
              fluidPage(theme =shinytheme("flatly"), 
                        navbarPage("MegaR",id= "inTabsetm",
                                   tabPanel("DATA INPUT", 
                                            fluidRow(sidebarLayout(sidebarPanel(fileInput(inputId = "file1otutable", label = "COUNT TABLE", multiple = FALSE)
                                            ),
                                            mainPanel(type = "tab",tabsetPanel(
                                              tabPanel("Data", tableOutput("mdataTbl")) #,
                                              #tabPanel("G_Heatmap", plotOutput("mgenus")),
                                              #tabPanel("S_Heatmap", plotOutput("mspecies"))
                                              
                                            ))
                                            ))
                                   ),
                                   tabPanel("Preprocessing", 
                                            fluidRow(sidebarLayout(sidebarPanel(
                                              radioButtons("level", "Criteria for feature selection",choices = c("Genus Level", "Species Level"),selected = "Genus"),
                                              numericInput("threshold", "THRESHOLD",min= 0, max = 100,  step = 0.001, value = 0.003),
                                              sliderInput("samplePercent", "Percentage of Sample", min = 0 , max = 100 , step = 1, value=5),
                                              helpText("OTU that have less than the the threshold value in given percentage of sample are removed."),
                                              radioButtons('norm' ,"Normalization", choices = c(YES = "css"  , NO = "none"), selected = "")
                                              #actionButton("run", "Run preprocessing")
                                            ),
                                            mainPanel(tabsetPanel(tabPanel("Data", tableOutput("mGoodTbl"))))
                                            ))
                                   ),
                                   navbarMenu("Model building",
                                              tabPanel("GLM",
                                                       sidebarLayout(sidebarPanel (
                                                         fileInput(inputId = "gmyresponseVector", label = "Please input the class vector", multiple = FALSE),
                                                         numericInput("gclassid", label = "column number for classid", min = 1, max = 100, value =8),
                                                         numericInput("gsampleid", label = "column number for sampleid", min = 1, max = 100, value =53),
                                                         numericInput("gpsd", "percentage of data in training", min = 60, max = 100, value = 90),
                                                         textInput("gruleout", label = "Text input", value = "EST")
                                                         
                                                       ), mainPanel(tabsetPanel(tabPanel("Error Rate",verbatimTextOutput("gAOC")),#uiOutput("gdownload1")), #downloadButton(outputId = "down", label = "Download the plot")),
                                                                                tabPanel("Confusion Matrix", imageOutput("gmyconfusionMatrix")),#, uiOutput("gdownload2")),#uiOutput(downloadButton(outputId = "confudown", label = "Download the plot")),
                                                                                tabPanel("Important feature", imageOutput("gimptFeature"))#,uiOutput("gdownload3"))
                                                       )))),
                                              tabPanel("Random Forest",
                                                       sidebarLayout(sidebarPanel (
                                                         fileInput(inputId = "myresponseVector", label = "Please input the class vector", multiple = FALSE),
                                                         numericInput("classid", label = "column number for class info", min = 1, max = 100, value =8),
                                                         numericInput("sampleid", label = "column number for sampleid", min = 1, max = 100, value =53),
                                                         numericInput("psd", "percentage of data in training", min = 60, max = 100, value = 90),
                                                         textInput("ruleout", label = "Remove class", value = "EST"),
                                                         tags$hr(),
                                                         actionButton("bltModel", "Build model")
                                                         #numericInput("mtry", "Number of predictor varibale to try at each split" ,value =5),
                                                         #numericInput("ntrees", "Number of trees to build the model", value = 10),
                                                         #helpText("Increasing number of tree increase robustness, decreasing number of mtry increase roustness?")
                                                       ),
                                                       mainPanel(tabsetPanel(tabPanel("Error Rate", imageOutput("AOC"),uiOutput("download1")), #downloadButton(outputId = "down", label = "Download the plot")),
                                                                             tabPanel("Confusion Matrix", imageOutput("myconfusionMatrix"), uiOutput("download2")),#uiOutput(downloadButton(outputId = "confudown", label = "Download the plot")),
                                                                             tabPanel("Important feature", imageOutput("imptFeature"),   uiOutput("download3")) #, output="imptfeat", label = "Download the plot"))
                                                       )
                                                       )
                                                       )
                                              ),
                                              tabPanel("SVM",
                                                       sidebarLayout(sidebarPanel (
                                                         fileInput(inputId = "smyresponseVector", label = "Please input the class vector", multiple = FALSE),
                                                         numericInput("sclassid", label = "column number for classid", min = 1, max = 100, value =8),
                                                         numericInput("ssampleid", label = "column number for sampleid", min = 1, max = 100, value =53),
                                                         numericInput("spsd", "percentage of data in training", min = 60, max = 100, value = 90),
                                                         textInput("sruleout", label = "Text input", value = "EST"),
                                                         textInput("svmtd", label = "SVM Method", value = "svmLinear")
                                                         ), mainPanel(tabsetPanel(tabPanel("Error Rate",verbatimTextOutput("sAOC"),uiOutput("sdownload1")), #downloadButton(outputId = "down", label = "Download the plot")),
                                                                                  tabPanel("Confusion Matrix", imageOutput("smyconfusionMatrix"), uiOutput("sdownload2")),#uiOutput(downloadButton(outputId = "confudown", label = "Download the plot")),
                                                                                  tabPanel("Important feature", imageOutput("simptFeature"),uiOutput("sdownload3"))
                                                         ))))
                                              
                                   ),
                                   tabPanel("Validate",
                                            "Do validation here"),
                                   navbarMenu("Prediction", 
                                              tabPanel("Use most recent model",
                                                       sidebarLayout(sidebarPanel(
                                                         fileInput(inputId = "unknw", label= "Please input your unknown dataset", multiple= FALSE)
                                                       ),
                                                       mainPanel(tabsetPanel(tabPanel("Prediction", tableOutput("Preresult"))
                                                                             # tabPanel("Statistics", textOutput("PredictionStatistics")),
                                                                             # tabPanel("PLOTS", imageOutput("results"))
                                                       )))),
                                              tabPanel("Use default model")
                                   ))
                        
                        
                        
              )),
      tabItem(tabName = "qiime",
              fluidPage(theme =shinytheme("flatly"), 
                        navbarPage("MegaR",id= "qinTabsetm",
                                   tabPanel("DATA INPUT", 
                                            fluidRow(sidebarLayout(sidebarPanel(fileInput(inputId = "qfile1otutable", label = "BIOM FILE", multiple = FALSE)
                                            ),
                                            mainPanel(type = "tab",tabsetPanel(
                                              tabPanel("Data", tableOutput("qmdataTbl")) #,
                                              #tabPanel("G_Heatmap", plotOutput("mgenus")),
                                              #tabPanel("S_Heatmap", plotOutput("mspecies"))
                                              
                                            ))
                                            ))
                                   ),
                                   tabPanel("Preprocessing", 
                                            fluidRow(sidebarLayout(sidebarPanel(
                                              radioButtons("qlevel", "Criteria for feature selection",choices = c("Genus Level", "Species Level"), selected = "Genus"),
                                              numericInput("qthreshold", "THRESHOLD",min= 0, max = 100,  step = 0.001, value = 0.003),
                                              sliderInput("qsamplePercent", "Percentage of Sample", min = 0 , max = 100 , step = 1, value=5),
                                              radioButtons('qnorm' ,"Normalization", choices = c(YES = "css"  , NO = "none"), selected = ""),
                                              helpText("OTU that have less than the the threshold value in given percentage of sample are removed.")
                                            ),
                                            mainPanel(tabsetPanel(tabPanel("Data", tableOutput("qmGoodTbl"))))
                                            ))
                                   ),
                                   navbarMenu("Model building",
                                              tabPanel("GLM",
                                                       sidebarLayout(sidebarPanel (
                                                         fileInput(inputId = "qgmyresponseVector", label = "Please input the class vector", multiple = FALSE),
                                                         numericInput("qgclassid", label = "column number for classid", min = 1, max = 100, value =1),
                                                         numericInput("qgsampleid", label = "column number for sampleid", min = 1, max = 100, value =66),
                                                         numericInput("qgpsd", "percentage of data in training", min = 60, max = 100, value = 90),
                                                         textInput("qgruleout", label = "Text input", value = "control")
                                                         
                                                       ), mainPanel(tabsetPanel(tabPanel("Error Rate",verbatimTextOutput("qgAOC")),#,uiOutput("qgdownload1")), #downloadButton(outputId = "down", label = "Download the plot")),
                                                                                tabPanel("Confusion Matrix", imageOutput("qgmyconfusionMatrix"), uiOutput("qgdownload2")),#uiOutput(downloadButton(outputId = "confudown", label = "Download the plot")),
                                                                                tabPanel("Important feature", imageOutput("qgimptFeature"),uiOutput("qgdownload3"))
                                                       )))),
                                              tabPanel("Random Forest",
                                                       sidebarLayout(sidebarPanel (
                                                         fileInput(inputId = "qmyresponseVector", label = "Please input the class vector", multiple = FALSE),
                                                         numericInput("qclassid", label = "column no. for classid", min = 1, max = 100, value =7),
                                                         numericInput("qsampleid", label = "column no. for sampleid", min = 1, max = 100, value =2),
                                                         numericInput("qpsd", "% of data in training", min = 60, max = 100, value = 90),
                                                         textInput("qruleout", label = "Class to remove", value = "control"),
                                                         tags$hr(),
                                                         actionButton("qbltModel", "Build model")
                                                         
                                                         #numericInput("mtry", "Number of predictor varibale to try at each split" ,value =5),
                                                         #numericInput("ntrees", "Number of trees to build the model", value = 10),
                                                         #helpText("Increasing number of tree increase robustness, decreasing number of mtry increase roustness?")
                                                       ),
                                                       mainPanel(tabsetPanel(tabPanel("Error Rate", imageOutput("qAOC"),uiOutput("qdownload1")),# downloadButton(outputId = "qdown", label = "Download the plot")),
                                                                             tabPanel("Confusion Matrix", imageOutput("qmyconfusionMatrix"),uiOutput("qdownload2")),#downloadButton(outputId = "qconfudown", label = "Download the plot")),
                                                                             tabPanel("Important feature", imageOutput("qimptFeature"),  uiOutput("qdownload3"))# downloadButton(output="qimptfeat", label = "Download the plot"))
                                                       )
                                                       )
                                                       )
                                              ),
                                              tabPanel("SVM", sidebarLayout(sidebarPanel (
                                                fileInput(inputId = "qsmyresponseVector", label = "Please input the class vector", multiple = FALSE),
                                                numericInput("qsclassid", label = "column number for classid", min = 1, max = 100, value =8),
                                                numericInput("qssampleid", label = "column number for sampleid", min = 1, max = 100, value =53),
                                                numericInput("qspsd", "percentage of data in training", min = 60, max = 100, value = 90),
                                                textInput("qsruleout", label = "Text input", value = "EST"),
                                                textInput("qsvmtd", label = "SVM Method", value = "svmLinear")
                                              ), mainPanel(tabsetPanel(tabPanel("Error Rate",verbatimTextOutput("qsAOC"),uiOutput("qsdownload1")), #downloadButton(outputId = "down", label = "Download the plot")),
                                                                       tabPanel("Confusion Matrix", imageOutput("qsmyconfusionMatrix"), uiOutput("qsdownload2")),#uiOutput(downloadButton(outputId = "confudown", label = "Download the plot")),
                                                                       tabPanel("Important feature", imageOutput("qsimptFeature"),uiOutput("qsdownload3"))
                                              ))))
                                              
                                   ),
                                   tabPanel("Validate",
                                            "Do validation here"),
                                   navbarMenu("Prediction", 
                                              tabPanel("Use most recent model",
                                                       sidebarLayout(sidebarPanel(
                                                         fileInput(inputId = "qunknw", label= "Please input your unknown dataset", multiple= FALSE)
                                                       ),
                                                       mainPanel(tabsetPanel(tabPanel("Prediction", tableOutput("qPreresult"))
                                                                             # tabPanel("Statistics", textOutput("PredictionStatistics")),
                                                                             # tabPanel("PLOTS", imageOutput("results"))
                                                       )))),
                                              tabPanel("Use default model")
                                   ))
              ))
    )))

server <- function(input, output, session){
  ############## function to generate specific taxon level data ######################
  myLevelData <- reactive({
    return(getLevelData(readmydata(input$file1otutable$datapath), input$level))
  })
 
  myGoodfeature <- reactive({
    return(getGoodfeature(myLevelData(), input$threshold, input$samplePercent, input$norm))
  })
  
  myrfmodel <- eventReactive(input$bltModel, {
    a<-gettrainingdonerf(myGoodfeature(), input$classid, input$sampleid, input$ruleout, input$psd,readmetadata(input$myresponseVector$datapath))
    return(a)
  })
  smyrfmodel <- reactive({
    a<-gettrainingdonesvm(myGoodfeature(), input$sclassid, input$ssampleid, input$sruleout, input$spsd,readmetadata(input$smyresponseVector$datapath), input$svmtd)
    return(a)
  })
  
  gmyrfmodel <- reactive({
    a<-gettrainingdoneglm(myGoodfeature(), input$gclassid, input$gsampleid, input$gruleout, input$gpsd,readmetadata(input$gmyresponseVector$datapath), input$gvmtd)
    return(a)
  })
  output$mdataTbl <- renderTable({
    req(input$file1otutable$datapath)
    return(readmetaphlan(input$file1otutable$datapath))
  }, rownames = TRUE)
  
  output$mGoodTbl <- renderTable({
    req(input$norm)
    myGoodfeature()
  }, rownames =  T)
  
  
  output$download1 <- renderUI({
    if(!is.null(myrfmodel()[[3]])) {
      downloadButton('down', 'Download Output File')
    }
  })
  output$download2 <- renderUI({
    if(!is.null(myrfmodel())){
      downloadButton('confudown', 'Download Output File')
    }
  })
  output$download3 <- renderUI({
    if(!is.null(myrfmodel())) {
      downloadButton('imptfeat', 'Download Output File')
    }
  })
  output$down <- downloadHandler(
    filename = "iris.pdf",
    content = function(file){
      pdf(file) # open the pdf device
      print(plot(myrfmodel()[[3]])) 
      dev.off()
    }
  )
  output$confudown <- downloadHandler(
    filename = "iris.pdf",
    content = function(file) {
      pdf(file) # open the pdf device
      print( getconfuMat(myrfmodel()[[2]], myrfmodel()[[3]]))
      dev.off()
    }
  )
  output$imptfeat <- downloadHandler(
    filename = "iris.pdf",
    content = function(file) {
      pdf(file) # open the pdf device
      print(plotimportantfeatures(myrfmodel()[[3]], 10))
      dev.off()
    }
  )
  
  
  output$myconfusionMatrix <- renderPlot({
    req(input$myresponseVector$datapath)
    getconfuMat(myrfmodel()[[2]], myrfmodel()[[3]])
  })
  
  output$AOC <- renderPlot({
    req(input$myresponseVector$datapath)
    plot(myrfmodel()[[3]], main= "Error rate during training model")
  })
  output$imptFeature <- renderPlot({
    req(myrfmodel())
    plotimportantfeatures(myrfmodel()[[3]], 10)
  })
  
  output$Preresult <- renderTable({
    req(input$unknw$datapath)
    a<- getGoodfeature(getLevelData(readmydata(input$unknw$datapath),
                                     input$level),input$threshold, input$samplePercent, input$norm)
    rownames(a)<- gsub("|", ".", rownames(a), fixed = TRUE)
    getunknpredict(a,myrfmodel())
  }, rownames= TRUE)
  
  #######################################
  ##### metaphlan svm ###### See if download button is required
 
  output$smyconfusionMatrix <- renderPlot({
    req(input$smyresponseVector$datapath)
    getconfuMat(smyrfmodel()[[2]], smyrfmodel()[[3]])
  })
  
  output$sAOC <- renderPrint({
    req(input$smyresponseVector$datapath)
    smyrfmodel()[[3]]#, smyrfmodel()[[1]])
  })
  
###########################################
### metaphlan glm
  output$gmyconfusionMatrix <- renderPlot({
    req(input$gmyresponseVector$datapath)
    getconfuMat(gmyrfmodel()[[2]], gmyrfmodel()[[3]])
  })
  
  output$gAOC <- renderPrint({
    req(input$gmyresponseVector$datapath)
    gmyrfmodel()[[3]]#, smyrfmodel()[[1]])
  })
  
  
  
  ########################################################
  ####qiime
  #######################################################
  qmydata <-reactive({
    return( readmydata(input$qfile1otutable$datapath))
  })
  
  qmyLevelData <-reactive({
    return(getLevelData(qmydata(), input$qlevel) )
  })
  
  qmyGoodfeature <- reactive({
    return(getGoodfeature(qmyLevelData(), input$qthreshold, input$qsamplePercent, input$qnorm))
  })
  #Model development
  ##############################################
  qmyrfmodel <- eventReactive(input$qbltModel, {
    a<-gettrainingdonerf(qmyGoodfeature(), input$qclassid, input$qsampleid, input$qruleout, input$qpsd,readmetadata(input$qmyresponseVector$datapath))
    return(a)
  })
  qsmyrfmodel <- reactive({
    a<-gettrainingdonesvm(qmyGoodfeature(), input$qsclassid, input$qssampleid, input$qsruleout, input$qspsd,readmetadata(input$qsmyresponseVector$datapath))
    return(a)
  })
  
  qgmyrfmodel <- reactive({
    a<-gettrainingdoneglm(qmyGoodfeature(), input$qgclassid, input$qgsampleid, input$qgruleout, input$qgpsd,readmetadata(input$qgmyresponseVector$datapath), input$qgvmtd)
    return(a)
  })
  
  output$qmdataTbl <- renderTable({
    req(input$qfile1otutable$datapath)
    #return(as.data.frame(as.matrix(biom_data(qmydata()))))
    return(qmydata())
    #a <- c(d=c(1,2,3),b=c(4,5,6)) 
  }, rownames = TRUE)
  
  output$qmGoodTbl <- renderTable({
    req(input$qnorm)
    qmyGoodfeature()
  }, rownames = TRUE)
  
  #########################
  #Random forest output
  output$qAOC <- renderPlot({
    req(qmyrfmodel())
    plot(qmyrfmodel()[[3]])
  })
  output$qmyconfusionMatrix <- renderPlot({
    req(input$qmyresponseVector$datapath)
    getconfuMat(qmyrfmodel()[[2]], qmyrfmodel()[[3]])
  })
  output$qimptFeature <- renderPlot({
    plotimportantfeatures(qmyrfmodel()[[3]], 10)
  })
  
  output$qdownload1 <- renderUI({
    if(!is.null(qmyrfmodel()[[3]])) {
      downloadButton('qdown', 'Download Output File')
    }
  })
  output$qdownload2 <- renderUI({
    if(!is.null(qmyrfmodel())){
      downloadButton('qconfudown', 'Download Output File')
    }
  })
  output$qdownload3 <- renderUI({
    if(!is.null(qmyrfmodel())) {
      downloadButton('qimptfeat', 'Download Output File')
    }
  })
  output$qdown <- downloadHandler(
    filename = "iris.pdf",    
    content = function(file) {
      pdf(file) # open the pdf device
      #plot(x=x(), y=y(), main = "iris dataset plot", xlab = xl(), ylab = yl()) # draw the plot
      print(plot(qmyrfmodel()[[3]])) 
      dev.off()
    }# turn the device off
  )
  
  output$qconfudown <- downloadHandler(
    filename = "iris.pdf",
    content = function(file) {
      pdf(file) # open the pdf device
      # plot(x=x(), y=y(), main = "iris dataset plot", xlab = xl(), ylab = yl()) # draw the plot
      print( getconfuMat(qmyrfmodel()[[2]], qmyrfmodel()[[3]])) 
      dev.off()
    }# turn the device off
  )
  
  output$qimptfeat <- downloadHandler(
    filename = "iris.pdf",
    content = function(file) {
      pdf(file) # open the pdf device
      print(plotimportantfeatures(qmyrfmodel()[[3]], 10))
      dev.off()
    }
  )
 ###########################################
  #Support vector machine
  output$qsAOC <- renderPrint({
    #req(input$qsmyresponseVector$datapath)
    qsmyrfmodel()[[3]]#, smyrfmodel()[[1]])
  })
  
  output$qsmyconfusionMatrix <- renderPlot({
    req(input$qsmyresponseVector$datapath)
    getconfuMat(qsmyrfmodel()[[2]], qsmyrfmodel()[[3]])
  })
  
  #################
  ## GLM 
  output$qgAOC <- renderPrint({
    req(input$qgmyresponseVector$datapath)
    qgmyrfmodel()[[3]]#, smyrfmodel()[[1]])
  })
  
  output$qgmyconfusionMatrix <- renderPlot({
    req(input$qgmyresponseVector$datapath)
    getconfuMat(qgmyrfmodel()[[2]], qgmyrfmodel()[[3]])
  })
  
 output$qPreresult <- renderTable({ 
    req(input$qunknw$datapath)
    a<-getGoodfeature(getLevelData(readmydata(input$qunknw$datapath), input$qlevel), input$qthreshold, input$qsamplePercent, input$qnorm)
    rownames(a) <- paste("X", rownames(a), sep = "")
    getunknpredict(a,qmyrfmodel())
  }, rownames= TRUE)
}

shinyApp(ui, server)