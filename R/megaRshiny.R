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
library(caret)
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
source("validation.R")
options(shiny.maxRequestSize=30*1024^2)

ui <-fluidPage(theme =shinytheme("flatly"), 
                        navbarPage("MegaR",id= "inTabsetm",
                                   tabPanel("Data Input", 
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
                                              radioButtons("level", "Criteria for feature selection",choices = c("Genus Level", "Species Level", "All Level"),selected = "Genus"),
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
                                                         
                                                       ), mainPanel(tabsetPanel(
                                                         tabPanel("Train Error",  verbatimTextOutput("gAOC")), #actionButton("gaplot1", "Plot train error"), 
                                                         tabPanel("Test Error", uiOutput("gmyconfusionMatrix"), actionButton("gaplot2", "Plot test error"), actionButton("gastats2", "Stats of the test error"))
                                                         #tabPanel("Important feature", imageOutput("gimptFeature"))#,uiOutput("gdownload3"))
                                                       )))),
                                              tabPanel("Random Forest",
                                                       sidebarLayout(sidebarPanel (
                                                         fileInput(inputId = "myresponseVector", label = "Please input the class vector", multiple = FALSE),
                                                         numericInput("classid", label = "column number for class info", min = 1, max = 100, value =8),
                                                         numericInput("sampleid", label = "column number for sampleid", min = 1, max = 100, value =53),
                                                         numericInput("psd", "percentage of data in training", min = 60, max = 100, value = 90),
                                                         textInput("ruleout", label = "Remove class", value = "EST"),
                                                         #tags$hr(),
                                                         #actionButton("bltModel", "Build model")
                                                         numericInput("mtry", "Number of predictor varibale to try at each split" ,value =5),
                                                         numericInput("ntrees", "Number of trees to build the model", value = 10),
                                                         helpText("Increasing number of tree increase robustness, decreasing number of mtry increase roustness?")
                                                       ),
                                                       mainPanel(tabsetPanel(tabPanel("Train Error", uiOutput("AOC"),
                                                                                      actionButton("aplot1", "Plot train error"),
                                                                                      uiOutput("download1"), 
                                                                                      actionButton("astats1", "Stats of the train error")), 
                                                                             #tabPanel("Test Error", verbatimTextOutput("myconfusionMatrix"), uiOutput("download2")),
                                                                             tabPanel("Test Error", uiOutput("myconfusionMatrix"), actionButton("aplot2", "Plot test error"), actionButton("astats2", "Stats of the test error")),# 
                                                                             tabPanel("Important feature", imageOutput("imptFeature"),   uiOutput("download3")) #, output="imptfeat", label = "Download the plot"))
                                                       )))
                                                       ),
                                              tabPanel("SVM",
                                                       sidebarLayout(sidebarPanel (
                                                         fileInput(inputId = "smyresponseVector", label = "Please input the class vector", multiple = FALSE),
                                                         numericInput("sclassid", label = "column number for classid", min = 1, max = 100, value =8),
                                                         numericInput("ssampleid", label = "column number for sampleid", min = 1, max = 100, value =53),
                                                         numericInput("spsd", "percentage of data in training", min = 60, max = 100, value = 90),
                                                         textInput("sruleout", label = "Text input", value = "EST"),
                                                         textInput("svmtd", label = "SVM Method", value = "svmLinear")
                                                       ), mainPanel(tabsetPanel(tabPanel("Train Error",verbatimTextOutput("sAOC")),#actionButton("saplot1", "Plot train error"),
                                                                                tabPanel("Test Error", uiOutput("smyconfusionMatrix"), actionButton("saplot2", "Plot test error"), actionButton("sastats2", "Stats of the test error"))
                                                                                #tabPanel("Important feature", imageOutput("simptFeature"),uiOutput("sdownload3"))
                                                       ))))
                                              
                                   ),
                                   tabPanel("Validate",sidebarLayout(sidebarPanel(
                                     numericInput("ntimes", "number of validation set",min= 1, max = 10,  step = 1, value = 3)),
                                     mainPanel(tabsetPanel(tabPanel("Accuracy", verbatimTextOutput("Acc")))
                                     ))
                                   ),                                           
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
                                   )
                                   ))


server <- function(input, output, session){
  ############## function to generate specific taxon level data ######################
  myLevelData <- reactive({
    return(getLevelData(readmydata(input$file1otutable$datapath), input$level))
  })
  
  myGoodfeature <- reactive({
    return(getGoodfeature(myLevelData(), input$threshold, input$samplePercent, input$norm))
  })
  
  myrfmodel <- reactive({
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
    return(readmydata(input$file1otutable$datapath))
  }, rownames = TRUE)
  
  output$mGoodTbl <- renderTable({
    req(input$norm)
    myGoodfeature()
  }, rownames =  T)
 
  v1 <- reactiveValues(data = NULL)
  v <- reactiveValues(data = NULL)

  output$AOC <- renderUI({
    req(v1$data)
    if(v1$data == 1){
      plotOutput("plottrainerror")
    }else{
      verbatimTextOutput("Statstrain")
    }
  })
  
  observeEvent(input$aplot1,{ v1$data <- 1  })
  observeEvent(input$astats1,{v1$data <- 2  })
  
  output$plottrainerror <- renderPlot({
    req(input$myresponseVector$datapath)
    plot(myrfmodel()[[3]], main= "Train Error during training model")
  })
  
  output$Statstrain <- renderPrint({
    req(input$myresponseVector$datapath)
    myrfmodel()[[3]]
  })
  #############################################################################################################################################################
  output$myconfusionMatrix <- renderUI({
    req(v$data)
    if(v$data == 1){
      plotOutput("plotconfumat")
      } else{
      verbatimTextOutput("Statsconfu")
      }
    })
  
  observeEvent(input$aplot2,{ v$data <- 1  })
  observeEvent(input$astats2,{v$data <- 2  })
  
    output$plotconfumat <- renderPlot({
    getconfuMat(myrfmodel()[[2]], myrfmodel()[[3]])[v$data]
  })
 
  output$Statsconfu <- renderPrint({
    req(myrfmodel())
    getconfuMat(myrfmodel()[[2]], myrfmodel()[[3]])[v$data]
  })
  ####################################################################################################################################################
  output$imptFeature <- renderPlot({
    req(myrfmodel())
    plotimportantfeatures(myrfmodel()[[3]], 10)
  })
  
  output$Acc <- renderPrint({
    req(input$ntimes)
    validation(input$ntimes, myGoodfeature(), input$classid, input$sampleid,input$ruleout, input$psd,readmetadata(input$myresponseVector$datapath))
  })
  
  output$Preresult <- renderTable({
    req(input$unknw$datapath)
    a<- getGoodfeature(getLevelData(readmydata(input$unknw$datapath),
                                    input$level),input$threshold, input$samplePercent, input$norm)
    rownames(a)<- gsub("|", ".", rownames(a), fixed = TRUE)
    getunknpredict(a,myrfmodel())
  }, rownames= TRUE)
  
  ####################################################################################################
  ##### metaphlan svm #############
  
  sv1 <- reactiveValues(data = NULL)
  sv <- reactiveValues(data = NULL)
  observeEvent(input$saplot2,{ sv$data <- 1  })
  observeEvent(input$sastats2,{  sv$data <- 2 })
  
  output$sAOC <- renderPrint({
    req(input$smyresponseVector$datapath)
    smyrfmodel()[[3]]
  })
 output$smyconfusionMatrix <- renderUI({
    req(sv$data)
    if(sv$data == 1){
      plotOutput("splotconfumat")
    } else{
      verbatimTextOutput("sStatsconfu")
    }
  })
  output$splotconfumat <- renderPlot({
   getconfuMat(smyrfmodel()[[2]], smyrfmodel()[[3]])[1]
  })
  output$sStatsconfu <- renderPrint({
    req(smyrfmodel())
    getconfuMat(smyrfmodel()[[2]],smyrfmodel()[[3]])[2]
  })
  
  ##########################################################################################################################################################
  ### metaphlan glm
  
  output$gAOC <- renderPrint({
    req(input$gmyresponseVector$datapath)
    gmyrfmodel()[[3]] #, smyrfmodel()[[1]])
  })
  
  gv1 <- reactiveValues(data = NULL)
  gv <- reactiveValues(data = NULL)
  
 
  observeEvent(input$gaplot1,{ gv1$data <- 1})
  observeEvent(input$gastats1,{gv1$data <- 2})
  
 
  output$gmyconfusionMatrix <- renderUI({
    req(gv$data)
    if(gv$data == 1){
      plotOutput("gplotconfumat")
    }else{
      verbatimTextOutput("gStatsconfu")
    }
  })
  
  observeEvent(input$gaplot2,{ gv$data <- 1  })
  observeEvent(input$gastats2,{ gv$data <- 2})
  
  output$gplotconfumat <- renderPlot({
    getconfuMat(gmyrfmodel()[[2]], gmyrfmodel()[[3]])[gv$data]
  })
  
  output$gStatsconfu <- renderPrint({
    req(gmyrfmodel())
    getconfuMat(gmyrfmodel()[[2]],gmyrfmodel()[[3]])[gv$data]
  })
}

shinyApp(ui, server)
