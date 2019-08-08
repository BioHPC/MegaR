#' megaR megaRshiny
#' Use megaR through shiny interface
#'
#' This function allows the user to input data files and alter the input
#' variables to make sure the formatting is correct.
#' They can then run the megaR package which will output the results and plots
#' in the browser and allow the user to download results as needed.
#'
#' @importFrom shiny shinyUI
#' @importFrom shiny tabPanel
#' @importFrom shiny sidebarLayout
#' @importFrom shiny sidebarPanel
#' @importFrom shiny selectInput
#' @importFrom shiny uiOutput
#' @importFrom shiny fileInput
#' @importFrom shiny radioButtons
#' @importFrom shiny mainPanel
#' @importFrom shiny downloadButton
#' @importFrom shiny sliderInput
#' @importFrom shiny plotOutput
#' @importFrom shiny renderUI
#' @importFrom shiny observeEvent
#' @importFrom shiny reactiveValues
#' @importFrom shiny tags
#' @importFrom shiny renderPlot
#' @importFrom shiny downloadHandler
#' @importFrom shiny shinyApp
#' @importFrom shiny updateRadioButtons
#' @importFrom shiny stopApp
#' @importFrom shiny textInput
#' @importFrom shiny navbarPage
#' @importFrom biomaRt listDatasets
#' @importFrom biomaRt useMart
#' @importFrom biomaRt listAttributes
#' @importFrom biomaRt listFilters
#' @importFrom DT renderDataTable
#' @importFrom DT dataTableOutput
#' @importFrom DT datatable
#' @importFrom grDevices dev.off
#' @importFrom grDevices png
#' @importFrom graphics legend
#' @importFrom graphics matplot
#' @importFrom graphics par
#' @importFrom utils write.csv
#' @importFrom utils write.table
#' @examples
#' if(interactive()) {LONGO()}
#' @export
#'
 megaR <- function(){


##check prediction
#library(shiny)
##library(shinythemes)

#library(randomForest)
#library(stringr)
#library(plyr)
#library(ggplot2)
#library(RColorBrewer)
#library(biomformat)
#library(caret)
options(warn=-1)
#source("glm.R")
#source("getgoodfea.R")
#source("getleveldata.R")
#source("training.R")
#source("plotconfumat.R")
#source("readdata.R")
#source("prediction.R")
#source("plottopimptfeat.R")
#source("svmmodeltrain.R")
#source("validation.R")
options(shiny.maxRequestSize=30*1024^2)



ui <- shiny::fluidPage(theme=shinythemes::shinytheme("flatly"),
                       shiny::navbarPage("MegaR",id= "inTabsetm",
                                         shiny::tabPanel("Data Input",
                                                         shiny::fluidRow(shiny::sidebarLayout(
                                                             shiny::sidebarPanel(shiny::fileInput(
                                                                 inputId = "file1otutable",
                                                                 label = "COUNT TABLE",
                                                                 multiple = FALSE)
                                                             ),
                                                             shiny::mainPanel(type = "tab",shiny::tabsetPanel(
                                                                 shiny::tabPanel("Data",
                                                                                   DT::dataTableOutput("mdataTbl", width = 900))
                                                                 #tabPanel("G_Heatmap",
                                                                 #plotOutput("mgenus")),
                                                                 #tabPanel("S_Heatmap",
                                                                 #plotOutput("mspecies"))
                                                             ))
                                                         ))
                                         ),
                                         shiny::tabPanel("Preprocessing",
                                                         shiny::fluidRow(
                                                             shiny::sidebarLayout(
                                                                 shiny::sidebarPanel(
                                                                     shiny:: radioButtons("level",
                                                                                          "Criteria for feature selection",
                                                                                          choices = c("Genus Level",
                                                                                                      "Species Level",
                                                                                                      "All Level"),
                                                                                          selected = "Genus"),
                                                                     shiny:: numericInput(
                                                                         "threshold", "THRESHOLD",min= 0,
                                                                         max = 100,  step = 0.001,
                                                                         value = 0.003),
                                                                     shiny::sliderInput(
                                                                         "samplePercent", "Percentage of Sample", min = 0 , max = 100,step = 1, value=5),
                                                                     shiny:: helpText(
                                                                         "OTU that have less than the threshold value in given percentage of sample are
                                                                         removed."),
                                                                     shiny:: radioButtons('norm' ,"Normalization",choices = c(YES = "css",NO="none"),
                                                                                          selected = "")),
                                                                 shiny::mainPanel(shiny::tabsetPanel(shiny::tabPanel("Data",DT::dataTableOutput(
                                                                     "mGoodTbl", width = 900))))))),
                                         shiny::navbarMenu("Model building",
                                                           shiny::tabPanel("GLM",
                                                                           shiny::sidebarLayout(shiny::sidebarPanel(
                                                                               shiny::fileInput(
                                                                                   inputId = "gmyresponseVector",
                                                                                   label ="Please input the class vector",multiple = FALSE),
                                                                               shiny::numericInput("gclassid",label ="column number for classid",
                                                                                                   min = 1, max = 100, value =8),
                                                                               shiny::numericInput("gsampleid",label= "column number for sampleid",
                                                                                                   min = 1, max = 100, value =53),
                                                                               shiny::numericInput("gpsd","percentage of data in training",
                                                                                                   min = 60, max = 100, value = 90),
                                                                               shiny::uiOutput("gselectclass")#shiny::textInput("gruleout",label = "Remove class", value = "EST")
                                                                           ),
                                                                           shiny:: mainPanel(shiny::tabsetPanel(shiny:: tabPanel("Train Error",
                                                                                                                                 shiny::verbatimTextOutput("gAOC")),
                                                                                                                shiny::tabPanel("Test Error",shiny::uiOutput("gmyconfusionMatrix"),
                                                                                                                                shiny:: actionButton("gaplot2", "Plot test error"),
                                                                                                                                shiny:: uiOutput("gdownload2") ,
                                                                                                                                shiny:: actionButton("gastats2", "Stats of the test error"))
                                                                                                                )))),
                                                           
                                                           shiny:: tabPanel("Random Forest", shiny::sidebarLayout(
                                                               shiny::sidebarPanel(shiny::fileInput(
                                                                   inputId = "myresponseVector",label="Please input the class vector",
                                                                   multiple = FALSE),
                                                                   shiny::numericInput("classid",label ="column number for class info",
                                                                                       min = 1, max = 100, value =8),
                                                                   shiny:: numericInput("sampleid", label = "column number for sampleid",
                                                                                        min = 1, max = 100, value =53),
                                                                   shiny::numericInput("psd", "percentage of data in training",
                                                                                       min = 60, max = 100, value = 90),
                                                                   shiny::uiOutput("selectclass"),#shiny::textInput("ruleout", label = "Remove class", value = "EST"),
                                                                   shiny::sliderInput("range", "Number of variable at split",
                                                                                      min = 1, max = 1000, value = c(200,500), round = T, step = 5)
                                                               ),
                                                               shiny::mainPanel(shiny::tabsetPanel(shiny::tabPanel("Train Error",
                                                                                                                   shiny::uiOutput("AOC"),
                                                                                                                   shiny::actionButton("aplot1",
                                                                                                                                       "Plot train error"),shiny::uiOutput(
                                                                                                                                           "download1"),
                                                                                                                   
                                                                                                                   shiny::actionButton("astats1", "Stats of the train error")),
                                                                                                   shiny:: tabPanel("Test Error", shiny::uiOutput("myconfusionMatrix"),
                                                                                                                    shiny::uiOutput("download2"), shiny::actionButton("aplot2", "Plot test error"),
                                                                                                                    shiny::actionButton("astats2", "Stats of the test error")),
                                                                                                   shiny:: tabPanel("Important feature", shiny::imageOutput("imptFeature"),
                                                                                                                    shiny:: uiOutput("download3")),
                                                                                                   shiny::tabPanel("Accuracy", shiny::imageOutput("accuracy")))))
                                                           ),
                                                           shiny:: tabPanel("SVM",shiny::sidebarLayout(shiny::sidebarPanel(
                                                               shiny::fileInput(inputId = "smyresponseVector",
                                                                                label = "Please input the class vector", multiple = FALSE),
                                                               shiny::numericInput("sclassid", label = "column number for classid",
                                                                                   min = 1, max = 100, value =8),
                                                               shiny::numericInput("ssampleid", label = "column number for sampleid",
                                                                                   min = 1, max = 100, value =53),
                                                               shiny::numericInput("spsd", "percentage of data in training",
                                                                                   min = 60, max = 100, value = 90),
                                                               shiny::uiOutput("sselectclass"),#shiny:: textInput("sruleout", label = "Remove class", value = "EST"),
                                                               shiny::sliderInput("srange", "Cost for non-linearity",
                                                                                  min = 0, max = 5, value = c(0.02,0.8), step = 0.01),
                                                               shiny::textInput("svmtd", label = "SVM Method", value = "svmLinear")
                                                           ),
                                                           shiny::mainPanel(shiny::tabsetPanel(
                                                            shiny::tabPanel("Train Error", shiny::verbatimTextOutput("sAOC")),
                                                           shiny::tabPanel("Test Error", shiny::uiOutput("smyconfusionMatrix"),
                                                                           shiny::actionButton("saplot2", "Plot test error"),
                                                                           shiny::uiOutput("sdownload2") ,
                                                                           shiny::actionButton("sastats2", "Stats of the test error")),
                                                           shiny::tabPanel("Accuracy", shiny::imageOutput("saccuracy"))
                                                           )))
                                                           
                                         )),
                                         shiny::tabPanel("Validate",
                                                         shiny:: sidebarLayout(shiny::sidebarPanel(
                                                             shiny::radioButtons("choicemdl", label = "Choose model",
                                                                                 choices = c(RandomForest = "rfmodel" ,
                                                                                             SVM = "svmmodel", GLM = "glmmodel"),
                                                                                 selected = ""),
                                                             shiny:: numericInput("ntimes", "number of validation set",min= 1, max = 10,
                                                                                  step = 1, value = 3)),
                                                             shiny:: mainPanel(shiny::tabsetPanel(shiny::tabPanel("Accuracy",
                                                                                                           shiny::verbatimTextOutput("validationAcc")))
                                                             ))
                                         ),
                                         shiny:: navbarMenu("Prediction",shiny::tabPanel(
                                             "Use most recent model",
                                             shiny::sidebarLayout(shiny::sidebarPanel(
                                                 shiny:: fileInput(inputId = "unknw",
                                                                   label= "Please input your unknown dataset",
                                                                   multiple= FALSE)),
                                                 shiny:: mainPanel(shiny::tabsetPanel(shiny::tabPanel("Prediction",
                                                                                                      DT::dataTableOutput("Preresult", width = 900))
                                                 )))),
                                             shiny:: tabPanel("Use default model")
                                         )
                                         ))


server <- function(input, output, session){
    ############## function to generate specific taxon level data #############
    myreaddata <- shiny::reactive({
        return(readmydata(input$file1otutable$datapath))
    })
    
    myLevelData <- shiny::reactive({
        return(getLevelData(myreaddata(), input$level))
    })
    
    myGoodfeature <- shiny::reactive({
        return(getGoodfeature(myLevelData(),input$threshold,input$samplePercent,
                              input$norm))
    })
    
    myrfmodel <- shiny::reactive({
        a <-gettrainingdonerf(myGoodfeature(), input$classid, input$sampleid,
                             input$ruleout, input$psd,readmetadata(
                                 input$myresponseVector$datapath),input$range)
        return(a)
    })
    smyrfmodel <-shiny:: reactive({
        a <- gettrainingdonesvm(myGoodfeature(), input$sclassid,input$ssampleid,
                                input$sruleout, input$spsd,readmetadata(
                                    input$smyresponseVector$datapath),input$svmtd, input$srange)
        return(a)
    })
    
    gmyrfmodel <-shiny:: reactive({
        a<-gettrainingdoneglm(myGoodfeature(), input$gclassid,
                              input$gsampleid, input$gruleout, input$gpsd,
                              readmetadata(input$gmyresponseVector$datapath))
        return(a)
    })
    
    output$mdataTbl <- DT::renderDataTable({
        shiny::req(input$file1otutable$datapath)
        DT::datatable(myreaddata(),options = list(scrollX = TRUE))
    })
    
    output$mGoodTbl <- DT::renderDataTable({
        shiny::req(input$norm)
        DT::datatable(myGoodfeature(),options = list(scrollX = TRUE))
    })
    
    v1 <- shiny::reactiveValues(data = NULL)
    v2 <- shiny::reactiveValues(data = NULL)
    
    output$AOC <- shiny::renderUI({
        shiny::req(v1$data)
        if(v1$data == 1){
            shiny::plotOutput("plottrainerror")
        }else{
            shiny::verbatimTextOutput("Statstrain")
        }
    })
    
    shiny:: observeEvent(input$aplot1,{ v1$data <- 1  })
    shiny::observeEvent(input$astats1,{v1$data <- 2  })
    
    output$plottrainerror <- shiny::renderPlot({
        shiny::req(input$myresponseVector$datapath)
        graphics::plot(myrfmodel()[[3]]$finalModel,
                       main="Train Error during training model")
    })
    
    output$Statstrain <- shiny::renderPrint({
        shiny::req(input$myresponseVector$datapath)
        myrfmodel()[[3]]$finalModel
    })
    
    output$myconfusionMatrix <- shiny::renderUI({
        shiny::req(v2$data)
        if(v2$data == 1){
            plotOutput("plotconfumat")
        } else{
            shiny::verbatimTextOutput("Statsconfu")
        }
    })
    
    shiny::observeEvent(input$aplot2,{ v2$data <- 1  })
    shiny::observeEvent(input$astats2,{v2$data <- 2  })
    
    output$plotconfumat <- shiny::renderPlot({
        getconfuMat(myrfmodel()[[2]], myrfmodel()[[3]])[v2$data]
    })
    
    output$Statsconfu <- shiny::renderPrint({
        shiny::req(myrfmodel())
        getconfuMat(myrfmodel()[[2]], myrfmodel()[[3]])[v2$data]
    })
    
    output$imptFeature <-shiny:: renderPlot({
        shiny::req(myrfmodel())
        plotimptfeatures(myrfmodel()[[3]], 10)
    })
    
    output$accuracy <- shiny::renderPlot({
        shiny::req(myrfmodel())
        plot(myrfmodel()[[3]])
    })
    output$saccuracy <- shiny::renderPlot({
        shiny::req(smyrfmodel())
        plot(smyrfmodel()[[3]])
    })
    
    output$validationAcc <-shiny:: renderPrint({
        shiny::req(input$choicemdl)
        if(input$choicemdl == "rfmodel"){
            validation(input$ntimes, input$choicemdl,myGoodfeature(),input$classid,
                       input$sampleid,input$ruleout, input$psd,
                       readmetadata(input$myresponseVector$datapath),
                       myrfmodel()[[3]]$finalModel$mtry)}
        
        else if (input$choicemdl == "svmmodel"){
            validation(input$ntimes, input$choicemdl,myGoodfeature(), input$sclassid,
                       input$ssampleid,input$sruleout, input$spsd,
                       readmetadata(input$smyresponseVector$datapath),
                       smyrfmodel()$results$C[which(smyrfmodel()$results$Accuracy==max(smyrfmodel()$results$Accuracy))])}
        
        else{
            validation(input$ntimes, input$choicemdl,myGoodfeature(), input$gclassid,
                       input$gsampleid,input$gruleout, input$gpsd,
                       readmetadata(input$gmyresponseVector$datapath), gmyrfmodel())}
        
    })
    
    output$Preresult <- DT::renderDataTable({
        shiny::req(input$unknw$datapath)
        a <- getGoodfeature(getLevelData(readmydata(input$unknw$datapath),
                                         input$level),input$threshold,
                            input$samplePercent, input$norm)
        if(input$choicemdl == "rfmodel"){
           DT::datatable(getunknpredict(a,myrfmodel(),options = list(scrollX = TRUE ))
                         }
        else if(input$choicemdl == "svmmodel"){
          DT::datatable(getunknpredict(a,smyrfmodel(),options = list(scrollX = TRUE))
                        }
        else {
           DT::datatable(getunknpredict(a,gmyrfmodel(), options = list(scrollX = TRUE))
        }        
    })
    
    #######################################################################
    ##### metaphlan svm #############
    
    sv1 <- shiny::reactiveValues(data = NULL)
    sv2 <- shiny::reactiveValues(data = NULL)
    shiny::observeEvent(input$saplot2,{ sv2$data <- 1  })
    shiny::observeEvent(input$sastats2,{ sv2$data <- 2 })
    
    output$sAOC <-shiny:: renderPrint({
        shiny::req(input$smyresponseVector$datapath)
        smyrfmodel()[[3]]
    })
    output$smyconfusionMatrix <-shiny:: renderUI({
        shiny::req(sv2$data)
        if(sv2$data == 1){
            shiny::plotOutput("splotconfumat")
        } else{
            shiny::verbatimTextOutput("sStatsconfu")
        }
    })
    output$splotconfumat <- shiny::renderPlot({
        getconfuMat(smyrfmodel()[[2]], smyrfmodel()[[3]])[1]
    })
    output$sStatsconfu <- shiny::renderPrint({
        shiny::req(smyrfmodel())
        getconfuMat(smyrfmodel()[[2]],smyrfmodel()[[3]])[2]
    })
    
    ########################################################################
    
    output$gAOC <- shiny::renderPrint({
        shiny::req(input$gmyresponseVector$datapath)
        gmyrfmodel()[[3]] #, smyrfmodel()[[1]])
    })
    
    gv1 <- shiny::reactiveValues(data = NULL)
    gv2 <- shiny::reactiveValues(data = NULL)
    
    
    shiny::observeEvent(input$gaplot1,{ gv1$data <- 1})
    shiny::observeEvent(input$gastats1,{gv1$data <- 2})
    
    
    output$gmyconfusionMatrix <- shiny::renderUI({
        shiny::req(gv2$data)
        if(gv2$data == 1){
            shiny::plotOutput("gplotconfumat")
        }else{
            shiny::verbatimTextOutput("gStatsconfu")
        }
    })
    
    shiny::observeEvent(input$gaplot2,{ gv2$data <- 1  })
    shiny::observeEvent(input$gastats2,{ gv2$data <- 2})
    
    output$gplotconfumat <- shiny::renderPlot({
        getconfuMat(gmyrfmodel()[[2]], gmyrfmodel()[[3]])[gv2$data]
    })
    
    output$gStatsconfu <- shiny::renderPrint({
        shiny::req(gmyrfmodel())
        getconfuMat(gmyrfmodel()[[2]],gmyrfmodel()[[3]])[gv2$data]
    })
    output$download1 <- shiny::renderUI({
        shiny::req(v1$data)
        if(v1$data == 1) {
            downloadButton('down', 'Download Output File')
        }
    })
    output$download2 <- shiny::renderUI({
        shiny::req(v2$data)
        if(v2$data == 1){
            downloadButton('confudown', 'Download Output File')
        }
    })
    output$sdownload2 <- shiny::renderUI({
        shiny::req(sv2$data)
        if(sv2$data == 1){
            downloadButton('sconfudown', 'Download Output File')
        }
    })
    output$gdownload2 <- shiny::renderUI({
        shiny::req(gv2$data)
        if(gv2$data == 1){
            downloadButton('gconfudown', 'Download Output File')
        }
    })
    output$download3 <- shiny::renderUI({
        if(!is.null(myrfmodel())) {
            downloadButton('imptfeat', 'Download Output File')
        }
    })
    
    output$down <- shiny::downloadHandler(
        filename = "iris.pdf",
        content = function(file){
            grDevices::pdf(file) # open the pdf device
            print(graphics::plot(myrfmodel()[[3]]))
            dev.off()
        }
    )
    output$confudown <- shiny::downloadHandler(
        filename = "iris.pdf",
        content = function(file) {
            grDevices::pdf(file) # open the pdf device
            print( getconfuMat(myrfmodel()[[2]], myrfmodel()[[3]]))
            dev.off()
        }
    )
    output$sconfudown <- shiny::downloadHandler(
        filename = "iris.pdf",
        content = function(file) {
            grDevices::pdf(file) # open the pdf device
            print( getconfuMat(smyrfmodel()[[2]], smyrfmodel()[[3]]))
            dev.off()
        }
    )
    output$gconfudown <- shiny::downloadHandler(
        filename = "iris.pdf",
        content = function(file) {
            grDevices::pdf(file) # open the pdf device
            print( getconfuMat(gmyrfmodel()[[2]], gmyrfmodel()[[3]]))
            dev.off()
        }
    )
    output$imptfeat <- shiny::downloadHandler(
        filename = "iris.pdf",
        content = function(file) {
            grDevices::pdf(file) # open the pdf device
            print(plotimptfeatures(myrfmodel()[[3]], 10))
            dev.off()
        }
    )
    output$selectclass <- renderUI({
        req(input$myresponseVector$datapath)
        selectInput("ruleout", "Select levels to classify",
                    choices = levels(as.factor(readmetadata(
                        input$myresponseVector$datapath)[,input$classid])) , 
                    multiple = TRUE)
    })
    output$sselectclass <- renderUI({
        req(input$smyresponseVector$datapath)
        selectInput("sruleout", "Select levels to classify",
                    choices = levels(as.factor(readmetadata(
                        input$smyresponseVector$datapath)[,input$sclassid])) , 
                    multiple = TRUE)
    })
    output$gselectclass <- renderUI({
        req(input$gmyresponseVector$datapath)
        selectInput("gruleout", "Select levels to classify",
                    choices = levels(as.factor(readmetadata(
                        input$gmyresponseVector$datapath)[,input$gclassid])) , 
                    multiple = TRUE)
    })
}
shiny::runApp(shiny::shinyApp(ui, server), quiet=FALSE, launch.browser=TRUE)
#shinyApp(ui, server)
}

