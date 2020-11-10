#' MegaR megaRshiny
#' Use MegaR through shiny interface
#'
#' This function allows the user to input data files and alter the input
#' variables to make sure the formatting is correct.
#' They can then run the MegaR package which will output the results and plots
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
#' if(interactive()) {MegaR()}
#' @export
#'
 MegaR <- function(){


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
                                                                                   DT::dataTableOutput("mdataTbl", width = 800))
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
                                                                         "threshold", "Threshold",min= 0,
                                                                         max = 100,  step = 0.001,
                                                                         value = 0.003),
                                                                     shiny::sliderInput(
                                                                         "samplePercent", "Percentage of Sample", min = 0 , max = 100,step = 1, value=5),
                                                                     shiny:: helpText(
                                                                         "OTU that have less than the threshold value in given percentage of sample are
                                                                         removed."),
                                                                     shiny:: radioButtons('norm' ,"Normalization",choices = c(TMM="TMM", Quantile = "quantile", CSS = "CSS", NO="none"),
                                                                                          selected = "")),
                                                                 shiny::mainPanel(shiny::tabsetPanel(shiny::tabPanel("Data",DT::dataTableOutput(
                                                                     "mGoodTbl", width = 800))))))),
                                         shiny::navbarMenu("Model building",
                                                           shiny::tabPanel("GLM",
                                                                           shiny::sidebarLayout(shiny::sidebarPanel(
                                                                               shiny::fileInput(
                                                                                   inputId = "gmyresponseVector",
                                                                                   label ="Please upload a metadata file",multiple = FALSE),
                                                                               shiny::numericInput("gclassid",label ="Column number for class ID",
                                                                                                   min = 1, max = 100, value =7),
                                                                               shiny::numericInput("gsampleid",label= "Column number for sample ID",
                                                                                                   min = 1, max = 100, value =53),
                                                                               shiny::numericInput("gpsd","Percentage of data in training",
                                                                                                   min = 60, max = 100, value = 90),
                                                                               shiny::uiOutput("gselectclass"),#shiny::textInput("gruleout",label = "Remove class", value = "EST"),
                                                                               shiny::actionButton("runglm","Run")
                                                                           ),
                                                                           shiny:: mainPanel(shiny::tabsetPanel(shiny:: tabPanel("Train Error",
                                                                                                                                 shiny::verbatimTextOutput("gAOC")),
                                                                                                                shiny::tabPanel("Test Error",shiny::uiOutput("gmyconfusionMatrix"),
                                                                                                                                shiny:: actionButton("gaplot2", "Plot test error"),
                                                                                                                                shiny:: uiOutput("gdownload2") ,
                                                                                                                                shiny:: actionButton("gastats2", "Stats of the test error")),
                                                                                                                shiny::tabPanel("AUC", shiny::imageOutput('aucglm'), shiny::uiOutput("AUCdownloadglm")),
                                                                                                                shiny::tabPanel("Download",shiny::downloadButton('downloadmodelglm',"Download Model"))
                                                                                                                )))),
                                                           
                                                           shiny:: tabPanel("Random Forest", shiny::sidebarLayout(
                                                               shiny::sidebarPanel(shiny::fileInput(
                                                                   inputId = "myresponseVector",label="Please upload a metadata file",
                                                                   multiple = FALSE),
                                                                   shiny::numericInput("classid",label ="Column number for class ID",
                                                                                       min = 1, max = 100, value =7),
                                                                   shiny:: numericInput("sampleid", label = "Column number for sample ID",
                                                                                        min = 1, max = 100, value =2),
                                                                   shiny::numericInput("psd", "Percentage of data in training",
                                                                                       min = 60, max = 100, value = 90),
                                                                   shiny::uiOutput("selectclass"),#shiny::textInput("ruleout", label = "Remove class", value = "EST"),
                                                                   shiny::sliderInput("range", "Number of variable at split",
                                                                                      min = 1, max = 1000, value = c(1,101), round = T, step = 5),
                                                                   shiny::actionButton("runrf","Run")
                                                               ),
                                                               shiny::mainPanel(shiny::tabsetPanel(shiny::tabPanel("Train Error",
                                                                                                                   shiny::uiOutput("AOC"),
                                                                                                                   shiny::actionButton("aplot1",
                                                                                                                                       "Plot train error"),
                                                                                                                   shiny::uiOutput("trainerrordl"),
                                                                                                                   shiny::actionButton("astats1", "Stats of the train error")),
                                                                                                   shiny:: tabPanel("Test Error", shiny::uiOutput("myconfusionMatrix"),
                                                                                                                    shiny::uiOutput("download2"), shiny::actionButton("aplot2", "Plot test error"),
                                                                                                                    shiny::actionButton("astats2", "Stats of the test error")),
                                                                                                   shiny:: tabPanel("Important feature", shiny::imageOutput("imptFeature"),
                                                                                                                    shiny:: uiOutput("download3")),
                                                                                                   shiny::tabPanel("Accuracy", shiny::imageOutput("accuracy"),shiny::uiOutput("download1")),
                                                                                                   shiny::tabPanel("AUC", shiny::imageOutput('auc'), shiny::uiOutput("AUCdownload")),
                                                                                                   shiny::tabPanel("Download", shiny::downloadButton('downloadmodel', "Download Model")))))
                                                           ),
                                                           shiny:: tabPanel("SVM",shiny::sidebarLayout(shiny::sidebarPanel(
                                                               shiny::fileInput(inputId = "smyresponseVector",
                                                                                label = "Please upload a metadata file", multiple = FALSE),
                                                               shiny::numericInput("sclassid", label = "Column number for class ID",
                                                                                   min = 1, max = 100, value =8),
                                                               shiny::numericInput("ssampleid", label = "Column number for sample ID",
                                                                                   min = 1, max = 100, value =53),
                                                               shiny::numericInput("spsd", "percentage of data in training",
                                                                                   min = 60, max = 100, value = 90),
                                                               shiny::uiOutput("sselectclass"),#shiny:: textInput("sruleout", label = "Remove class", value = "EST"),
                                                               shiny::sliderInput("srange", "Cost for non-linearity",
                                                                                  min = 0, max = 5, value = c(0.02,0.8), step = 0.01),
                                                               shiny::textInput("svmtd", label = "SVM Method", value = "svmLinear"),
                                                               shiny::actionButton("runsvm","Run")
                                                           ),
                                                           shiny::mainPanel(shiny::tabsetPanel(
                                                            shiny::tabPanel("Train Error", shiny::verbatimTextOutput("sAOC")),
                                                           shiny::tabPanel("Test Error", shiny::uiOutput("smyconfusionMatrix"),
                                                                           shiny::actionButton("saplot2", "Plot test error"),
                                                                           shiny::uiOutput("sdownload2"),
                                                                           shiny::actionButton("sastats2", "Stats of the test error")),
                                                           shiny::tabPanel("Accuracy", shiny::imageOutput("saccuracy"), shiny::uiOutput('saccuracydl')),
                                                           shiny::tabPanel("AUC", shiny::imageOutput('aucsvm'), shiny::uiOutput("AUCdownloadsvm")),
                                                           shiny::tabPanel("Download",shiny::downloadButton('downloadmodelsvm',"Download Model"))
                                                           )))
                                                           
                                         )),
                                         shiny::tabPanel("Validate",
                                                         shiny:: sidebarLayout(shiny::sidebarPanel(
                                                             #shiny::radioButtons("choicemdl", label = "Choose model",
                                                                                 #choices = c(RandomForest = "rfmodel" ,
                                                                                             #SVM = "svmmodel", GLM = "glmmodel"),
                                                                                 #selected = ""),
                                                             shiny:: numericInput("ntimes", "number of validation set",min= 1, max = 10,
                                                                                  step = 1, value = 3),
                                                             shiny::actionButton("val","Validate")),
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
                                                                                                      DT::dataTableOutput("Preresult", width = 800))
                                                 )))),
                                             shiny:: tabPanel("Use custom model",
                                                 shiny::sidebarLayout(shiny::sidebarPanel(
                                                     shiny::fileInput(inputId = 'unknw2',
                                                                  label = 'Please input your unknown dataset',
                                                                  multiple = FALSE),
                                                 shiny::fileInput(inputId = 'upmodel',
                                                                  label = "Please input your custom model",
                                                                  multiple = FALSE)),
                                                 shiny::mainPanel(shiny::tabsetPanel(shiny::tabPanel("Prediction",
                                                                                                     DT::dataTableOutput("Preresult2", width = 800))
                                                ))))
                                             
                                             )
                                         )
                                         )


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
    #progressbarrf <- shiny::observeEvent(input$runrf,{
        #progress <- shiny::Progress$new(style = 'notification')
        #progress$set(message = "Ready to plot", value = 100)
        #Sys.sleep(3)
        #on.exit(progress$close())
    #})
    #progressbarglm <- shiny::observeEvent(input$runglm,{
    #    progress <- shiny::Progress$new(style = 'notification')
    #    progress$set(message = "Ready to plot", value = 100)
    #    Sys.sleep(3)
    #    on.exit(progress$close())
    #})
    #progressbarsvm <- shiny::observeEvent(input$runsvm,{
        #progress <- shiny::Progress$new(style = 'notification')
        #progress$set(message = "Ready to plot", value = 100)
        #Sys.sleep(3)
        #on.exit(progress$close())
    #})
    myrfmodel <- shiny::eventReactive(input$runrf, {
        progress <- shiny::Progress$new(style = 'notification')
        progress$set(message = "Working...",value = 100)
        on.exit(progress$close())
        a <-gettrainingdonerf(myGoodfeature(), input$classid, input$sampleid,
                             input$ruleout, input$psd,readmetadata(
                                 input$myresponseVector$datapath),input$range)
        selectedmodel<<-'RF'
        out<-list(a,'RF',input$level, input$threshold, input$samplePercent, input$norm)
        return(out)
    })
    smyrfmodel <-shiny:: eventReactive(input$runsvm, {
        progress <- shiny::Progress$new(style = 'notification')
        progress$set(message = "Working...",value = 100)
        on.exit(progress$close())
        a <- gettrainingdonesvm(myGoodfeature(), input$sclassid,input$ssampleid,
                                input$sruleout, input$spsd,readmetadata(
                                    input$smyresponseVector$datapath),input$svmtd, input$srange)
        selectedmodel<<-'svmmodel'
        out<-list(a,'svmmodel',input$level, input$threshold, input$samplePercent, input$norm)
        return(out)
    })
    gmyrfmodel <-shiny:: eventReactive(input$runglm, {
        progress <- shiny::Progress$new(style = 'notification')
        progress$set(message = "Working...",value = 100)
        on.exit(progress$close())
        a<-gettrainingdoneglm(myGoodfeature(), input$gclassid,
                              input$gsampleid, input$gruleout, input$gpsd,
                              readmetadata(input$gmyresponseVector$datapath))
        selectedmodel<<-'glmmodel'
        out<-list(a,'glmmodel',input$level, input$threshold, input$samplePercent, input$norm)
        return(out)
    })
    output$mdataTbl <- DT::renderDataTable({
        shiny::req(input$file1otutable$datapath)
        DT::datatable(myreaddata() ,options = list(scrollX = TRUE))
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
        graphics::plot(myrfmodel()[[1]][[3]]$finalModel,
                       main="Train Error during training model")
    })
    
    output$Statstrain <- shiny::renderPrint({
        shiny::req(input$myresponseVector$datapath)
        myrfmodel()[[1]][[3]]$finalModel
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
        getconfuMat(myrfmodel()[[1]][[2]], myrfmodel()[[1]][[3]])[v2$data]
    })
    
    output$Statsconfu <- shiny::renderPrint({
        shiny::req(myrfmodel()[[1]])
        getconfuMat(myrfmodel()[[1]][[2]], myrfmodel()[[1]][[3]])[v2$data]
    })
    
    output$imptFeature <-shiny:: renderPlot({
        shiny::req(myrfmodel)
        plotimptfeatures(myrfmodel()[[1]][[3]], 10)
    })
    output$auc <- shiny::renderPlot({shiny::req(myrfmodel)
        rfauc <<- MLeval::evalm(myrfmodel()[[1]][[3]])
        rfauc$roc
    })
    output$aucsvm <- shiny::renderPlot({shiny::req(smyrfmodel)
        svmauc <<- MLeval::evalm(smyrfmodel()[[1]][[3]])
        svmauc$roc
    })
    output$aucglm <- shiny::renderPlot({shiny::req(gmyrfmodel)
        glmauc <<- MLeval::evalm(gmyrfmodel()[[1]][[3]])
        glmauc$roc
    })
    output$accuracy <- shiny::renderPlot({
        shiny::req(myrfmodel)
        plot(myrfmodel()[[1]][[3]])
    })
    output$saccuracy <- shiny::renderPlot({
        shiny::req(smyrfmodel)
        plot(smyrfmodel()[[1]][[3]])
    })
    
    output$validationAcc <-shiny:: renderPrint({
        shiny::req(input$val)
        if(selectedmodel== 'RF'){
            validation(input$ntimes, myrfmodel()[[2]],myGoodfeature(),input$classid,
                       input$sampleid,input$ruleout, input$psd,
                       readmetadata(input$myresponseVector$datapath),
                       myrfmodel()[[1]][[3]]$finalModel$mtry)}
        
        else if (selectedmodel=='svmmodel'){
            validation(input$ntimes, smyrfmodel()[[2]],myGoodfeature(), input$sclassid,
                       input$ssampleid,input$sruleout, input$spsd,
                       readmetadata(input$smyresponseVector$datapath),
                       smyrfmodel()[[1]]$results$C[which(smyrfmodel()[[1]]$results$Accuracy==max(smyrfmodel()[[1]]$results$Accuracy))])}
        
        else{
            validation(input$ntimes, gmyrfmodel()[[2]],myGoodfeature(), input$gclassid,
                       input$gsampleid,input$gruleout, input$gpsd,
                       readmetadata(input$gmyresponseVector$datapath), gmyrfmodel()[[1]])}
        
    })
    
    output$Preresult <- DT::renderDataTable({
        shiny::req(input$unknw$datapath)
        a <- getGoodfeature(getLevelData(readmydata(input$unknw$datapath),
                                         input$level),input$threshold,
                            input$samplePercent, input$norm)
        if(selectedmodel=='RF'){
           DT::datatable(getunknpredict(a,myrfmodel()[[1]]),options = list(scrollX = TRUE ))
                         }
        else if(selectedmodel=='svmmodel'){
          DT::datatable(getunknpredict(a,smyrfmodel()[[1]]),options = list(scrollX = TRUE))
                        }
        else {
           DT::datatable(getunknpredict(a,gmyrfmodel()[[1]]), options = list(scrollX = TRUE))
        }        
    })
    
    output$Preresult2 <- DT::renderDataTable({
        shiny::req(input$unknw2$datapath)
        shiny::req(input$upmodel$datapath)
        b <- readRDS(input$upmodel$datapath)
        a <- getGoodfeature(getLevelData(readmydata(input$unknw2$datapath),
                                       b[[3]]),b[[4]],
                          b[[5]], b[[6]])

        DT::datatable(getunknpredict(a,b[[1]]),options = list(scrollX = TRUE ))
    })
    
    #######################################################################
    ##### metaphlan svm #############
    
    sv1 <- shiny::reactiveValues(data = NULL)
    sv2 <- shiny::reactiveValues(data = NULL)
    shiny::observeEvent(input$saplot2,{ sv2$data <- 1  })
    shiny::observeEvent(input$sastats2,{ sv2$data <- 2 })
    
    output$sAOC <-shiny:: renderPrint({
        shiny::req(input$smyresponseVector$datapath)
        smyrfmodel()[[1]][[3]]
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
        getconfuMat(smyrfmodel()[[1]][[2]], smyrfmodel()[[1]][[3]])[1]
    })
    output$sStatsconfu <- shiny::renderPrint({
        shiny::req(smyrfmodel)
        getconfuMat(smyrfmodel()[[1]][[2]],smyrfmodel()[[1]][[3]])[2]
    })
    
    ########################################################################
    
    output$gAOC <- shiny::renderPrint({
        shiny::req(input$gmyresponseVector$datapath)
        gmyrfmodel()[[1]][[3]] #, smyrfmodel[[1]])
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
        getconfuMat(gmyrfmodel()[[1]][[2]], gmyrfmodel()[[1]][[3]])[gv2$data]
    })
    
    output$gStatsconfu <- shiny::renderPrint({
        shiny::req(gmyrfmodel)
        getconfuMat(gmyrfmodel()[[1]][[2]],gmyrfmodel()[[1]][[3]])[gv2$data]
    })
    rf2 <- shiny::reactiveValues()
    observe({
        if(!is.null(myrfmodel()))
       isolate(
            rf2<<-myrfmodel()
        )
    })
    output$downloadmodel <- shiny::downloadHandler(
        filename <- function(){
            paste("Model.rds")
        },
        content=function(file){
            saveRDS(rf2,file=file)
        }
    )
    rf3 <- shiny::reactiveValues()
    observe({
        if(!is.null(smyrfmodel()))
            isolate(
                rf3<<-smyrfmodel()
            )
    })
    output$downloadmodelsvm <- shiny::downloadHandler(
        filename <- function(){
            paste("Model.rds")
        },
        content=function(file){
            saveRDS(rf3,file=file)
        }
    )
    rf4 <- shiny::reactiveValues()
    observe({
        if(!is.null(gmyrfmodel()[[1]][[3]]))
            isolate(
                rf4<<-gmyrfmodel()
            )
    })
    output$downloadmodelglm <- shiny::downloadHandler(
        filename <- function(){
            paste("Model.rds")
        },
        content=function(file){
            saveRDS(rf4,file=file)
        }
    )
    output$trainerrordl<- shiny::renderUI({
        if(!is.null(myrfmodel)){
            downloadButton('tedl','Download Output File')
        }
    })
    output$download1 <- shiny::renderUI({
        #shiny::req(v1$data)
        #if(v1$data == 1) {
        if(!is.null(myrfmodel)){
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
        #shiny::req(sv2$data)
        #if(sv2$data == 1){
        if(!is.null(smyrfmodel)){
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
        if(!is.null(myrfmodel)) {
            downloadButton('imptfeat', 'Download Output File')
        }
    })
    output$saccuracydl <- shiny::renderUI({
        if(!is.null(smyrfmodel)){
            downloadButton("saccdl", "Download Output File")
        }
    })
    output$AUCdownload <- shiny::renderUI({
        if(!is.null(myrfmodel)) {
            downloadButton("AUCdl", 'Download AUC Graph')
        }
    })
    output$AUCdownloadsvm <- shiny::renderUI({
        if(!is.null(myrfmodel)) {
            downloadButton("AUCdlsvm", 'Download AUC Graph')
        }
    })
    output$AUCdownloadglm <- shiny::renderUI({
        if(!is.null(myrfmodel)) {
            downloadButton("AUCdlglm", 'Download AUC Graph')
        }
    })
    output$down <- shiny::downloadHandler(
        filename = "stats_plot.pdf",
        content = function(file){
            grDevices::pdf(file) # open the pdf device
            print(graphics::plot(myrfmodel()[[1]][[3]]))
            dev.off()
        }
    )
    output$confudown <- shiny::downloadHandler(
        filename = "confusion_matrix.pdf",
        content = function(file) {
            grDevices::pdf(file) # open the pdf device
            print( getconfuMat(myrfmodel()[[1]][[2]], myrfmodel()[[1]][[3]]))
            dev.off()
        }
    )
    output$sconfudown <- shiny::downloadHandler(
        filename = "confusion_matrix.pdf",
        content = function(file) {
            grDevices::pdf(file) # open the pdf device
            print( getconfuMat(smyrfmodel()[[1]][[2]], smyrfmodel()[[1]][[3]]))
            dev.off()
        }
    )
    output$gconfudown <- shiny::downloadHandler(
        filename = "confusion_matrix.pdf",
        content = function(file) {
            grDevices::pdf(file) # open the pdf device
            print( getconfuMat(gmyrfmodel()[[1]][[2]], gmyrfmodel()[[1]][[3]]))
            dev.off()
        }
    )
    output$imptfeat <- shiny::downloadHandler(
        filename = "important_features.pdf",
        content = function(file) {
            grDevices::pdf(file) # open the pdf device
            print(plotimptfeatures(myrfmodel()[[1]][[3]], 10))
            dev.off()
        }
    )
    output$AUCdl <- shiny::downloadHandler(
        filename = "AUC.pdf",
        content = function(file) {
            grDevices::pdf(file)
            print(rfauc$roc)
            dev.off()
        }
    )
    output$AUCdlsvm <- shiny::downloadHandler(
        filename = "AUC.pdf",
        content = function(file) {
            grDevices::pdf(file)
            print(svmauc$roc)
            dev.off()
        }
    )
    output$AUCdlglm <- shiny::downloadHandler(
        filename = "AUC.pdf",
        content = function(file) {
            grDevices::pdf(file)
            print(glmauc$roc)
            dev.off()
        }
    )
    output$saccdl <- shiny::downloadHandler(
        filename = "Accuracy.pdf",
        content = function(file) {
            grDevices::pdf(file)
            print(plot(smyrfmodel()[[1]][[3]]))
            dev.off()
        }
    )
    output$tedl <- shiny::downloadHandler(
        filename = "Train_error.pdf",
        content = function(file) {
            grDevices::pdf(file)
            print(graphics::plot(myrfmodel()[[1]][[3]]$finalModel,
                                 main="Train Error during training model"))
            dev.off()
        }
    )
    output$selectclass <- renderUI({
        req(input$myresponseVector$datapath)
        selectInput("ruleout", "Select classification labels",
                    choices = levels(as.factor(readmetadata(
                        input$myresponseVector$datapath)[,input$classid])) , 
                    multiple = TRUE)
    })
    output$sselectclass <- renderUI({
        req(input$smyresponseVector$datapath)
        selectInput("sruleout", "Select classification labels",
                    choices = levels(as.factor(readmetadata(
                        input$smyresponseVector$datapath)[,input$sclassid])) , 
                    multiple = TRUE)
    })
    output$gselectclass <- renderUI({
        req(input$gmyresponseVector$datapath)
        selectInput("gruleout", "Select classification labels",
                    choices = levels(as.factor(readmetadata(
                        input$gmyresponseVector$datapath)[,input$gclassid])) , 
                    multiple = TRUE)
    })
}
shiny::runApp(shiny::shinyApp(ui, server), quiet=FALSE, launch.browser=TRUE)
#shinyApp(ui, server)
}

