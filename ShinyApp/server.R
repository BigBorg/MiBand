# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

if(!"MiBand" %in% installed.packages()){
        require(devtools)
        install_github("MiBand_R_Package","BigBorg")
}
library(MiBand)
library(shiny)

shinyServer(function(input, output) {
<<<<<<< HEAD
        observeEvent(input$demofile,{
=======
        observeEvent(input$domofile,{
>>>>>>> dcad271df6a81c64b92e642f9bcde0c12e629652
                if(!dir.exists("databases")){
                        unzip("./data/databases.zip")
                }
        })
<<<<<<< HEAD

         reactiveload <- reactive({
                 loadMiData("./databases",input$userid)
         })
=======
        
        reactiveload <- reactive({
                MiData <- loadMiData("~/databases",input$userid)
        })
>>>>>>> dcad271df6a81c64b92e642f9bcde0c12e629652
        
        observeEvent(input$gobutton,{
                if(is.null(input$file1) & !dir.exists("databases")){
                        print("NULL")
                        return(NULL)
                }
                if(!is.null(input$file1)){
                        unzip(input$file1$datapath,exdir = ".")
                }
                output$rawsummary <- renderTable({
<<<<<<< HEAD
                        MiData <- reactiveload()
                        summary(MiData$rawdata)
                })
                output$completesummary <- renderTable({
                        MiData <- reactiveload()
                        summary(MiData$completedata)
                })
                
                #Sleep
                reactivesleepplot <- reactive({
                  MiData <- reactiveload()
                  sleepPlot(MiData,show=FALSE)
                })  # reactive
                
                output$sleepbox <- renderPlot({
                        MiData <- reactiveload()
                        sleepplots<-reactivesleepplot()
                        sleepplots$boxplot
                })
                output$sleepTrend <- renderPlot({
                        MiData <- reactiveload()
                        sleepplots<-reactivesleepplot()
                        sleepplots$trendplot
                })
                
                
                reactiveweekplot <- reactive({
                        MiData <- reactiveload()
                        weeksummary(MiData,FALSE)
                })  # reactive
                output$weeklight <- renderPlot({
                        weekplots <- reactiveweekplot()
                        weekplots$lightsleep
                })
                output$weekdeep <- renderPlot({
                        weekplots <- reactiveweekplot()
                        weekplots$deepsleep
                })
                
                reactiveefficiency <- reactive({
                  MiData <- reactiveload()
                  sleepEfficiencyPlot(MiData,FALSE)
                })
                
                output$efficiencytotal <- renderPlot({
                        efficiencyplots <- reactiveefficiency()
                        efficiencyplots$total
                })
                output$efficiencydate <- renderPlot({
                        efficiencyplots <- reactiveefficiency()
                        efficiencyplots$date
                })
                
                #Step
                reactivestep <- reactive({
                  MiData <- reactiveload()
                  stepPlot(MiData,FALSE)
                })
                output$stepHist <- renderPlot({
                        stepplots <- reactivestep()
                        stepplots$hist
                })
                output$stepTrend <- renderPlot({
                        stepplots <- reactivestep()
                        stepplots$trend
                })
                output$weekstep <- renderPlot({
                        weekplots <- reactiveweekplot()
=======
                        #MiData <- loadMiData("~/databases",input$userid)
                        reactiveload()
                        summary(MiData$rawdata)
                })
                output$completesummary <- renderTable({
                        summary(MiData$completedata)
                })
                output$sleepbox <- renderPlot({
                        sleepplots<-sleepPlot(MiData,show=FALSE)
                        sleepplots$boxplot
                })
                output$sleepTrend <- renderPlot({
                        sleepplots<-sleepPlot(MiData,show=FALSE)
                        sleepplots$trendplot
                })
                output$weeklight <- renderPlot({
                        weekplots <- weeksummary(MiData,FALSE)
                        weekplots$lightsleep
                })
                output$weekdeep <- renderPlot({
                        weekplots <- weeksummary(MiData,FALSE)
                        weekplots$deepsleep
                })
                output$efficiencytotal <- renderPlot({
                        efficiencyplots <- sleepEfficiencyPlot(MiData,FALSE)
                        efficiencyplots$total
                })
                output$efficiencydate <- renderPlot({
                        efficiencyplots <- sleepEfficiencyPlot(MiData,FALSE)
                        efficiencyplots$date
                })
                output$stepHist <- renderPlot({
                        stepplots <- stepPlot(MiData,FALSE)
                        stepplots$hist
                })
                output$stepTrend <- renderPlot({
                        stepplots <- stepPlot(MiData,FALSE)
                        stepplots$trend
                })
                output$weekstep <- renderPlot({
                        weekplots <- weeksummary(MiData,FALSE)
>>>>>>> dcad271df6a81c64b92e642f9bcde0c12e629652
                        weekplots$step
                })
           })
        
        observeEvent(input$delete,{
                if(dir.exists("databases")){
                        system("rm -r databases")
                }
                if(!dir.exists("databases")){
                        "Successfully Deleted!"
                }
        })
})
