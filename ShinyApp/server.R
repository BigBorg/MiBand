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
        observeEvent(input$domofile,{
                if(!dir.exists("databases")){
                        unzip("./data/databases.zip")
                }
        })
        
        reactiveload <- reactive({
                MiData <- loadMiData("~/databases",input$userid)
        })
        
        observeEvent(input$gobutton,{
                if(is.null(input$file1) & !dir.exists("databases")){
                        print("NULL")
                        return(NULL)
                }
                if(!is.null(input$file1)){
                        unzip(input$file1$datapath,exdir = ".")
                }
                output$rawsummary <- renderTable({
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
