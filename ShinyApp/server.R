# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#
library(MiBand)
library(shiny)
library(rCharts)

shinyServer(function(input, output) {
        observeEvent(input$demofile,{
                if(!dir.exists("databases")){
                        unzip("./data/databases.zip")
                }
        })
         reactiveload <- reactive({
                 loadMiData("./databases",input$userid)
         })
        observeEvent(input$gobutton,{
                if(is.null(input$file1) & !dir.exists("databases")){
                        print("NULL")
                        return(NULL)
                }
                if(!is.null(input$file1)){
                        unzip(input$file1$datapath,exdir = ".")
                }
                # First Tab Panel: Loading Data
                output$rawsummary <- renderTable({
                        MiData <- reactiveload()
                        summary(MiData$data_clean)
                })
                output$completesummary <- renderTable({
                        MiData <- reactiveload()
                        summary(MiData$data_week)
                })
                # output$data_week <- renderChart({
                #         MiData <- reactiveload()
                #         table<-dTable(MiData$data_week)
                #         table$addParams(dom = 'data_week')
                # })
                
                
                #Sleep
                output$hist_sleep <- renderPlotly({
                        MiData <- reactiveload()
                        p <- miPlot(MiData,"hist","sleep",bins = input$bins)
                        ggplotly(p)
                })
                output$box_sleep <- renderPlotly({
                        MiData <- reactiveload()
                        p <- miPlot(MiData,"box","sleep")
                        ggplotly(p)
                })
                output$ts_sleep <- renderPlotly({
                        MiData <- reactiveload()
                        p <- miPlot(MiData,"ts","sleep")
                        ggplotly(p)
                })
                output$week_sleep <- renderPlotly({
                        MiData <- reactiveload()
                        p <- miPlot(MiData,"week","sleep")
                        ggplotly(p)
                })
                output$week_efficiency <- renderPlotly({
                        MiData <- reactiveload()
                        p <- miPlot(MiData,"week","efficiency")
                        ggplotly(p)
                })
                #Step
                output$hist_step <- renderPlotly({
                        MiData <- reactiveload()
                        p <- miPlot(MiData,"hist","step",bins = input$bins)
                        ggplotly(p)
                })
                output$ts_step <- renderPlotly({
                        MiData <- reactiveload()
                        p <- miPlot(MiData,"ts","step")
                        ggplotly(p)
                })
                output$week_step <- renderPlotly({
                        MiData <- reactiveload()
                        p <- miPlot(MiData,"week","step")
                        ggplotly(p)
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
