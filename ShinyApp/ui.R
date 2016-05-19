# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)
library(MiBand)
library(plotly)

shinyUI(pageWithSidebar(
  headerPanel("MiBand Analysis"),
  
  sidebarPanel(
        p("If you don't have MiBand, click the Demo button then Submit button to use built-in data. Do not change default user id when using demo data."),
        shiny::actionButton("demofile","Demo Data"),
        shiny::textInput("userid","Your MiBand user id","963276123"),
        shiny::fileInput("file1","Your databases.zip file:"),
        shiny::actionButton("gobutton","Submit"),
        br(),
        sliderInput("bins","bins",min = 1,max=100,value = 30),
        code("Please delete your files before closing page."),
        br(),
        shiny::actionButton("delete","Delete"),
        textOutput("isdeleted")
        ),
  
mainPanel(
        shiny::tabsetPanel(
                shiny::tabPanel("Load Data",
                                h2("Data Loading"),
                                p("MiBand package can be installed from github repo: https://github.com/BigBorg/MiBand_R_Package"),
                                code("require(devtools)"),
                                br(),
                                code("install_github(\"MiBand_R_Package\",\"BigBorg\")"),
                                br(),
                                code("library(MiBand)"),
                                p("loadMiData function reads in Mi Databases file and return a list of clean data and data with missing value substituted"),
                                code("MiData <- loadMiData(\"~/databases\",\"user_id=963276123\")"),
                                br(),
                                code("summary(MiData$data_clean)"),
                                tableOutput("rawsummary"),
                                code("summary(MiData$data_week)"),
                                tableOutput("completesummary")
                                # p("View data_week: missing value filled with mean value of the same day of a week"),
                                # chartOutput("data_week","polyCharts")
                                ),
                shiny::tabPanel("Sleep",
                                h2("Sleep Analysis"),
                                h4("hist sleep"),
                                code("p<-miPlot(MiData,'hist',y=\"sleep\",bins=input$bins)"),
                                br(),
                                code("ggplotly(p)"),
                                plotlyOutput("hist_sleep"), # hist sleep
                                h4("box sleep"),
                                code("p<-miPlot(MiData,'box',y=\"sleep\")"),
                                br(),
                                code("ggplotly(p)"),
                                plotlyOutput("box_sleep"), # box sleep
                                h4("ts sleep"),
                                code("p<-miPlot(MiData,'ts',y=\"sleep\")"),
                                br(),
                                code("ggplotly(p)"),
                                plotlyOutput("ts_sleep"), # ts sleep
                                h4("week sleep"),
                                code("p<-miPlot(MiData,'week',y=\"sleep\")"),
                                br(),
                                code("ggplotly(p)"),
                                plotlyOutput("week_sleep"), # week sleep
                                h4("week efficiency"),
                                code("p<-miPlot(MiData,'week',y=\"efficiency\")"),
                                br(),
                                code("ggplotly(p)"),
                                plotlyOutput("week_efficiency") # week efficiency
        ),
                shiny::tabPanel("Step", 
                                h2("Step Analysis"),
                                h4("hist step"), # hist step
                                code("p<-miPlot(MiData,'hist',y=\"step\")"),
                                br(),
                                code("ggplotly(p)"),
                                plotlyOutput("hist_step"),
                                h4("ts step"), # ts step
                                code("p<-miPlot(MiData,'ts',y=\"step\")"),
                                br(),
                                code("ggplotly(p)"),
                                plotlyOutput("ts_step"),
                                h4("week step"), # week step
                                code("p<-miPlot(MiData,'week',y=\"step\")"),
                                br(),
                                code("ggplotly(p)"),
                                plotlyOutput("week_step")
                                )
        ) 

))
)
