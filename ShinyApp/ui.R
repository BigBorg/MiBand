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
        p("Compress /data/data/com.xiaomi.hm.health/databases directory from android to create your own databases.zip file(folder structure maintained). After decompression, there should be a folder named databases containing .db files. User id can be founded in MiBand android app's user info page or inside databases directory as part of file name origin_db_<userid>.db. "),
        shiny::textInput("userid","Your MiBand user id","963276123"),
        shiny::fileInput("file1","Your databases.zip file:"),
        shiny::h5("If you don't have your data ready, simply ignore the file input and click the Submit button."),
        shiny::actionButton("gobutton","Submit"),
        br(),
        sliderInput("bins","bins",min = 1,max=100,value = 30)
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
