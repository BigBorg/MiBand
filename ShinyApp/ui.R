# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)

shinyUI(pageWithSidebar(
  headerPanel("MiBand Analysis"),
  
  sidebarPanel(
        p("If you don't have MiBand, click the Demo button then Submit button to use built-in data. Do not change default user id when using demo data."),
        shiny::actionButton("domofile","Demo"),
        shiny::textInput("userid","Your MiBand user id","963276123"),
        shiny::fileInput("file1","Your databases.zip file:"),
        shiny::actionButton("gobutton","Submit"),
        p("Please delete your files before closing page."),
        shiny::actionButton("delete","Delete"),
        textOutput("isdeleted")
        ),
  
mainPanel(
        shiny::tabsetPanel(
                shiny::tabPanel("Load Data",
                                h2("Data cleaning"),
                                p("MiBand package can be installed from github repo:"),
                                code("require(devtools)"),
                                br(),
                                code("install_github(\"MiBand_R_Package\",\"BigBorg\")"),
                                br(),
                                code("library(MiBand)"),
                                p("loadMiData function reads in Mi Databases file and return a list of raw data and cleaned data"),
                                code("MiData <- loadMiData(\"~/databases\",\"user_id=963276123\")"),
                                br(),
                                code("summary(MiData$rawdata)"),
                                tableOutput("rawsummary"),
                                code("summary(MiData$completedata)"),
                                tableOutput("completesummary")
                                ),
                shiny::tabPanel("Sleep",
                                h2("Sleep Analysis"),
                                h4("Compare light sleep and deep sleep."),
                                p("\"sleepPlot\" function returns plots in a list. The first parameter is the object returned by function \"loadMiData\", the second parameter sets whether to show plots directly or simply return plots as a list"),
                                code("sleepplots<-sleepPlot(MiData,show=FALSE)"),
                                br(),
                                code("sleepplots$boxplot"),
                                plotOutput("sleepbox"),
                                h4("Trend Plot"),
                                code("sleepplots$trendplot"),
                                plotOutput("sleepTrend"),
                                h2("Sleep Efficiency"),
                                h4("Against Total Sleep"),
                                p("\"sleepEfficiencyPlot\" function plots sleep efficiency against total sleep duration. Sleep Efficiency is caculated as deep sleep divided by total sleep"),
                                code("efficiencyplots <- sleepEfficiencyPlot(MiData,FALSE)"),
                                br(),
                                code("efficiencyplots$total"),
                                plotOutput("efficiencytotal"),
                                h4("Against Date"),
                                code("efficiencyplots$date"),
                                plotOutput("efficiencydate"),
                                h2("Weekly Means"),
                                code("weeksum <- weeksummary(MiData,FALSE)"),
                                br(),
                                code("weeksum$lightsleep"),
                                plotOutput("weeklight"),
                                code("weeksum$deepsleep"),
                                plotOutput("weekdeep")
                                ),
                shiny::tabPanel("Step",
                                h2("Step Analysis"),
                                h4("Hist"),
                                p("\"stepPlot\" function returns histogram plot and a trend plot as a list."),
                                code("stepplots <- stepPlot(MiData,FALSE)"),
                                br(),
                                code("stepplots$hist"),
                                plotOutput("stepHist"),
                                h4("Trend"),
                                code("stepplots$trend"),
                                plotOutput("stepTrend"),
                                h2("Weekly Means"),
                                code("weeksum <- weeksummary(MiData,FALSE)"),
                                br(),
                                code("weeksum$step"),
                                plotOutput("weekstep")
                                )
        )
)

))