
#product hunt

library(zoo)
library(xts)
library(dygraphs)
library(dplyr)
library(ggplot2)
library(plotly)
library(RColorBrewer)
library(portfolio)
library(treemap)
library(wordcloud)
library(NLP)
library(tm)
library(SnowballC)
library(RColorBrewer)
library(stringr)
library(textreg)
library(googleVis)
library(shiny)


shinyUI(fluidPage(
  

  titlePanel("Product Hunt Web Scarpying Project"),
  sidebarLayout(
  sidebarPanel(
    dateRangeInput("datetime", label = h3("Date range"), start = "2016-10-20", end = "2017-07-29"),
    selectInput("timetype","Choose:",
                choices=timetypes),
    selectInput("selection", "Choose:",
                choices =reviewtypes
                  ),
    actionButton("update", "Change"),
    hr(),
    sliderInput("freq",
                "Minimum Frequency:",
                min = 1,  max = 100, value = 10),
    sliderInput("max",
                "Maximum Number of Words:",
                min = 1,  max = 500,  value = 200)
  ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Overview",
                 h4("Time"),
                 dygraphOutput("o1"),
                 h4('Tag'),
                 plotlyOutput("o2")



        ),
        tabPanel("Review",
                 h3("Title"),
                 plotOutput("plot")
      ),
     tabPanel("Hypothesis-Team Member",
              h4("Summary"),
              verbatimTextOutput("summary1"),
              h3('ScatterPlot'),
              plotlyOutput("h1"),
              h3("Boxplot"),
              plotlyOutput("h2")
  ),
  tabPanel("Hypothesis-Description",
           h4("Summary"),
           verbatimTextOutput("summary2"),
           h3('Description'),
           plotOutput("h3"),
           h3("Word"),
           plotlyOutput("h4"),
           h4('Wordcloud'), 
           plotOutput("h5")

))
))
)
)

