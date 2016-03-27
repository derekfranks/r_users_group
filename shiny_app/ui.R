
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library('RCurl')
library('XML')
library('treemap')
library('dplyr')


shinyUI(fluidPage(
  
  # Application title
  titlePanel("AlchemyAPI Text Analysis"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      radioButtons(inputId = "type", label = "What do you want to analyze?",
                   choices = c("concept", "entity", "keyword"),
                   selected = "concept"),
      fileInput(inputId = "textFile", label = "Upload *.txt file(s)",
                multiple = TRUE, accept = "text/plain"),
      actionButton(inputId = "go", label = "Analyze it!")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("treeMap")
    )
  ),
  dataTableOutput("outputTable"),
  downloadButton("downloadData")
))

