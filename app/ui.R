
library(scagnostics)
library(tidyverse)
library(randomForest)

shinyUI(pageWithSidebar(
  headerPanel("Looking at Pairwise Relationships"),
  ##
  ## sidebar panel
  ##
  sidebarPanel(   
    # Input: Select a file ----
    fileInput("file1", "Choose CSV File",
              multiple = TRUE,
              accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv")),
    
    # Horizontal line ----
    tags$hr(),
    
    # Input: Checkbox if file has header ----
    checkboxInput("header", "Header", TRUE),
    
    # Input: Select separator ----
    radioButtons("sep", "Separator",
                 choices = c(Comma = ",",
                             Semicolon = ";",
                             Tab = "\t"),
                 selected = ",")
  ),
  ##
  ## main panel 
  ##
  mainPanel(
      uiOutput("twotabs")
  )
))
