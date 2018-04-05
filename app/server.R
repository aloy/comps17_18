#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(scagnostics)
library(MASS)
library(tidyverse)
library(class)
library(randomForest)
library(tidyr)
library(scagnostics)
library(caret)
library(boot)
library(e1071)

shinyServer(function(input, output) {
  
  ###CLEAN UP
  #loads data in
  df <- reactive({
    req(input$file1)
    read.csv(input$file1$datapath, sep = input$sep)
  })
  
  #checking whether discrete or not:
  discrete_cols <- reactive({
    df <- df()
    cols <- vector()
    for (var in colnames(df)){
      if (length(unique(df[,var]))/length(df[,var]) > 0.05){
        cols <- c(cols, var) 
      }
    }
    cols
  })
  
  predictions <- reactive({
    df <- df()
    colnames <- discrete_cols()
    
    pairs <- combn(colnames, 2)
    pairs_list <- split(pairs, rep(1:ncol(pairs), each = nrow(pairs)))
    
    scag_fun <- function(dataset, col_names){
      scagnostics <- scagnostics(scale(dataset[col_names]))
      return(scagnostics[1:9])
    }
    string_fun <- function(col_names){
      return(paste(col_names[1], 'vs', col_names[2]))
    }
    
    output <- t(as.data.frame(lapply(pairs_list, scag_fun, dataset = df))) 
    
    rownames(output) <- lapply(pairs_list, string_fun)
    colnames(output) <- c("scag_num_1", "scag_num_2", "scag_num_3", "scag_num_4", "scag_num_5", "scag_num_6", "scag_num_7", "scag_num_8", "scag_num_9")
    
    scag_randomForest <- readRDS("model_4.2.18.rds")
    preds <- predict(scag_randomForest, newdata = output)
    pred_df<- as.data.frame(preds)
    pred_df$name <- lapply(pairs_list, string_fun)
    pred_df <- pred_df %>% arrange(preds)
    
    return(pred_df)
  })
  
  
  ###OUTPUT
  output$contents <- renderTable({
    pred_df<- predictions()
    plot_type <- unique(pred_df$preds)
    df_bytype <- list()
    i <- 1
    for (type in plot_type){
      df_bytype[[i]] <- filter(pred_df, preds == type)
      i <- i + 1
    }
    return(pred_df)
  })
  
  output$pairs <- renderPlot({
    df <- df()
    colnames <- discrete_cols()
    return(pairs(df[,colnames]))
  })
  
})
