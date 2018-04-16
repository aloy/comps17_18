# setwd("~/Work/RD/MacroStab")
library(ggplot2)
####
#### Server
####
shinyServer(function(input, output, session) {
  ##
  ##  the two available datasets
  ##
  output$dat1 <- renderTable({ dat1 })
  output$dat2 <- renderTable({ dat2 })  
  ##
  ## get the selected dataset
  ##
  datGet <- reactive({
    if (input$datatest == "0") return(NULL) 
    if (input$datatest == "1") return(dat1) 
    if (input$datatest == "2") return(dat2) 
  })
  ##
  ## Preliminary objects 
  ##
  Radiobutton <- function(i, selected=NULL){
    radioButtons(paste0("radio",i), "plot title:", choices=c("bonjour", "guten Tag"), selected=selected)      
  }
  pObjects <- reactive({
    dat <- datGet()
    if (is.null(dat)) return(NULL) 
    Levels <- levels(dat$Test)
    J <- length(Levels)
    Tabnames <- paste0("Test ", Levels) 
    list(J=J, Levels=Levels, Tabnames=Tabnames)
  })
  outputNodes <- reactive({ # output node names
    pobjects <- pObjects()
    if (is.null(pobjects)) return(NULL)  
    J <- pobjects$J
    list(tnodes=paste0("tnode", LETTERS[1:J]), # table outputs
         pnodes=paste0("pnode", LETTERS[1:J])) # plot outputs
  })
  Selecteds <- reactive({ # return the values selected in the tabs (selectInput is defined in the tabs)
    dat <- datGet()
    if (is.null(dat)) return(NULL) 
    J <- length(levels(dat$Test))
    selecteds <- rep(NA, J)
      for(i in 1:J){ 
        selecteds[i] <- input[[paste0("sel",i)]]
      }
    selecteds
  })
  ##
  ## make a radio button in the sidebar depending on the active tab
  ##
  output$Radio <- renderUI({
    if (!is.null(pObjects())) {
      i <- input$tab0
      selected <-  if(!is.null(input[[paste0("radio",i)]])) input[[paste0("radio",i)]] else NULL
      if(!is.element(i, c("0","firsttab","summarytab"))){
        return(Radiobutton(as.numeric(i),selected))
      }
    }
  })
  SelectedRadios <- reactive({ # return the values selected in the tabs 
    pobjects <- pObjects()
    if (is.null(pobjects)) return(NULL)  
    J <- pobjects$J
    selecteds <- rep(NA, J)
    for(i in 1:J){ 
      sel <- input[[paste0("radio",i)]]
      selecteds[i] <- if(is.null(sel)) NA else sel
    }
    selecteds
  })
  ##
  ## make the UI in each tab - TRICK: use input$tab0 as the current counter, not i ! 
  ##
  observe({ 
    pobjects <- pObjects()
    if (!is.null(pobjects)) {
      outnodes <- outputNodes()
      tnodes <- outnodes$tnodes
      pnodes <- outnodes$pnodes
      tests <- pobjects$Levels
      J <- pobjects$J
      dat <- datGet()
      ## overall plot in the first tab :
      output$dataplot <- renderPlot({
        gg <- ggplot(dat, aes(x=timepoint, y=y)) + 
            geom_point() + 
            geom_smooth(method=lm, se=FALSE, size=1, linetype="twodash") +
            facet_grid(Test~.) +
            ylab("result")
        print(gg) 
      }, width=500, height=900)
      ## tab 1, 2, ..., J
      I <- input$tab0
      for(i in 1:J){ 
        if(I==i){
          test <- tests[as.numeric(I)] 
          dd <- droplevels(subset(dat, subset= Test== test))
          output[[tnodes[i]]] <- renderTable({ # table in each tab 
            dd
          })
          title <- input[[paste0("radio",i)]]
          output[[pnodes[i]]] <- renderPlot({ # plot in each tab
            plot(dd$timepoint, dd$y, main=title)
          }, width=600, height=300)
        }
      }
      ## UIs in the summary tab:
      output$selections <- renderTable({ # to display in the "Summary" tab
        data.frame(tab=pobjects$Tabnames, selected=Selecteds())
      })
      output$radioselections <- renderTable({ # to display in the "Summary" tab
        data.frame(tab=pobjects$Tabnames, selected=SelectedRadios())
      })
    }
  })
  ##
  ## make the tabs 
  ##
  output$twotabs <- renderUI({
    tabs <- list(NULL)
    ## temporary firsttab (disappears after data selection) :
    tabs[[1]] <- tabPanel("Data", 
      h2("Choose a test dataset"), 
      h3("one tab will be generated for each level of the Test column"), 
      withTags(div(class='row-fluid',
                   div(class='span4', h3("Data test 1:"), tableOutput("dat1")),
                   div(class='span4', h3("Data test 2:"), tableOutput("dat2"))
      )),
       value="0")
    ## permanent tabs : firsttab, 1, 2, ..., J, summarytab
    pobjects <- pObjects()
    if (!is.null(pobjects)) { 
      outnodes <- outputNodes()
      tnodes <- outnodes$tnodes
      pnodes <- outnodes$pnodes
      tabnames <- pobjects$Tabnames
      J <- length(tabnames)
      tabs[[1]] <- tabPanel("Data",
                          h3("Overview of Data"), 
                          h3("Click on the tabs to run the analysis for each test"), 
                          h3("When done, click on the Summary tab to check and generate a report"), 
                          plotOutput("dataplot"),
                          value="firsttab")
      for(i in 1:J){
        tabs[[i+1]] <- tabPanel(tabnames[i], 
                            h3(tabnames[i]), 
                            selectInput(paste0("sel",i), "Select (will be rendered in the summary tab)", choices=as.character(1:3), selected="1"), 
                            tableOutput(tnodes[i]), 
                            plotOutput(pnodes[i]),
                            value=i)
      }
      tabs[[J+2]] <- tabPanel("Summary", 
                                    h3("Your selections:"),
                                    tableOutput("selections"),
                                    tableOutput("radioselections"), 
                                    value="summarytab")
    } 
    tabs$id <- "tab0"
    do.call(tabsetPanel, tabs)
  })
  #
})
  