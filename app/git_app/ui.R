shinyUI(pageWithSidebar(
  headerPanel("Generating an arbitrary number of tabs - assay 7bis"),
  ##
  ## sidebar panel
  ##
  sidebarPanel(   
    selectInput("datatest", "Select a dataset", choices=c(none=0, test1=1, test2=2), selected=0),
    uiOutput("Radio")
  ),
  ##
  ## main panel 
  ##
  mainPanel(
      uiOutput("twotabs")
  )
))
