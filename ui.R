library(shiny)

ui <- fluidPage(
  titlePanel("Attrition Analytics"),
  
  sidebarLayout( 
    
    sidebarPanel(
      fileInput("variable","Upload CSV File:",accept = c("text/csv",
                                                         "text/comma-separated-values,text/plain",
                                                         ".csv"))
    ),
    mainPanel(tableOutput("contents"))
               )
               )