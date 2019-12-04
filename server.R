library(shiny)

# Define server logic required to plot various variables against mpg
server <- function(input, output) {
  output$contents <- renderTable({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    variable <- input$variable
    
    if (is.null(variable))
      return(NULL)
    
    read.csv(variable$datapath, header = T)
  })
  
}