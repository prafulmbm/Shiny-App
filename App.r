###############  Attrition Prediction ############################

# For Verizon, Telstra and HERE's Hyderabad and Chennai location # 

##################################################################
if(!require(shiny))
{
  install.packages("shiny")
  library(shiny)
}
if(!require(shinythemes))
{
  install.packages("shinythemes")
  library(shinythemes)
} 
if(!require(shinyWidgets))
{
  install.packages("shinyWidgets")
  library(shinyWidgets)
}

shinyApp(    
  ui = fluidPage(   theme = shinytheme("sandstone"),  
                    
                    # Only show this panel if the prediction generation is required
                    fluidRow(column(4,div(tags$img(src='TM.png', align = "left",height = "80px"))),
                             column(8,strong(h1("Employee Attrition Prediction",align = "left",style = "color: rgb(222,51,59); font-family: 'Modern No. 20';text-decoration: none")))
                             ,style = "background-color: white;"),
                    fluidRow(
                      column(8,switchInput(inputId = "switch",onLabel = "Predictions",
                                           offLabel = "Sample Data",onStatus = TRUE,
                                           offStatus = TRUE),style = "margin-top: 10%; margin-left: 17%",align="center")
                    ),
                    conditionalPanel( condition = "input.switch==true",
                                      fluidRow(  
                                        column(5,
                                               wellPanel( 
                                                 fileInput("variable","*Upload CSV file containing employee data:",accept = c("text/csv","text/comma-separated-values","text/plain",".csv")),
                                                 #    actionButton("Submit", "Submit",class = "btn-primary")
                                                 downloadButton('downloadData', 'Generate Predictions',style = "margin: auto;")
                                                 #,
                                                 # style = "background-color: white; border: none"
                                               )
                                               ,align = "center",style = "margin-top: 10%; margin-left: 30%")
                                        #style = "background-color: white; border: none",
                                      )
                    ),
                    uiOutput("moreControls"),style = "background-color: rgb(255,240,255);"
                    
  ),
  
  
  server = function(input, output) 
  {
    
    
    OutputFILE <- reactive(
      quote(
        {
          if(!require(randomForest))
          {
            install.packages("randomForest")
            library(randomForest)
          }
          Truth <- read.csv(input$variable$datapath, header = T)
          
          RFW <- readRDS("Noida Attrition Prediction")
          
          Truth$Employee.ID <- NULL
          Truth$Date.of.Birth <- NULL
          Truth$Date.of.Joining.TechM <- NULL
          Truth$Date.Separated.from.TechM <- NULL
          Truth$Education <- NULL
          Truth$HomeTown.Flag <- Truth$HomeTown
          Truth$HomeTown <- NULL
          Truth$Consultant <- NULL
          
          
          Truth<-droplevels(Truth)
          
          Truth$HomeTown.Flag<-factor(as.character(Truth$HomeTown.Flag),levels =c("N" ,"Y"))
          Truth$Source.of.Hiring<-factor(as.character(Truth$Source.of.Hiring),levels = c("Agency", "Buddy" ,  "Campus" , "Direct"  , "Off Campus Recruitment" ,"Others" ))
          Truth$Delivery.Location<-factor(as.character(Truth$Delivery.Location),levels =c("CHANDIGARH","NOIDA"))
          Truth$Fresher<-factor(as.character(Truth$Fresher),levels=c("N","Y"))
          Truth$Professional<-factor(as.character(Truth$Professional),levels=c("N","Others" ,"Y" ))
          Truth$Technical<-factor(as.character(Truth$Technical),levels=c("N","Others" ,"Y" ))
          Truth$PostGraduate<-factor(as.character(Truth$PostGraduate),levels=c("N","Others" ,"Y" ))
          Truth$UnderGraduate<-factor(as.character(Truth$UnderGraduate),levels=c("N","Others" ,"Y" ))
          Truth$Graduate<-factor(as.character(Truth$Graduate),levels=c("N","Others" ,"Y" ))
          Truth$Marital.Status<-factor(as.character(Truth$Marital.Status),levels=c("Married" , "Single & Others"))
          Truth$Hired.for.Voice.or.Non.Voice.or.Both<-factor(as.character(Truth$Hired.for.Voice.or.Non.Voice.or.Both),levels=c("Non-Voice","Voice"  ))
          Truth$Account.Name<-factor(as.character(Truth$Account.Name),levels=c("Bell Canada" ,"Netgear" ,"Verizon"))
          Truth$Band<-factor(as.character(Truth$Band),levels=c("U1","U2"))
          Truth$MOJ<-factor(as.character(Truth$MOJ),levels=c("M1", "M10","M11","M12","M2", "M3", "M4", "M5", "M6", "M7", "M8", "M9" ))
          Truth$CTC.Hired.At <- as.numeric(Truth$CTC.Hired.At)
          
          pred <- predict(RFW,Truth[,-1],type="prob")
          
          
          as.data.frame(cbind(Truth,pred[,2]))
          
        }
      ),
      quoted=TRUE
    )
    
    output$downloadData  <- downloadHandler( filename = function() { paste(input$variable$name, ".csv", sep='') } , content = function(file) { write.csv(OutputFILE(), file)})
    
    output$moreControls <- renderUI({fluidRow(  
      column(5,
             wellPanel( if(!input$switch)
             {
               downloadButton('downloadData', 'Download Sample Data',style = "margin: auto;")
             }
             else{
               return(NULL)
             }),align = "center",style = "margin-top: 10%;margin-left: 30%"))})
  }
)
# input$file1 will be NULL initially. After the user selects
# and uploads a file, it will be a data frame with 'name',
# 'size', 'type', and 'datapath' columns. The 'datapath'
# column will contain the local filenames where the data can
# be found.  if (is.null(input$variable))




