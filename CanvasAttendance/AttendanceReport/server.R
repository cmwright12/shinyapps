library(shiny)
library(shinydashboard)
library(dplyr)

server <- function(input, output) {
  
  # This reactive function will take the inputs from UI.R and use them for read.table() to read the data from the file. It returns the dataset in the form of a dataframe.
  # file$datapath -> gives the path of the file
  data <- reactive({
    file1 <- input$file
    if(is.null(file1)){return()} 
    #read.table(file=file1$datapath, sep=input$sep, header = input$header, stringsAsFactors = input$stringAsFactors)
    read.csv(file=file1$datapath, sep=",", header=T, stringsAsFactors=F)
  })
  
  courseID <- reactive({ data$SIS.Course.ID[1] })
  #selected <- reactive({ select(data, Student.Name, Class.Date, Attendance) })
  
  # This reactive output contains the dataset and display the dataset in table format
  output$rawtable <- renderTable({
    if(is.null(data())){return ()}
    data()
  })
  
  
  output$table <- renderTable({
    if(is.null(data())){return ()}
    data() %>%
      select(Student.Name, Class.Date, Attendance)
    
  })
  
  # the following renderUI is used to dynamically generate the tabsets when the file is loaded. Until the file is loaded, app will not show the tabset.
  output$tb <- renderUI({
    if(is.null(data()))
      h5(tags$img(src='jsulogorgb.jpg', height=196, width=600))
    else
      tabsetPanel(
        #tabPanel("About file", tableOutput("filedf")),
        tabPanel("Raw Data", tableOutput("rawtable")),
        tabPanel("Data", tableOutput("table"))
        #tabPanel("Summary", tableOutput("sum"))
        )
  })
  
}
