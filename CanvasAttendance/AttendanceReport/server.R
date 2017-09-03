library(shiny)
library(shinydashboard)
library(dplyr)
library(stringr)

server <- function(input, output) {
  
  # This reactive function will take the inputs from UI.R and use them for read.csv() to read the data from the file. It returns the dataset in the form of a dataframe.
  # file$datapath -> gives the path of the file
  data <- reactive({
    file1 <- input$file
    if(is.null(file1)){return()} 
    read.csv(file=file1$datapath, sep=",", header=T, stringsAsFactors=F) 
  })

  #order_by_lastname <- function(data){
  #  data <- data %>%
  #    dplyr::mutate(Last.Name = str_extract(Student.Name, "[A-Za-z'-]+$"))
  #  data <- data %>%
  #    dplyr::mutate(First.Name = str_extract(Student.Name, "^[A-Za-z'-]+"))
  #  data <- data %>%
  #    arrange(Class.Date, Last.Name, First.Name)
  #  return(data[,-c(12,13)])
  #}
    
  #data <- reactive({
  #  if(is.null(data())){return ()}
  #  data <- order_by_lastname(data()) 
  #  data()
  #  })

  
  output$courseID <- renderText({ 
    if(is.null(data())){return("")}
    data()$SIS.Course.ID[1] 
    })
  
  output$days <- reactive({
    if(is.null(data())){return("")}
    unique(data()$Class.Date)
  })
  output$ndays <- reactive({
    length(unique(data()$Class.Date))
  })
  
  output$choose_day <- renderUI({
    selectInput("day", "Choose a Date", as.list(data()$Class.Date))
  })
  
  output$choose_status <- renderUI({
    checkboxGroupInput("status", "Show:", choices=as.list(unique(data()$Attendance)), selected=as.list(unique(data()$Attendance)) )
  })
  
  output$statuses <- reactive({
    if(is.null(data())){return("")}
    unique(data()$Class.Attendance)
  })

  output$students <- renderTable({
    if(is.null(data())){return("")}
    data() %>%
      dplyr::mutate(Last.Name = str_extract(Student.Name, "[A-Za-z'-]+$")) %>%
      dplyr::mutate(First.Name = str_extract(Student.Name, "^[A-Za-z'-]+")) %>%
      arrange(Last.Name, First.Name) %>%
      distinct(Student.Name)
  })
  output$nstudents <- reactive({
    data() %>%
      dplyr::mutate(Last.Name = str_extract(Student.Name, "[A-Za-z'-]+$")) %>%
      dplyr::mutate(First.Name = str_extract(Student.Name, "^[A-Za-z'-]+")) %>%
      arrange(Last.Name, First.Name) %>%
      distinct(Student.Name) %>%
      nrow()
  })
  
  # This reactive output contains the dataset and display the dataset in table format
  output$rawtable <- renderTable({
    if(is.null(data())){return ()}
    data()
  })
  
  
  output$table <- renderTable({
    if(is.null(data())){return ()}
    data() %>%
      dplyr::mutate(Last.Name = str_extract(Student.Name, "[A-Za-z'-]+$")) %>%
      dplyr::mutate(First.Name = str_extract(Student.Name, "^[A-Za-z'-]+")) %>%
      arrange(Class.Date, Last.Name, First.Name) %>%
      select(Student.Name, Class.Date, Attendance)
  })
  
  output$by_day <- renderTable({
    if(is.null(data())){return ()}
    data() %>%
      filter(Class.Date == input$day, Attendance %in% input$status) %>%
      dplyr::mutate(Last.Name = str_extract(Student.Name, "[A-Za-z'-]+$")) %>%
      dplyr::mutate(First.Name = str_extract(Student.Name, "^[A-Za-z'-]+")) %>%
      arrange(Last.Name, First.Name) %>%
      select(Student.Name, Attendance)
  })
  
  
  # the following renderUI is used to dynamically generate the tabsets when the file is loaded. Until the file is loaded, app will not show the tabset.
  output$tb <- renderUI({
    if(is.null(data()))
      h5(tags$img(src='jsulogorgb.jpg', height=196, width=600))
    else
      tabsetPanel(
        #tabPanel("About file", tableOutput("filedf")),
        tabPanel("Raw Data", tableOutput("rawtable")),
        tabPanel("Data", tableOutput("table")),
        tabPanel("Summary", 
                 h5("No. of Dates: "), textOutput("ndays"),
                 h5("Dates of Report"), textOutput("days"),
                 h5("No. of students: "), textOutput("nstudents"),
                 tableOutput("students"))
        )
  })
  
}
