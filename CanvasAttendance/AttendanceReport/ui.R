library(shiny)
library(shinydashboard)
library(dplyr)

dashboardPage(
  dashboardHeader(title= "Canvas Attendance Report"),
  dashboardSidebar(
    
    # Input: Select a file ----
    fileInput("file", "Choose CSV File",
              accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv")),
    
    # Horizontal line ----
    tags$hr()
  ),
  dashboardBody(
    uiOutput("tb")
  )
)