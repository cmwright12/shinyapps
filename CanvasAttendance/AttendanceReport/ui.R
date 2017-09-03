library(shiny)
library(shinydashboard)
library(dplyr)
library(stringr)

dashboardPage(
  dashboardHeader(title= "Canvas Attendance Report"),
  dashboardSidebar(
    
    # Input: Select a file ----
    fileInput("file", "Choose CSV File",
              accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv")),
    
    # Horizontal line ----

    tags$hr(),
    
    # Menu ----
    sidebarMenu(
      menuItem(h3("Dashboard"), tabName="dashboard"),
      menuItem(h3("By Date"), tabName = "menu_date"),
      menuItem(h3("By Student"), tabName = "menu_student")
      #menuItem(h3("Attendance Report"), tabName = "menu_report")
    )
  ),
  dashboardBody(
    
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              h1(textOutput("courseID")),
              uiOutput("tb")
      ),
      
      # Second tab content
      tabItem(tabName = "menu_date",
              uiOutput("choose_day"),
              uiOutput("choose_status"),
              textOutput(length("by_day")),
              uiOutput("by_day")
      ),
      
      # Third tab content
      tabItem(tabName = "menu_student",
              uiOutput("choose_student"),
              uiOutput("by_student_rate"),
              uiOutput("by_student")
      )
      
    )
  )
)