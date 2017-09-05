#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(tidyr)

filename <- "Awards.csv"
data <- read.csv(filename, sep=",", header=T, stringsAsFactors=F) %>%
  subset(!duplicated(Abstract)) %>%
  subset(!duplicated(Title)) %>%
  select(AwardNumber, Title, Organization, AwardInstrument, AwardedAmountToDate, Abstract)
  #arrange(Abstract) %>% 
  #mutate(Abstract.Short = substr(Abstract,1,500))

nrows <- nrow(data)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  i <- reactive({ input$item })
  
  #it <- eventReactive(input$buttonNext, {
  #  input$item 
  #})
  
  #input$buttonNext
  # Use isolate() to avoid dependency
  #it <- isolate( reactive({ input$item + 1  })   )
  
   
  output$title <- renderText({ data$Title[i()] })
  output$awardnumber <- renderText({ data$AwardNumber[i()] })
  output$org <- renderText({ data$Organization[i()] })
  output$type <- renderText({ data$AwardInstrument[i()] })
  output$amount <- renderText({ data$AwardedAmountToDate[i()] })
  output$abstract <- renderText({data$Abstract[i()] })
 
  
  
})
