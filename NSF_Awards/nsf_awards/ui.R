#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
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

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  titlePanel("NSF Awards"),

  sidebarLayout(
    sidebarPanel(
      numericInput("item", "Item number: ", 1, min=1, max=nrows)
      #actionButton("buttonNext", "Next")
      #actionButton("buttonSave", "Save")
    ),
    
    
  mainPanel(
      h2(textOutput("awardnumber")),
      h2(textOutput("title")),
      h5(textOutput("org")),
      h5(textOutput("type")),
      h5(textOutput("amount")),
      h4(textOutput("abstract"))
    )
  )
))
