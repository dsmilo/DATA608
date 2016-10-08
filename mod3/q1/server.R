## Dan Smilowitz
## DATA 608 Assignment 3
## Question 1

library(shiny)
library(dplyr)
suppressPackageStartupMessages(library(googleVis))

data_loc <- 'https://raw.githubusercontent.com/jlaurito/CUNY_IS608/master/lecture3/data/cleaned-cdc-mortality-1999-2010.csv'
mort <- read.csv(data_loc, row.names = 1)
mort <- mort[, -c(1, 5, 7)] #all Notes are NA
names(mort)[1:2] <- c("Chapter", "Code")
names(mort)[ncol(mort)] <- "Rate"

shinyServer(function(input, output) {
  q1 <- reactive(mort %>% 
                   filter(Year == 2010, Code == input$Cause) %>% 
                   select(State, Rate) %>% arrange(desc(Rate))
                 )
  
  
  output$gvisplot <- renderGvis({
    gvisBarChart(q1(), chartid = "2010RatesbyState",
                 options = list(title = paste0("2010 Mortality Rates by State:\n", 
                                               as.character(mort$Chapter[mort$Code == input$Cause][1])),
                                vAxes = "[{textStyle:{fontSize: 9}}]",
                                hAxes = "[{textStyle:{fontSize: 10}}]",
                                height = 1200, width = 800,
                                chartArea = "{width: '75%', height: '90%'}",
                                titleTextStyle="{fontSize:18}", legend = "none"))
  })
})
