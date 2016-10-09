## Dan Smilowitz
## DATA 608 Assignment 3
## Combined Questions 1 & 2

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
  
  output$q1plot <- renderGvis({
    gvisBarChart(q1(), chartid = "2010RatesbyState",
                 options = list(title = paste0("2010 Mortality Rates by State:\n", 
                                               as.character(mort$Chapter[mort$Code == input$Cause][1])),
                                vAxes = "[{textStyle:{fontSize: 9}}]",
                                hAxes = "[{textStyle:{fontSize: 10}}]",
                                height = 1200, width = 800,
                                chartArea = "{width: '75%', height: '90%'}",
                                titleTextStyle="{fontSize:18}", legend = "none"))
  })
  
  nat_rate <- reactive(mort %>% 
                         filter(Code == input$Cause) %>%
                         group_by(Year) %>% summarize(Rate = sum(Population * Rate) / sum(Population)))
  
  nat_change <- reactive(nat_rate() %>% 
                           mutate(Change = (Rate - Rate[Year == 1999]) / Rate[Year == 1999]) %>% 
                           filter(Year == 2010) %>% select(Change))
  
  q2 <- reactive(mort %>% filter(Code == input$Cause) %>% 
                   group_by(State) %>% select(State, Year, Rate) %>%
                   mutate(Change = (Rate - Rate[Year == 1999]) / Rate[Year == 1999]) %>% 
                   filter(Year == 2010) %>% 
                   mutate(Relative = Change - nat_change()$'Change') %>%
                   select(State, Relative) %>% arrange(Relative))
  
  output$q2plot <- renderGvis({
    gvisBarChart(q2(), chartid = "ChangeVsNational", 
                 options = list(title = paste0("Mortality Rate Change Relative to National Change 1999-2010:",
                                               "\n", mort$Chapter[mort$Code == input$Cause][1],
                                               " (National Change: ",
                                               as.character(round(nat_change()$'Change', 4)*100),"%)"),
                                hAxes = "[{title:'Improvement Rate Difference from National Average Improvement, 1999-2010', format:'##%'}]",
                                vAxes = "[{textStyle:{fontSize: 9}}]",
                                height = 1200, width = 800,
                                chartArea = "{width: '75%', height: '90%'}",
                                titleTextStyle="{fontSize:18}", legend = "none"))
  })
})
