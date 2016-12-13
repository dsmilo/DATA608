library(shiny)
library(shinyjs)
library(V8)
library(plotly)
library(tidyverse)
library(shinythemes)
library(viridis)
library(markdown)

# load in prepared data
load('gentrify.Rda')

# set variables for custom plot creation
plot_vars <- list('Businesses Opened' = 'TotalNewBiz',
                  'Real Estate Value' = 'MedianPrice',
                  'Change in Real Estate Value' = 'PriceChange',
                  'Population' = 'Population',
                  'Percent Male' = 'Male',
                  'Percent White' = 'White',
                  'Median Age' = 'MedianAge',
                  'Median Income' = 'MedianIncome')

ui <- shinyUI(
  navbarPage(theme = shinytheme("flatly"),
    "Indicators of Changing Neighborhoods",
    tabPanel("Information",
             fluidPage(
               includeMarkdown("Information.md"))), 
    tabPanel("Visualization", 
             fluidPage(
               plotlyOutput('pyMainChart')
               )
             ),
    tabPanel('Data Exploration',
             fluidPage(
               sidebarPanel(
                 conditionalPanel(condition="input.conditionedPanels==1",
                                  selectInput(inputId = 'y_lin', 
                                              label = 'Select Variable to Plot', 
                                              choices = plot_vars,
                                              selected = 'PriceChange'),
                                  radioButtons(inputId = 'lin_scale', 
                                               label = 'Scale for y axis', 
                                               choices = list(
                                                 'Linear' = 'linear',
                                                 'Logarithmic' = 'log'),
                                               inline = TRUE),
                                  selectInput(inputId = 'neighborhood', 
                                              label = 'Select Neighborhood to Plot',
                                              choices = sort(
                                                unique(gentrify$Neighborhood)
                                              ),
                                              selected = 'Central Harlem')
                 ),
                 conditionalPanel(condition="input.conditionedPanels==2",
                                  selectInput(inputId = 'x_mot',
                                              label = 'Select Independent Variable',
                                              choices = plot_vars,
                                              selected = 'TotalNewBiz'),
                                  radioButtons(inputId = 'mot_scale_x', 
                                               label = 'Scale for x axis', 
                                               choices = list(
                                                 'Linear' = 'linear',
                                                 'Logarithmic' = 'log'),
                                               selected = 'log',
                                               inline = TRUE),
                                  selectInput(inputId = 'y_mot',
                                              label = 'Select Dependent Variable',
                                              choices = plot_vars,
                                              selected = 'PriceChange'),
                                  radioButtons(inputId = 'mot_scale_y', 
                                               label = 'Scale for y axis', 
                                               choices = list(
                                                 'Linear' = 'linear',
                                                 'Logarithmic' = 'log'),
                                               inline = TRUE),
                                  selectInput(inputId = 'size',
                                              label = 'Select Size Variable',
                                              choices = plot_vars,
                                              selected = 'Population'),
                                  selectInput(inputId = 'color_mot',
                                              label = 'Select Color Variable',
                                              choices = c(plot_vars,
                                                          list(
                                                            'Borough' = 
                                                              'Borough',
                                                            'Neighborhood' = 
                                                              'Neighborhood',
                                                            'ZipCode' = 
                                                              'ZipCode')
                                                          ),
                                              selected = 'Borough')
                 )
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Line Chart", value=1, 
                            plotlyOutput('pyLineChart')),
                   tabPanel("Motion Chart", value=2, 
                            plotlyOutput('pyMotionChart')),
                   id = "conditionedPanels"
               )
             ))
    ), tabPanel("References",
                fluidPage(
                  includeMarkdown("References.md"))))
  )


server <- shinyServer(function(input, output) {
  
  output$pyMainChart <- renderPlotly({
    gentrify %>%
      plot_ly(x = ~TotalNewBiz, y = ~PriceChange, size = ~Population, 
              text = ~ZipCode, hoverinfo = 'all', 
              alpha = 0.75, sizes = c(100, 2000), height = 600) %>%
      group_by(ZipCode) %>%
      layout(xaxis = list(type = 'log')) %>%
      add_markers(color = ~Borough, frame = ~Year, colors = rev(viridis(4))) %>%
      animation_opts(frame = 2000, easing = 'cubic-in-out') %>%
      layout(title = paste0(
        'Change in Real Estate Value vs. Opening of New Businesses',
        '<br>', 'in New York City by Zip Code, 2004-2014'),
        legend = list(x = 0, y = 1, orientation = 'h'),
        xaxis = list(title = 'New Business Opened per Year'),
        yaxis = list(title = 'Increase in Median Home Value', 
                     tickformat = '.0%'),
        margin = list(l = 50, r = 50, b = 0, t = 100, pad = 1))
  })
  
  output$pyMotionChart <- renderPlotly({
    gentrify %>%
      plot_ly(x = as.formula(paste0('~', input$x_mot)),
              y = as.formula(paste0('~', input$y_mot)),
              size = as.formula(paste0('~', input$size)), sizes = c(10, 100),
              text = ~ZipCode, hoverinfo = 'all', alpha = 0.75,
              height = 600) %>%
      group_by(ZipCode) %>%
      layout(xaxis = list(type = input$mot_scale_x),
             yaxis = list(type = input$mot_scale_y)) %>%
      add_markers(color = as.formula(paste0('~', input$color_mot)), 
                  frame = ~Year, colors = rev(viridis(4))) %>%
      animation_opts(frame = 2000, easing = 'cubic-in-out') %>%
      layout(margin = list(l = 100, r = 100, b = 50, t = 50, pad = 1))
  })

  output$pyLineChart <- renderPlotly({
    gentrify %>% 
      filter(Neighborhood == input$neighborhood) %>%
      plot_ly(x = ~Year, y = as.formula(paste0('~', input$y_lin)),
              text = ~ZipCode, hoverinfo = 'all', 
              color = ~Borough, colors = rev(viridis(4))) %>%
      group_by(ZipCode) %>% add_lines() %>%
      layout(margin = list(l = 100, r = 100, b = 50, t = 50, pad = 1),
             yaxis = list(type = input$lin_scale))
  })
  
  
})

shinyApp(ui, server)
