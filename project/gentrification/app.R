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
    # tab containing project information
    tabPanel("Information",
             fluidPage(
               includeMarkdown("Information.md"))), 
    # tab containing main visualization
    tabPanel("Visualization", 
             fluidPage(
               plotlyOutput('pyMainChart')
               )
             ),
    # tab containing exploratory visualizations
    tabPanel('Data Exploration',
             fluidPage(
             	# tabbed panel within exploration tab with different sidebar
             	sidebarPanel(
                 # for line plots, select variable, y scale, neighborhood
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
                 # for motion plot, select x/y variables/scales plus size/color
                 # default to same options as main visualization
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
                   # show line chart
                   tabPanel("Line Chart", value=1, 
                            plotlyOutput('pyLineChart')),
                   # show motion chart
                   tabPanel("Motion Chart", value=2, 
                            plotlyOutput('pyMotionChart')),
                   id = "conditionedPanels"
               )
             ))
    ), tabPanel("References", # tab with references
                fluidPage(
                  includeMarkdown("References.md"))))
  )


server <- shinyServer(function(input, output) {
  
  # create main chart using plotly
  output$pyMainChart <- renderPlotly({
    gentrify %>%
      # create plotly graph with price change vs. new businesses
      # and adjust visual parameters
      plot_ly(x = ~TotalNewBiz, y = ~PriceChange, size = ~Population, 
              text = ~Neighborhood, hoverinfo = 'all', 
              alpha = 0.75, sizes = c(10, 100), height = 600) %>%
      group_by(ZipCode) %>%
      # set x axis to log scale
      layout(xaxis = list(type = 'log')) %>%
      # color by borough; animate by year
      add_markers(color = ~ZipCode, frame = ~Year, colors = rev(viridis(5))) %>%
      # set duration and type of transition between years
      animation_opts(frame = 2000, easing = 'cubic-in-out') %>%
      # set layout parameters (titles, legend, formatting)
      layout(title = paste0(
        'Change in Real Estate Value vs. Opening of New Businesses',
        '<br>', 'in New York City by Zip Code, 2004-2014'),
        showlegend = FALSE,
        xaxis = list(title = 'New Business Opened per Year'),
        yaxis = list(title = 'Increase in Median Home Value', 
                     tickformat = '.0%'),
        margin = list(l = 50, r = 50, b = 0, t = 100, pad = 1))
  })
  
  # create motion chart based on selections
  output$pyMotionChart <- renderPlotly({
    gentrify %>%
      # create plotly graph using selected x, y, and size variables
      plot_ly(x = as.formula(paste0('~', input$x_mot)),
              y = as.formula(paste0('~', input$y_mot)),
              size = as.formula(paste0('~', input$size)), sizes = c(10, 100),
              text = ~ZipCode, hoverinfo = 'all', alpha = 0.75,
              height = 600) %>%
      group_by(ZipCode) %>%
      # scale axes based on selections
      layout(xaxis = list(type = input$mot_scale_x),
             yaxis = list(type = input$mot_scale_y)) %>%
      # color points based on selection
      add_markers(color = as.formula(paste0('~', input$color_mot)), 
                  frame = ~Year, colors = rev(viridis(4))) %>%
      # set animation and layout parameters
      animation_opts(frame = 2000, easing = 'cubic-in-out') %>%
      layout(margin = list(l = 100, r = 100, b = 50, t = 50, pad = 1))
  })

  # create line chart based on selections
  output$pyLineChart <- renderPlotly({
    gentrify %>% 
      # select neighborhood based on selection
      filter(Neighborhood == input$neighborhood) %>%
      # create plotly line graph based on selected y variable
      plot_ly(x = ~Year, y = as.formula(paste0('~', input$y_lin)),
              text = ~ZipCode, hoverinfo = 'all', 
              color = ~Borough, colors = rev(viridis(4))) %>%
      group_by(ZipCode) %>% add_lines() %>%
      # set layout parameters
      layout(margin = list(l = 100, r = 100, b = 50, t = 50, pad = 1),
             yaxis = list(type = input$lin_scale), hovermode = 'closest')
  })
  
  
})

shinyApp(ui, server)
