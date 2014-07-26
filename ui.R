library(shiny)
library(shinyIncubator)

shinyUI(fluidPage(theme='bootstrap.min.css',
  tags$style(type='text/css',
             'label {font-size: 12px;}',
             '.recalculating {opacity: 1.0;}'
  ),
  
  tags$h2("State sentiment analysis using twitter livestream"),
  
  progressInit(),  
  wellPanel(
    fluidRow(
      column(6, htmlOutput('header_map1')),
      column(6, htmlOutput('header_map2'))
      ),
    fluidRow(
      column(6, plotOutput('plot_map1')),
      column(6, plotOutput('plot_map2'))
      ),
    fluidRow(
      column(12, plotOutput('plot_trend'))
    ),
    p(htmlOutput('sample_tweets'))
  ),
  
  p(tags$a(href = 'http://www.runzemc.com', 'www.runzemc.com'))
))
