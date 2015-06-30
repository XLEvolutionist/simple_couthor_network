library(shiny)
library(ggplot2)

dataset <- diamonds

fluidPage(
  
  titlePanel("My network of co-authors"),
  
  sidebarPanel(
    
    sliderInput('minCitations', 'minimum number of citations for a link', min=1, max=50,
                value=1, step=1, round=0)
  ),
  mainPanel(
    plotOutput('plot')
  )
)