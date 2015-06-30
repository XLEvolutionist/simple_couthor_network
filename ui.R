library(shiny)
library(ggplot2)

dataset <- diamonds

fluidPage(theme = "sandstone.css",
  
  titlePanel("My network of co-authors"),
  
  sidebarPanel(
    
    sliderInput('minCitations', 'minimum number of co-authorships', min=1, max=50,
                value=1, step=1, round=0),
    sliderInput('col', 'node color', min=1, max=24,
                value=1, step=1, round=0),
    sliderInput('colE', 'edge color', min=1, max=52,
                value=1, step=1, round=0),
    sliderInput('lwd', 'edge width', min=1, max=16,
                value=3, step=1, round=0),
    sliderInput('textSize', 'text size', min=0.1, max=4,
                value=1, step=0.1, round=0),
    
    actionButton("goButton", "redraw")
  ),
  mainPanel(
    plotOutput('plot')
  )
)