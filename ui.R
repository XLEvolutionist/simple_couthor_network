library(shiny)

dataset <- diamonds

fluidPage(theme = "sandstone.css",
  
  titlePanel("co-authorship networks"),
  
  sidebarPanel(
    
    sliderInput('minCitations', 'minimum number of co-authorships', min=1, max=50,
                value=1, step=1, round=0),
    sliderInput('col', 'node color', min=1, max=24,
                value=1, step=1, round=0),
    sliderInput('trans', 'node transparency', min=0, max=1,
                value=0.8, step=0.1, round=1),
    sliderInput('colE', 'edge color', min=1, max=52,
                value=1, step=1, round=0),
    sliderInput('lwd', 'edge width', min=1, max=16,
                value=3, step=1, round=0),
    sliderInput('textSize', 'text size', min=0.1, max=4,
                value=1, step=0.1, round=0),
    sliderInput('off', 'text offset', min=0, max=2,
                value=0, step=0.1, round=0),
    #uiOutput("choose_dataset"),
    selectInput("dims", "dimesions", c(2,3), selected = 2),
    actionButton("goButton", "redraw")
  ),
  mainPanel(
    plotOutput('plot')
  )
)