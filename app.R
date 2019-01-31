library(shiny)
library(reactrend)

ui <- fluidPage(
  titlePanel("reactR HTMLWidget Example"),
  reactrendOutput('widgetOutput')
)

server <- function(input, output, session) {
  output$widgetOutput <- renderReactrend(
    reactrend("Hello world!")
  )
}

shinyApp(ui, server)