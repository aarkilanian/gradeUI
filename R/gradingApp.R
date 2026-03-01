run_grading <- function(){
  shiny::shinyApp(ui, server)
}

ui <- shiny::fluidPage(
  shiny::sliderInput(
    "n", "Sample size", 0, 100, 25
  ),
  shiny::plotOutput("hist")
)

server <- function(input, output){
  output$hist <- shiny::renderPlot({
    hist(rnorm(input$n))
  })
}

