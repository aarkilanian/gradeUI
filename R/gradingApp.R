##### Other resources #####

source("R/cards.R")

##### Main #####

ui <- bslib::page_sidebar(
  # Title
  shiny::titlePanel("Title"),
  # Sidebar content
  sidebar = bslib::sidebar(card_code, width = "40%"),
  # Main content
  card_student,
  card_rubric,
  # New footer
  fluidRow(
    column(width = 6, card_progress
    ), column(width = 6, card_shortcuts
    )
  )
)

server <- function(input, output){

  # Set reactive values
  rv <- shiny::reactiveValues(
    graded = 5,
    tot_student = 20,
    tot_question = 32,
    q_index = 1,
    grades = list()
  )

  output$progress <- shiny::renderUI({
    div(
      HTML("<p><b>Progress:</b> "),
      HTML(paste0(rv$graded/rv$tot_student*100), " % complete<br>"),
      HTML(paste0(rv$tot_student - rv$graded), " students remaining<br>"),
      HTML(paste0(rv$tot_student*rv$tot_question - length(rv$grades), " questions remaining")),
      HTML("</p>")
    )
  })

  output$shortcuts <- shiny::renderUI(
    div(
      HTML("<p><b>Shortcuts</b><br>"),
      shiny::icon("circle-right"),
      HTML(" (next question)<br>"),
      shiny::icon("circle-left"),
      HTML(" (previous question)<br>"),
      shiny::icon("arrow-up-1-9"),
      HTML(" (select rubric items)<br>"),
      HTML("</p>"),
    )
  )
}

run_grading <- function(){
  shiny::shinyApp(ui, server)
}

run_grading()
