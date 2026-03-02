##### Other resources #####

source("R/cards.R")


##### Main #####

ui <- bslib::page_sidebar(

  # Add keyboard shortcuts
  tags$script(HTML("
    document.addEventListener('keydown', function(e) {

      // Question navigation
      if (e.key === 'ArrowRight') {
        Shiny.setInputValue('key_next', Math.random());
      }
      if (e.key === 'ArrowLeft') {
        Shiny.setInputValue('key_prev', Math.random());
      }

      // Student navigation
      if (e.key === 'ArrowUp') {
        Shiny.setInputValue('key_nexts', Math.random());
      }
      if (e.key === 'ArrowDown') {
        Shiny.setInputValue('key_prevs', Math.random());
      }

      // Rubric selection
      if (e.key === '1') {
        Shiny.setInputValue('key_1', Math.random());
      }
      if (e.key === 'Digit1') {
        Shiny.setInputValue('key_1', Math.random());
      }
      if (e.key === '2') {
        Shiny.setInputValue('key_2', Math.random());
      }
      if (e.key === 'Digit2') {
        Shiny.setInputValue('key_2', Math.random());
      }
      if (e.key === '3') {
        Shiny.setInputValue('key_3', Math.random());
      }
      if (e.key === 'Digit3') {
        Shiny.setInputValue('key_3', Math.random());
      }
      if (e.key === '4') {
        Shiny.setInputValue('key_4', Math.random());
      }
      if (e.key === 'Digit4') {
        Shiny.setInputValue('key_4', Math.random());
      }
      if (e.key === '5') {
        Shiny.setInputValue('key_5', Math.random());
      }
      if (e.key === 'Digit5') {
        Shiny.setInputValue('key_5', Math.random());
      }
      if (e.key === '6') {
        Shiny.setInputValue('key_6', Math.random());
      }
      if (e.key === 'Digit6') {
        Shiny.setInputValue('key_6', Math.random());
      }
      if (e.key === '7') {
        Shiny.setInputValue('key_7', Math.random());
      }
      if (e.key === 'Digit7') {
        Shiny.setInputValue('key_7', Math.random());
      }
      if (e.key === '8') {
        Shiny.setInputValue('key_8', Math.random());
      }
      if (e.key === 'Digit8') {
        Shiny.setInputValue('key_8', Math.random());
      }
      if (e.key === '9') {
        Shiny.setInputValue('key_9', Math.random());
      }
      if (e.key === 'Digit9') {
        Shiny.setInputValue('key_9', Math.random());
      }
      if (e.key === '0') {
        Shiny.setInputValue('key_0', Math.random());
      }
      if (e.key === 'Digit0') {
        Shiny.setInputValue('key_0', Math.random());
      }

      // Custom rubric
      if (e.key === 'c') {
        Shiny.setInputValue('key_custom', Math.random());
      }
    });
  ")),

  # Add theme
  theme = my_theme,

  # Title
  shiny::titlePanel(shiny::htmlOutput("student_question")),

  # Sidebar content
  sidebar = bslib::sidebar(card_code, width = "40%"),

  # Main content
  layout_column_wrap(
    width = 1/2,
    card_student,
    card_solution
  ),
  uiOutput("auto_comments"),
  card_rubric,

  # Footer
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
    s_index = 1,
    grades = list(),
    autocomment = c("Yes this is an error.")
  )

  # Get current student and question
  output$student_question <- renderUI({
    HTML(paste0("Student ", rv$s_index, " | ", "Question ", rv$q_index))
  })

  # Change question (forward)
  observeEvent(input$key_next, {
    # If at end of questions
    if(rv$q_index == rv$tot_question){
      # If at last student, don't change
      if(rv$s_index == rv$tot_student){
        rv$q_index <- rv$q_index
      # If not go to next student
      } else{
        rv$q_index <- 1
        rv$s_index <- rv$s_index + 1
      }
    # Otherwise increment question
    } else{
      rv$q_index <-  rv$q_index + 1
    }
  })

  # Change question (backwards)
  observeEvent(input$key_prev, {
    # If at start of questions go to previous student
    if(rv$q_index == 1){
      # If at first student, don't change
      if(rv$s_index == 1){
        rv$q_index <- rv$q_index
      } else{
        rv$q_index <- rv$tot_question
        rv$s_index <- rv$s_index - 1
      }
    } else{
      rv$q_index <-  rv$q_index - 1
    }
  })

  # Change student (forward)
  observeEvent(input$key_nexts, {
    # If at end of students, don't change
    if(rv$s_index == rv$tot_student){
      rv$s_index <- rv$s_index
    } else{
      rv$s_index <- rv$s_index + 1
    }
  })

  # Change student (backwards)
  observeEvent(input$key_prevs, {
    # If at start of students, don't change
    if(rv$s_index == 1){
      rv$s_index <- rv$s_index
    } else{
      rv$s_index <- rv$s_index - 1
    }
  })

  output$auto_comments <- renderUI({
    if(!is.na(rv$autocomment[rv$q_index])){
      card_comment
    }
  })

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
