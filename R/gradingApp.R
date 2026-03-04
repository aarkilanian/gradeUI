##### Resources #####

source("R/cards.R")
rubric <- read.csv("ex_rubric.csv")

##### UI #####

ui <- bslib::page_sidebar(

  # Add keyboard shortcuts
  tags$script(includeHTML("key_shortcuts.html")),

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

##### Server #####

server <- function(input, output, session){

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

  # Set current rubric selection
  cur_rub_sel <- reactiveVal(NULL)

  # Get current student and question
  output$student_question <- renderUI({
    HTML(paste0("Student ", rv$s_index, " | ", "Question ", rv$q_index))
  })

  # RUBRIC SETUP
  # Render checkboxes for current question
  output$rubric_checkbox <- renderUI({
    choices <- get_choices()
    checkboxGroupInput("rubric_select", "Rubric:", choices = choices)
  })

  # Get choices for current question from rubric csv
  get_choices <- reactive({
    # Subset rubric to current question
    sub_rubric <- rubric[rubric$question == rv$q_index,]
    choices <- setNames(sub_rubric$key,
                        paste0(sub_rubric$key,
                               ". ",
                               sub_rubric$comment,
                               " (-",
                               sub_rubric$deduction,
                               ")"))
    return(choices)
  })

  # Capture rubric choice from keyboard
  observeEvent(list(input$key_rubric, input$rubric_select), {

    if(!is.null(input$key_rubric)){
      selected <- input$key_rubric
    } else{
      if(input$rubric_select == cur_rub_sel()){
        return()
      }
      else{
        selected <- input$rubric_select
      }
    }

    # Find corresponding rubric item
    rub_row <- which(rubric$question == rv$q_index & rubric$key == selected)
    # If not a rubric item return nothing
    if(length(rub_row) == 0) return()

    # Get previous selections
    prev_sel <- cur_rub_sel()

    # If no previous value
    if(is.null(prev_sel)){
      new_selection <- selected
    } else if (any(c(0,1) %in% prev_sel) | any(c(0,1) %in% selected)){
      new_selection <- selected
    } else{
      if(selected %in% prev_sel){
        # Remove selection if already checked
        new_selection <- setdiff(prev_sel, selected)
      } else{
        # Otherwise add to selections
        new_selection <- c(selected, prev_sel)
      }
    }

    # Update checkboxes
    updateCheckboxGroupInput(
      session,
      "rubric_select",
      selected = new_selection
    )

    # Update stored value
    cur_rub_sel(new_selection)

  })

  # SCORE CALCULATION
  output$score_display <- renderText({
    paste("Question score: ", score())
  })

  score <- reactive({
    # Calculate total question marks
    total_marks <- rubric[rubric$question == rv$q_index,]
    total_marks <- total_marks[1,5]
    # Get selected rubric items
    selected <- input$rubric_select
    if(is.null(selected)) return()
    sub_rubric <- rubric[rubric$question == rv$q_index,]
    # Get and sum deductions
    selected_ded <- sub_rubric[sub_rubric$key %in% selected,]
    selected_ded <- selected_ded$deduction
    return(total_marks - sum(selected_ded))
  })

  # CARD LAYOUTS
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

  output$shortcuts <- shiny::renderUI({
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
  })

  # NAVIGATION
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

}

run_grading <- function(){
  shiny::shinyApp(ui, server)
}

run_grading()
