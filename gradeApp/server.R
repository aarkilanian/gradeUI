function(input, output, session){

  # Set reactive values
  rv <- shiny::reactiveValues(
    tot_student = length(students),
    tot_question = length(answers),
    q_index = 1,
    s_index = 1,
    graded = 0,
    cur_rub_sel = NULL
  )

  ##### RUBRIC #####

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

  # Capture rubric choice
  observeEvent(input$rubric_select, {

    # Get previous selections
    prev_sel <- rv$cur_rub_sel

    # Set rubric to selections
    cur_sel <- input$rubric_select
    # Get newest addition
    if(is.null(prev_sel)){
      new_sel <- cur_sel
    } else {
      new_sel <- setdiff(cur_sel, prev_sel)
    }

    # Find corresponding rubric item
    rub_row <- which(rubric$question == rv$q_index & rubric$key == new_sel)
    # If not a rubric item return nothing
    if(length(rub_row) == 0) return()

    #
    if(any(new_sel %in% c("0", "1"))){
      new_selection <- new_sel
    } else if(any(prev_sel %in% c("0", "1"))){
      new_selection <- new_sel
    } else{
      new_selection <- cur_sel
    }

    # Update checkboxes
    updateCheckboxGroupInput(
      session,
      "rubric_select",
      selected = new_selection
    )

    # Update stored value
    rv$cur_rub_sel <- new_selection

  })

  # Modify rubric choice with keyboard
  observeEvent(input$key_rubric, {

    # Get previous selections
    prev_sel <- rv$cur_rub_sel

    # Get current selection
    cur_sel <- input$key_rubric

    # Find corresponding rubric item
    rub_row <- which(rubric$question == rv$q_index & rubric$key == cur_sel)
    # If not a rubric item return nothing
    if(length(rub_row) == 0) return()

    # Compose new selection
    if(any(cur_sel %in% c("0", "1"))){
      new_selection <- cur_sel
    } else if(any(prev_sel %in% c("0", "1"))){
      new_selection <- cur_sel
    } else {
      new_selection <- union(prev_sel, cur_sel)
    }

    # Update checkboxes
    updateCheckboxGroupInput(
      session,
      "rubric_select",
      selected = new_selection
    )

    # Update stored value
    rv$cur_rub_sel <- new_selection

  })

  ##### DISPLAYS #####

  # Title: current student and question
  output$student_question <- renderUI({
    HTML(paste0("Student ", rv$s_index, " | ", "Question ", rv$q_index))
  })

  # Question score
  output$score_display <- renderText({
    paste("Question score: ", score())
  })

  # Question score calculator
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

  # Autograding comments
  output$auto_comments <- renderUI({
    students[[rv$s_index]][[rv$q_index]]$auto_message
  })

  # Grading progress
  output$progress <- shiny::renderUI({
    div(
      HTML("<p><b>Progress:</b><br>"),
      HTML(paste0(rv$graded/rv$tot_student*100), " % complete<br>"),
      HTML(paste0(rv$tot_student - rv$graded), " students remaining<br>"),
      HTML(paste0(rv$tot_student*rv$tot_question - length(rv$grades), " questions remaining")),
      HTML("</p>")
    )
  })

  # Shortcut guide
  output$shortcuts <- shiny::renderUI({
    div(
      HTML("<p><b>Shortcuts:</b><br>"),
      shiny::icon("circle-right"),
      HTML(" (next question)<br>"),
      shiny::icon("circle-left"),
      HTML(" (previous question)<br>"),
      shiny::icon("arrow-up-1-9"),
      HTML(" (select rubric items)<br>"),
      HTML("</p>"),
    )
  })

  ##### ANSWER AND SOLUTION #####

  output$s_call <- renderText({
    as.character(students[[rv$s_index]][[rv$q_index]]$answer_call)
  })

  output$s_answer <- renderPrint({
    students[[rv$s_index]][[rv$q_index]]$answer
  })

  output$a_call <- renderText({
    as.character(answers[[rv$q_index]]$answer_call)
  })

  output$a_answer <- renderPrint({
    answers[[rv$q_index]]$answer
  })

  ##### EDITOR #####

  # Get student script
  student_script <- reactive({
    assignment_path <- students[[rv$s_index]]$path
    script_text <- paste(readLines(assignment_path), collapse = '\n')
    return(script_text)
  })

  observeEvent(student_script(), {
    updateAceEditor(session, "code", value = student_script())
  })

  # Reset code
  observeEvent(input$reset, {
    updateAceEditor(session, "code", value = student_script())
  })

 # Evaluate expression when pressing "run"
  evaled_call <- eventReactive(input$run, {
    eval(parse(text = isolate(input$code_selection)))
  })

 # Render expression output
 output$code_output <- renderPrint({
   evaled_call()
 })

  ##### NAVIGATION #####

  # Change question (forward)
  observeEvent(input$key_next, {
    # If at end of questions
    if(rv$q_index == rv$tot_question){
      # If at last student, wrap around to start
      if(rv$s_index == rv$tot_student){
        rv$q_index <- 1
        rv$s_index <- 1
        # If not go to next student
      } else{
        rv$q_index <- 1
        rv$s_index <- rv$s_index + 1
      }
      # Otherwise increment question
    } else{
      rv$q_index <- rv$q_index + 1
    }
  })

  # Change question (backwards)
  observeEvent(input$key_prev, {
    # If at start of questions go to previous student
    if(rv$q_index == 1){
      # If at first student, wrap around to last
      if(rv$s_index == 1){
        rv$q_index <- rv$tot_question
        rv$s_index <- rv$tot_student
      } else{
        rv$q_index <- rv$tot_question
        rv$s_index <- rv$s_index - 1
      }
    } else{
      rv$q_index <-  rv$q_index - 1
    }
  })

  # # Change student (forward)
  # observeEvent(input$key_nexts, {
  #   # If at end of students, don't change
  #   if(rv$s_index == rv$tot_student){
  #     rv$s_index <- rv$s_index
  #   } else{
  #     rv$s_index <- rv$s_index + 1
  #   }
  # })
  #
  # # Change student (backwards)
  # observeEvent(input$key_prevs, {
  #   # If at start of students, don't change
  #   if(rv$s_index == 1){
  #     rv$s_index <- rv$s_index
  #   } else{
  #     rv$s_index <- rv$s_index - 1
  #   }
  # })

}
