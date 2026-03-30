function(input, output, session){

  # Set reactive values
  rv <- shiny::reactiveValues(
    q_index = 1,
    s_index = 1,
    cur_rub_sel = NULL
  )

  ##### DISPLAYS #####

  # Title: current student and question
  output$student_question <- renderUI({
    HTML(paste0("Student ", rv$s_index, " | ", "Question ", rv$q_index))
  })

  # Question score
  output$score_display <- renderText({
    paste("Question score: ", score())
  })

  # Flagging
  observe({
    #curFlag <- gradelog[[rv$s_index]][[rv$q_index]]$flagged
    req(rv$s_index)
    req(rv$q_index)
    curFlag <- TRUE
    updateCheckboxInput(session, inputId = "flag", value = curFlag)
  })

  # Question score calculator
  score <- reactive({
    # Calculate total question marks
    total_marks <- rubric[rubric$question == rv$q_index,]
    total_marks <- total_marks[1,5]
    # Get selected rubric items
    selected <- rv$cur_rub_sel
    if(is.null(selected)) return()
    sub_rubric <- rubric[rubric$question == rv$q_index,]
    # Get and sum deductions
    selected_ded <- sub_rubric[sub_rubric$key %in% selected,]
    selected_ded <- selected_ded$deduction
    return(total_marks - sum(selected_ded))
  })

  # Autograding comments
  output$auto_comments <- renderUI({
    mess <- students[[rv$s_index]][[rv$q_index]]$auto_message
    if(is.na(mess)) "" else paste0("Autograder: ", mess)
  })

  # Grading progress
  output$progress <- shiny::renderUI({
    div(
      HTML(paste0(rv$graded/tot_student*100), " % complete<br>"),
      HTML(paste0(tot_student - rv$graded), " students remaining<br>"),
      HTML(paste0(tot_student*tot_question - length(rv$grades), " questions remaining")),
      HTML("</p>")
    )
  })

  ##### SAVE & LOAD #####

  # Check and read log if changed
  logData <- reactiveFileReader(100, session, log_path, readRDS) %>%
    bindEvent(rv$q_index, rv$s_index)

  # Save progress to log when current rubric reactive changes
  observe( {
    # Get current student and question
    curS <- isolate(rv$s_index)
    curQ <- isolate(rv$q_index)
    print(paste0(curS, curQ))
    # If choices are unchanged don't save
    logChoices <- gradelog[[curS]][[curQ]]$choices
    n_choices <- length(logChoices)
    if(n_choices == 0){ # TODO this fucked
      # Do nothing
    } else if (all(logChoices == rv$cur_rub_sel)) {
      # Do nothing
    } else {
      gradelog[[curS]][[curQ]]$choices <- rv$cur_rub_sel
      gradelog[[curS]][[curQ]]$date <- date()
      saveRDS(gradelog, log_path)
    }
  })

  # Output last saved grade date if any
  output$grading_status <- renderUI({
    curLog <- logData()
    cur_q_log <- curLog[[rv$s_index]][[rv$q_index]]
    cur_q_log$date
  })

  ##### RUBRIC #####

  # Display comment
  observe({
    curMess <- gradelog[[rv$s_index]][[rv$q_index]]
    updateTextInput(session, "ta_comment", value = curMess)
  })

  # Render checkboxes for current question
  output$rubric_checkbox <- renderUI({
    rub_choices <- get_choices()
    prev_sel <- logged_choice()
    radioButtons("rubric_select",
                  "Rubric:",
                  choices = rub_choices,
                  selected = prev_sel)
  })

  # Get choices for current question from rubric
  get_choices <- reactive({
    # Get current log
    curLog <- rv$log
    # Subset rubric to current question
    req(rv$s_index)
    sub_rubric <- rubric[rubric$question == rv$q_index,]
    choices <- setNames(sub_rubric$key,
                        paste0(sub_rubric$key,
                               ". ",
                               sub_rubric$comment,
                               " (-",
                               sub_rubric$deduction,
                               ")"))
    # Return result
    return(choices)
  })

  # Get saved rubric selection
  logged_choice <- reactive({
    # Get log
    curLog <- rv$log
    # Get current rubric key
    curRub <- curLog[[rv$s_index]][[rv$q_index]]$key
    # Save as reactive
    # rv$cur_rub_sel <- curRub
    # Return
    return(curRub)
  })

  # Capture rubric choice with mouse
  observe({

    # Get current selection
    cur_sel <- input$rubric_select

    # Update checkboxes
    updateRadioButtons(
      session,
      "rubric_select",
      selected = input$rubric_select)

    # Update log and grade
  })

  # # Modify rubric choice with keyboard
  # observeEvent(input$key_rubric, {
  #
  #   # Get previous selections
  #   prev_sel <- logged_choice()
  #   # Get current selection
  #   cur_sel <- input$key_rubric
  #
  #   # Update checkboxes
  #   updateRadioButtons(
  #     session,
  #     "rubric_select",
  #     selected = cur_sel)
  #
  #   # Update log and grade
  # })

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

  # Get student script when student changes
  student_script <- reactive({
    assignment_path <- students[[rv$s_index]]$path
    script_text <- paste(readLines(assignment_path), collapse = '\n')
    return(script_text)
  })

  # Update script in editor
  observeEvent(student_script(), {
    updateAceEditor(session, "code", value = student_script())
  })

  # Reset script
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

  ##### QUESTION NAVIGATION #####

  # Change student (forward)
  observeEvent(input$key_next, {

    # If not at at end of students, go to next student
    if(rv$s_index != tot_student){
      rv$s_index <- rv$s_index + 1

    } else{

      # If not at end of questions, got to next question
      if (rv$q_index != tot_question) {
        rv$q_index <- rv$q_index + 1
        rv$s_index <- 1

      # Otherwise, go back to first question and student
      } else {
        rv$s_index <- 1
        rv$q_index <- 1
      }

    }
  })

  # Change student (backwards)
  observeEvent(input$key_prev, {

    # If not at at start of students, go to prev student
    if(rv$s_index != 1){
      rv$s_index <- rv$s_index - 1

    } else{

      # If not at start of questions, got to prev question
      if (rv$q_index != 1) {
        rv$q_index <- rv$q_index - 1
        rv$s_index <- tot_student

        # Otherwise, go to last question and student
      } else {
        rv$s_index <- tot_student
        rv$q_index <- tot_question
      }

    }
  })

  # Change question (forward)
  observeEvent(input$key_nexts, {

    # If not at end of questions
    if(rv$q_index != tot_question){
      rv$q_index <- rv$q_index + 1

    } else {

      # If not at end of students
      if (rv$s_index != tot_student) {
        rv$q_index <- 1
        rv$s_index <-  rv$s_index + 1

        # Go back to start
      } else {
        rv$q_index <- 1
        rv$s_index <- 1

      }
    }
  })

  # Change question (backwards)
  observeEvent(input$key_prevs, {

    # If not at end of questions
    if(rv$q_index != 1){
      rv$q_index <- rv$q_index - 1

    } else {

      # If not at end of students
      if (rv$s_index != 1) {
        rv$q_index <- tot_question
        rv$s_index <-  rv$s_index - 1

        # Go back to start
      } else {
        rv$q_index <- tot_question
        rv$s_index <- tot_student

      }
    }
  })

  ##### OTHER NAVIGATION #####

  # Flag question
  observeEvent(input$key_flag, {
    updateCheckboxInput(session, inputId = "flag", value = !input$flag)
    # TODO save flag status to log
  })

  # Reset focus
  observeEvent(input$key_reset, {
    shinyjs::runjs("document.activeElement.blur();")
  })

  # Go to comment
  observeEvent(input$key_comment, {
    js$refocus("ta_comment")
  })


}
