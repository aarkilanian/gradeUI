function(input, output, session){

  ##### VALUES #####

  # Create student/question tracking variables
  rv <- reactiveValues(
    q_index = last_q,
    s_index = last_s
  )

  # Create main log variable
  # fullLog <- reactiveVal(read.csv(log_path, colClasses = c("character",
  #                                                          "character",
  #                                                          "character",
  #                                                          "character",
  #                                                          "character",
  #                                                          "logical")))
  fullLog <- reactiveFileReader(1000, session, log_path, read.csv,
                                colClasses = c("character",
                                               "character",
                                               "character",
                                               "character",
                                               "character",
                                               "logical"))

  # Create logging variables
  curLog <- reactiveVal()

  # Parse current log
  observe({
    full_log <- fullLog()
    # Filter current question and student log
    cur_log <- full_log %>%
      filter(student == rv$s_index & question == rv$q_index)
    # Update current log
    curLog(cur_log)
  })

  ##### DISPLAYS #####

  # Current student and question
  output$student_question <- renderUI({
    HTML(paste0("Student ", rv$s_index, " | ", "Question ", rv$q_index))
  })

  # Autograding comments
  output$auto_comments <- renderUI({
    mess <- students[[rv$s_index]][[rv$q_index]]$auto_message
    if(is.na(mess)) "" else paste0("Autograder: ", mess)
  })

  # Display saved message
  output$ta_comment <- renderUI({
    textInput("ta_comment", "Feedack", value = curLog()$mess)
  })

  # Display flagging
  output$flag <- renderUI({
    checkboxInput("flag", "Flag question", value = curLog()$flagged)
  })

  # Display checkboxes
  output$rubric_checkbox <- renderUI({
    cur_rub <- curLog()$rub
    print(cur_rub)
    if(cur_rub == "") {
      sel <- character(0)
    } else {
      sel <- cur_rub
    }
    radioButtons("rubric_checkbox", "Rubric",
                 choices = get_choices(),
                 selected = sel)
  })

  # Display last saved date
  output$last_date <- renderText({
    paste("Last graded: ", curLog()$last_date)
  })

  # Display grade
  output$score_display <- renderText({
    paste("Question score: ", score())
  })

  ##### RUBRIC #####

  ### Reactives

  # Get choices for current question from rubric
  get_choices <- reactive({
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

  # Question score calculator
  score <- reactive({
    # Calculate total question marks
    total_marks <- rubric[rubric$question == rv$q_index,]
    total_marks <- total_marks[1,5]
    # Get selected rubric items
    selected <- input$rubric_checkbox
    if(is.null(selected)) return()
    sub_rubric <- rubric[rubric$question == rv$q_index,]
    # Get and sum deductions
    selected_ded <- sub_rubric[sub_rubric$key %in% selected,]
    selected_ded <- selected_ded$deduction
    return(total_marks - sum(selected_ded))
  })

  ### Inputs

  # Capture rubric with keyboard
  observeEvent(input$key_rubric, {

    # Get selection
    sel <- isolate(input$key_rubric)

    # Update rubric checkboxes
    updateRadioButtons(session,
                       inputId = "rubric_checkbox",
                       selected = sel)
  })

  # Capture rubric choice with mouse and save
  observeEvent(input$rubric_checkbox, {

    if(length(input$rubric_checkbox) == 0) return()
    if(input$rubric_checkbox == curLog()$rub) return()

    print("saving rubric")

    # Read current and full log
    full_log <- fullLog()
    cur_log <- curLog()

    # Make changes
    cur_log$rub <- input$rubric_checkbox
    cur_log$last <- as.character(Sys.time())

    # Save current log reactive
    curLog(cur_log)

    # Modify full log
    full_log <- full_log %>%
      rows_update(cur_log, by = c("student", "question"))

    # Save to file
    write.csv(full_log, log_path, row.names = FALSE)

    # Reread file
    fullLog()

  })

  # Flag question (keyboard)
  observeEvent(input$key_flag, {

    # Get checkbox value
    curFlag <- isolate(input$flag)
    # Update checkbox
    updateCheckboxInput(session, inputId = "flag", value = !curFlag)

  })

  # Flag question (mouse) & save
  observeEvent(input$flag, {

    #if(input$flag == curLog()$flagged) return()

    print("saving flag")

    # Read current and full log
    full_log <- fullLog()
    cur_log <- curLog()

    # Make changes
    cur_log$flagged <- input$flag

    # Save current log reactive
    curLog(cur_log)

    # Modify full log
    full_log <- full_log %>%
      rows_update(cur_log, by = c("student", "question"))

    # Save to file
    write.csv(full_log, log_path, row.names = FALSE)

    # Reread file
    fullLog()
  })

  # Save comment
  observeEvent(input$ta_comment, {

    #if(input$ta_comment == curLog()$mess) return()

    print("saving comment")

    # Read current and full log
    full_log <- fullLog()
    cur_log <- curLog()

    # Make changes
    cur_log$mess <- input$ta_comment

    # Save current log reactive
    curLog(cur_log)

    # Modify full log
    full_log <- full_log %>%
      rows_update(cur_log, by = c("student", "question"))

    # Save to file
    write.csv(full_log, log_path, row.names = FALSE)

    # Reread file
    fullLog()
  })

  # Grading progress
  # output$progress <- shiny::renderUI({
  #   div(
  #     HTML(paste0(rv$graded/tot_student*100), " % complete<br>"),
  #     HTML(paste0(tot_student - rv$graded), " students remaining<br>"),
  #     HTML(paste0(tot_student*tot_question - length(rv$grades), " questions remaining")),
  #     HTML("</p>")
  #   )
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
    print(assignment_path)
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

  ##### NAVIGATION #####

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

  # Reset focus
  observeEvent(input$key_reset, {
    shinyjs::runjs("document.activeElement.blur();")
  })

  # Go to comment
  observeEvent(input$key_comment, {
    js$refocus("ta_comment")
  })


}
