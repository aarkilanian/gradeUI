bslib::page_sidebar(

  ##### SHORTCUTS #####
  tags$script(HTML(
    "let aceFocused = false;

      document.addEventListener('focusin', function(e) {
      if (e.target.closest('.ace_editor')) {
        aceFocused = true;
      }
    });

    document.addEventListener('focusout', function(e) {
      if (e.target.closest('.ace_editor')) {
        aceFocused = false;
      }
    });

    document.addEventListener('keydown', function(e) {

      if (aceFocused) return;  // disable shortcuts while editing

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
        Shiny.setInputValue('key_rubric', '1', {priority: 'event'});
      }
      if (e.key === 'Digit1') {
        Shiny.setInputValue('key_rubric', '1', {priority: 'event'});
      }
      if (e.key === '2') {
        Shiny.setInputValue('key_rubric', '2', {priority: 'event'});
      }
      if (e.key === 'Digit2') {
        Shiny.setInputValue('key_rubric', '2', {priority: 'event'});
      }
      if (e.key === '3') {
        Shiny.setInputValue('key_rubric', '3', {priority: 'event'});
      }
      if (e.key === 'Digit3') {
        Shiny.setInputValue('key_rubric', '3', {priority: 'event'});
      }
      if (e.key === '4') {
        Shiny.setInputValue('key_rubric', '4', {priority: 'event'});
      }
      if (e.key === 'Digit4') {
        Shiny.setInputValue('key_rubric', '4', {priority: 'event'});
      }
      if (e.key === '5') {
        Shiny.setInputValue('key_rubric', '5', {priority: 'event'});
      }
      if (e.key === 'Digit5') {
        Shiny.setInputValue('key_rubric', '5', {priority: 'event'});
      }
      if (e.key === '6') {
        Shiny.setInputValue('key_rubric', '6', {priority: 'event'});
      }
      if (e.key === 'Digit6') {
        Shiny.setInputValue('key_rubric', '6', {priority: 'event'});
      }
      if (e.key === '7') {
        Shiny.setInputValue('key_rubric', '7', {priority: 'event'});
      }
      if (e.key === 'Digit7') {
        Shiny.setInputValue('key_rubric', '7', {priority: 'event'});
      }
      if (e.key === '8') {
        Shiny.setInputValue('key_rubric', '8', {priority: 'event'});
      }
      if (e.key === 'Digit8') {
        Shiny.setInputValue('key_rubric', '8', {priority: 'event'});
      }
      if (e.key === '9') {
        Shiny.setInputValue('key_rubric', '9', {priority: 'event'});
      }
      if (e.key === 'Digit9') {
        Shiny.setInputValue('key_rubric', '9', {priority: 'event'});
      }
      if (e.key === '0') {
        Shiny.setInputValue('key_rubric', '0', {priority: 'event'});
      }
      if (e.key === 'Digit0') {
        Shiny.setInputValue('key_rubric', '0', {priority: 'event'});
      }

      // Other navigation

      // Comment focus
      if (e.key === 'c') {
        Shiny.setInputValue('key_comment', Math.random());
      }
      // Reset focus
      if (e.key == 'Escape') {
        Shiny.setInputValue('key_reset', Math.random());
      }
      // Flag question
      if (e.key == 'f') {
        Shiny.setInputValue('key_flag', Math.random());
      }
    });"
  )),

  # activate shinyjs and extendShinyjs
  useShinyjs(),
  extendShinyjs(text = jscode, functions = "refocus"),

  # activate shiny feedback
  shinyFeedback::useShinyFeedback(),

  # Add theme
  theme = my_theme,

  # Title
  shiny::titlePanel(shiny::htmlOutput("student_question")),

  # Subtitle
  h5("This is the prompt."),

  # Student code
  sidebar = bslib::sidebar(card_code, width = "25%", open = FALSE),

  # Answers
  layout_column_wrap(
    accordion(id = "s_card", multiple = TRUE,
              accordion_panel("Student call", verbatimTextOutput("s_call")),
              accordion_panel("Student answer", verbatimTextOutput("s_answer")),
              accordion_panel("Student plots")),
    accordion(id = "a_card", multiple = TRUE,
              accordion_panel("Solution call", verbatimTextOutput("a_call")),
              accordion_panel("Solution answer", verbatimTextOutput("a_answer")),
              accordion_panel("Solution plots"))
  ),

  # Grading
  layout_column_wrap(
    card_rubric,
    layout_column_wrap(width = 1,
      card_feedback,
      card_progress
    )
  ),

  # Footer
  accordion(open = FALSE,
            accordion_panel(
              title = "Shortcuts (click to expand / shrink)",
              fillRow(
                flex = c(1,5,1,5,1,5),
                height = "100px",
                short_nav_sym,
                short_nav_desc,
                short_grade_sym,
                short_grade_desc,
                short_extra_sym,
                short_extra_desc
              )
            )
  )

 #  # Footer
 #  accordion(open = FALSE,
 #    accordion_panel(
 #      title = "Shortcuts (click to expand / shrink)",
 #      layout_columns(
 #        col_widths = c(2,4,2,4),
 #        card_short_nav_sym,
 #        card_short_nav_desc,
 #        card_short_grade_sym,
 #        card_short_grade_desc
 #      )
 #    )
 # )
)
