bslib::page_sidebar(

  # Add keyboard shortcuts
  tags$script(HTML(
    "document.addEventListener('keydown', function(e) {

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
    });"
  )),
  # Add theme
  theme = my_theme,

  # Title
  shiny::titlePanel(shiny::htmlOutput("student_question")),

  # Sidebar content
  sidebar = bslib::sidebar(card_code, width = "25%"),

  # Main content
  layout_column_wrap(
    width = "350px",
    height = "300px",
    card_student,
    card_solution
  ),
  uiOutput("auto_comments", class = "auto-coms"),
  card_rubric,

  # Footer
  accordion(
    accordion_panel(
      title = "Progress and shortcuts (click to expand / shrink)",
      fluidRow(
        column(width = 6, card_progress
        ), column(width = 6, card_shortcuts
        )
      )
    )
  )
)
