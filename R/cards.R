card_progress <- bslib::card(
  shiny::htmlOutput("progress")
)

card_shortcuts <- bslib::card(
  shiny::uiOutput("shortcuts")
)

card_code <- bslib::card(
  bslib::card_header("Student code"),
  shiny::htmlOutput("student_script"),
  shiny::actionButton("run", "Run selection"),
  shiny::actionButton("reset", "Reset code")
)

card_student <- bslib::card(
  bslib::card_header("Student output"), "test",
  bslib::card_header("Solution output"), "test")

card_rubric <- bslib::card(
  bslib::card_header("Rubric"), "test")
