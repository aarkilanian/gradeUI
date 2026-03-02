# Add auto-card theme
my_theme <- bs_theme() |>
  bs_add_rules(".auto-card { border-color: #FF7878 ; }")

##### Cards #####
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
  bslib::card_header("Student output"), "test")

card_solution <- bslib::card(
  bslib::card_header("Solution output"), "test")

card_rubric <- bslib::card(
  bslib::card_header("Rubric"), "test")

card_comment <- bslib::card(
  bslib::card_header("Autograder comments"), class = "auto-card", "test")
