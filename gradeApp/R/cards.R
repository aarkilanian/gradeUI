# Add auto-card theme
my_theme <- bs_theme() |>
  bs_add_rules(".auto-coms { color: #FF7878; }")

##### Cards #####
card_progress <- bslib::card(
  shiny::htmlOutput("progress")
)

card_shortcuts <- bslib::card(
  shiny::uiOutput("shortcuts")
)

card_code <- bslib::card(
  bslib::card_header("Student code"),
  aceEditor("code", mode = "r", selectionId = "selection"),
  shiny::actionButton("run", "Run selection"),
  shiny::actionButton("reset", "Reset code"),
  verbatimTextOutput("code_output")
)

card_student <- bslib::card(
  bslib::card_header("Student call & output"),
  verbatimTextOutput("s_call"),
  verbatimTextOutput("s_answer")
)

card_solution <- bslib::card(
  bslib::card_header("Expected call & output"),
  verbatimTextOutput("a_call"),
  verbatimTextOutput("a_answer")
)

card_rubric <- bslib::card(
  height = "400px",
  bslib::card_header("Rubric"),
  uiOutput("rubric_checkbox"),
  textOutput("score_display")
  )
