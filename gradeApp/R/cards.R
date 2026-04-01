# Add auto-card theme
my_theme <- bs_theme() |>
  bs_add_rules(".auto-coms { color: #FF7878; }")

##### Submission display cards #####
card_code <- bslib::card(
  bslib::card_header("Student code"),
  aceEditor("code", mode = "r", selectionId = "selection", fontSize = 16),
  shiny::actionButton("run", "Run selection"),
  shiny::actionButton("reset", "Reset code"),
  verbatimTextOutput("code_output")
)

card_student_call <- bslib::card(
  bslib::card_header("Student call & output"),
  verbatimTextOutput("s_call"),
  verbatimTextOutput("s_answer")
)

##### Grading cards #####
card_solution <- bslib::card(
  bslib::card_header("Expected call & output"),
  verbatimTextOutput("a_call"),
  verbatimTextOutput("a_answer")
)

card_rubric <- bslib::card(
  bslib::card_header("Rubric"),
  uiOutput("auto_comments", class = "auto-coms"),
  uiOutput("rubric_checkbox"),
  textOutput("score_display")
  )

card_feedback<- bslib::card(
  bslib::card_header("Feedback"),
  uiOutput("ta_comment"),
  checkboxInput("flag", "Flag question", FALSE)
)

card_progress <- bslib::card(
  bslib::card_header("Progress"),
  shiny::htmlOutput("progress"),
  uiOutput("grading_status")
)

##### Shortcuts #####
short_nav_sym <- div(
    strong("\u2190"),
    HTML("<br>"),
    strong("\u2192"),
    HTML("<br>"),
    strong("\u2191"),
    HTML("<br>"),
    strong("\u2193")
  )

short_nav_desc <- div(
  HTML("next question<br>"),
  HTML("previous question<br>"),
  HTML("next student<br>"),
  HTML("previous student<br>")
)

short_grade_sym <- div(
  strong("1-9"),
  HTML("<br>"),
  strong("f"),
  HTML("<br>"),
  strong("c"),
  HTML("<br>"),
  strong("Esc")
)

short_grade_desc <- div(
  HTML("select rubric items<br>"),
  HTML("flag question<br>"),
  HTML("go to feedback<br>"),
  HTML("leave feedback<br>")
)

short_extra_sym <- div(
  strong("e"),
  HTML("<br>"),
  strong("alt+e"),
  HTML("<br>"),
  strong("\u21b5")
)

short_extra_desc <- div(
  HTML("enter edit mode<br>"),
  HTML("exit edit mode<br>"),
  HTML("force save<br>")
)
