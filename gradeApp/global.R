##### Resources #####
require(dplyr)
require(lubridate)
require(bslib)
require(shiny)
require(shinyAce)
require(shinyjs)
require(shinyFeedback)
source("R/cards.R")

# JS refocus function
jscode <- "
shinyjs.refocus = function(e_id) {
  document.getElementById(e_id).focus();
}"

# Read files
rubric <- read.csv("../data/ex_rubric.csv")
students <- readRDS("../data/students.rda")
answers <- readRDS("../data/answers.rda")
ass_path <- "../data/ex_assignments/"

# Read descriptive data
tot_student <- length(students)
tot_question <- length(answers)

# Set log path
log_path <- "../data/gradelog.csv"
# Build log if not present
if (!file.exists(log_path)) {

  # Define descriptive and dimensions
  student <- rep(1:tot_student, each = tot_question)
  question <- rep(1:tot_question, times = tot_student)

  # Logging columns
  mess <- character(length = length(student))
  rub <- character(length = length(student))
  last <- character(length = length(student))
  flagged <- logical(length = length(student))

  # Combine to dataframe
  gradelog <- data.frame(student, question, mess, rub, last, flagged)

  # Save as csv
  write.csv(gradelog, log_path, row.names = FALSE)

} else {

  gradelog <- read.csv(log_path, colClasses = c("character",
                                                "character",
                                                "character",
                                                "character",
                                                "character",
                                                "logical"))

}

# Get last graded student and question
if(any(gradelog$last != "")){
  recent <- which(gradelog$last == max(gradelog$last))
  last_q <- gradelog[recent,]$question
  last_s <- gradelog[recent,]$student

} else {
  last_q <- 1
  last_s <- 1
}
