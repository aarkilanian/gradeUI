##### Setup #####

# Load packages
require(dplyr)
require(lubridate)
require(bslib)
require(shiny)
require(shinyAce)
require(shinyjs)
require(shinyFeedback)

# Source functions
source("R/cards.R")

# Read grading rubric
rubric <- read.csv("../data/1002_rubric.csv") %>%
  filter(question != "1") %>%
  mutate(question = question - 1)

# Read autograding output
students <- readRDS("../data/autograded.rda")

# Read solutions
answers <- readRDS("../data/1002_answers.rda")

# Set path to submission directory
ass_path <- "www/"

# Read descriptive data
tot_student <- length(students)
tot_question <- length(answers)

# JavaScript refocus function
jscode <- "
shinyjs.refocus = function(e_id) {
  document.getElementById(e_id).focus();
}"

##### Log #####

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

##### Retrieve progress #####

# Get last graded student and question
if(any(gradelog$last != "")){
  recent <- which(gradelog$last == max(gradelog$last))
  last_q <- as.numeric(gradelog[recent,]$question)
  last_s <- as.numeric(gradelog[recent,]$student)

} else {
  last_q <- 1
  last_s <- 1
}
