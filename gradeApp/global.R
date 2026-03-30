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
log_path <- "../data/gradelog.rda"
# Build log if not present
if (!file.exists(log_path)) {

  gradelog <- vector("list", tot_student)

  for (i in 1:tot_student){
    gradelog[[i]] <- vector("list", tot_question)

    for(j in 1:tot_question){
      gradelog[[i]][[j]] <- list(grade = NA,
                                  choices = character(),
                                  date = NA)
    }
  }

  saveRDS(gradelog, log_path)

} else {

  gradelog <- readRDS(log_path)

}
