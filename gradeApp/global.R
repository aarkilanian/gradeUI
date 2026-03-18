##### Resources #####
require(bslib)
require(shiny)
require(shinyAce)
source("R/cards.R")

# Read files
rubric <- read.csv("../data/ex_rubric.csv")
students <- readRDS("../data/students.rda")
answers <- readRDS("../data/answers.rda")
ass_path <- "../data/ex_assignments/"
