# Write example answer script
example_sub <- tempfile(fileext = ".R")
con_sub <- file(example_sub, "w")
cat("# Question 1\n", file = con_sub)
cat("a <- data.frame(a = c(1:80), b = rep(x = c('A', 'B', 'C', 'D'), times = 20))\n", file = con_sub)
cat("\n", file = con_sub)
cat("# Question 5\n", file = con_sub)
cat("b <- data.frame(a = c(1:100), b = rep(x = c('A', 'Z'), times = 50))\n", file = con_sub)
cat("\n", file = con_sub)
cat("#some comment about something which is the answer to Question 3.\n", file = con_sub)
cat("\n", file = con_sub)
cat("# Now here's a plot for Question 2\n", file = con_sub)
cat("plot(x = 1:100, y = (1:100)^2)", file = con_sub)
close(con_sub)

# Get calls
example_calls <- parse(example_sub)

# Write example final submissions list
example_student1 <- list(
  studentID = c("aark"),
  auto_comments = c("", "", "", "Could not parse", "Answer equivalent"), # Check, if not NA (wrong) display comment and check solutions and grade
  grades = c(NA, NA, NA, NA, 1), # Pre-populated by autograder for certain right answers
  answers = list( # Contains R objects associated with results, or index to plot/comment object
    data.frame(a = c(1:10), b = rep(x = c("A", "Z"), times = 5)),
    "plot_1", # Could also just be index if type included in bigger list
    "comment_1",
    NA,
    data.frame(a = c(1:100), b = rep(x = c("A", "Z"), times = 50))
  ),
  answer_calls = c(1, 3, NA, NA, 2) # Contains index of calls associated when possible
)

example_student2 <- list(
  studentID = c("bbark"),
  auto_comments = c("Answer equivalent", "", "", "Could not parse", "Answer equivalent"), # Check, if not NA (wrong) display comment and check solutions and grade
  grades = c(NA, NA, NA, NA, 1), # Pre-populated by autograder for certain right answers
  answers = list( # Contains R objects associated with results, or index to plot/comment object
    data.frame(a = c(1:80), b = rep(x = c("A", "B", "C", "D"), times = 20)),
    NA, # Should either be named list where we don't care about the order or NA should be inserted to preserve length. If named we can match question naming
    data.frame(a = c(1:100), b = rep(x = c("A", "Z"), times = 50))
  ),
  answer_calls = c(1, 3, NA, NA, 2) # Contains index of calls associated when possible
)

example_solutions <- list(
  data.frame(a = c(1:80), b = rep(x = c("A", "B", "C", "D"), times = 20)),
  NA, # Or an index etc
  NA, # Or an index etc
  "Balsam_Fir",
  data.frame(a = c(1:100), b = rep(x = c("A", "Z"), times = 50))
)

example_rubric <- data.frame(
  questionID = c("Question 1", # Get unique from this to get names of questions
                 "Question 1",
                 "Question 2 (plot)",
                 "Question 2 (plot)",
                 "Question 3 (comment)",
                 "Question 4 (auto-parse)",
                 "Question 5 (auto-wrong)"),
  comment = c("missing 1",
              "missing 2",
              "missing 1",
              "missing 2",
              "missing 1",
              "missing 1",
              "missing 1",),
  deduction = c(0.5, 0.25, 0.5, 0.25, 0.5, 0.25, 0.5),
  total = c(1, 1, 1, 1, 1, 1, 1)
)
