students <- list(
  student_1 = list(
    q1 = list(
      grade = TRUE,
      TA_grade = NA,
      deductions = c(),
      auto_message = NA,
      answer = data.frame(a = 1:3, b = 4:6),
      answer_call = parse(text = "a <- data.frame(a = 1:3, b = 4:6)")
    ),
    q2 = list(
      grade = FALSE,
      TA_grade = NA,
      deductions = c(),
      auto_message = "Wrong.",
      answer = 5,
      answer_call = parse(text = "b <- 6 - 1")
    ),
    q3 = list(
      grade = TRUE,
      TA_grade = NA,
      deductions = c(),
      auto_message = "Correct.",
      answer = c("a", "b", "c"),
      answer_call = parse(text = "aa <- c('a', 'b', 'c')")
    ),
    path = "../data/ex_assignments/student_1.R"
  ),
  student_2 = list(
    q1 = list(
      grade = TRUE,
      TA_grade = NA,
      deductions = c(),
      auto_message = "Correct.",
      answer = data.frame(a = 4:6, b = 1:3),
      answer_call = parse(text = "a <- data.frame(a = 1:3, b = 4:6)")
    ),
    q2 = list(
      grade = TRUE,
      TA_grade = NA,
      deductions = c(),
      auto_message = "Correct.",
      answer = 10,
      answer_call = parse(text = "b <- 11 - 1")
    ),
    q3 = list(
      grade = FALSE,
      TA_grade = NA,
      deductions = c(),
      auto_message = "Wrong.",
      answer = c("a", "d", "f"),
      answer_call = parse(text = "aa <- c('a', 'b', 'c')")
    ),
    path = "../data/ex_assignments/student_2.R"
  ),
  student_3 = list(
    q1 = list(
      grade = FALSE,
      TA_grade = NA,
      deductions = c(),
      auto_message = "Correct.",
      answer = data.frame(a = 10:15, b = 20:25),
      answer_call = parse(text = "a <- data.frame(a = 1:3, b = 4:6)")
    ),
    q2 = list(
      grade = FALSE,
      TA_grade = NA,
      deductions = c(),
      auto_message = "Wrong.",
      answer = 7,
      answer_call = parse(text = "b <- 6 + 1")
    ),
    q3 = list(
      grade = TRUE,
      TA_grade = NA,
      deductions = c(),
      auto_message = "Correct.",
      answer = c("a", "b", "c"),
      answer_call = parse(text = "aa <- c('a', 'b', 'c')")
    ),
    path = "../data/ex_assignments/student_3.R"
  )
)
