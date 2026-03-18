answers <- list(
  q1 = list(
    answer = data.frame(a = 1:3, b = 4:6),
    answer_call = parse(text = "a <- data.frame(a = 1:3, b = 4:6)")
  ),
  q2 = list(
    answer = 10,
    answer_call = parse(text = "b <- 11 - 1")
  ),
  q3 = list(
    answer = c("a", "b", "c"),
    answer_call = parse(text = "aa <- c('a', 'b', 'c')")
  )
)
