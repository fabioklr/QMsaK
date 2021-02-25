esg_score_getter <- function(string) {
  result <- vector()
  for (i in 1:nchar(string)) {
    if (str_sub(string, -i, -i) %in% letters) {
      result <- c(result, str_sub(string, -i, -i))
    }
    else {
      break
    }
  }
  return(paste(capitalize(rev(result)), collapse = ""))
}