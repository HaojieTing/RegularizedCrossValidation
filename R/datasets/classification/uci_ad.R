readAdData <- function(filename) {
  data <- read.table(filename, na.strings = "?", sep=",", strip.white = TRUE)
  data1[is.na(data)] <- 0
}