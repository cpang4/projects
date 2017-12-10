library(tidyverse)
library(stringr)

pres.data <- read_lines("https://raw.githubusercontent.com/cpang4/projects/master/Text%20Mining/stateoftheunion1790-2012.txt")

asterisk.index <- which(str_detect(pres.data, "\\*\\*\\*") == TRUE)
speech.summaries <- data.frame(presName = character(), year=character(), month=character(), day=character(), numSentences=character(), numWords=character(), stringsAsFactors = FALSE)

for (i in 2:length(asterisk.index)-1){
  curr.speech <- pres.data[(asterisk.index[i] + 1):(asterisk.index[(i+1)] - 1)]
  pres.name_curr <- curr.speech[3]
  date_curr <- str_split(curr.speech[4], boundary("word"))
  year_curr <- date_curr[[1]][3]
  month_curr <- date_curr[[1]][1]
  numericMonth <- match(tolower(substr(month_curr, 1, 3)), tolower(month.abb))
  day_curr <- date_curr[[1]][2]
  dateWithDashes <- paste(year_curr, numericMonth, day_curr, sep = "-")
  dayOfTheWeek_curr <- weekdays(as.Date(dateWithDashes))
  speechString <- paste(curr.speech[6:length(curr.speech)], sep=" ",collapse=" ")
  numSentences_curr <- length(str_split(speechString, boundary("sentence"))[[1]])
  numWords_curr <- length(str_split(speechString, boundary("word"))[[1]])
  speech.summaries <- rbind(speech.summaries, data.frame(presName = pres.name_curr, year = year_curr, month = month_curr, day = dayOfTheWeek_curr, numSentences = numSentences_curr, numWords = numWords_curr, stringsAsFactors = FALSE))
}

speech.summaries