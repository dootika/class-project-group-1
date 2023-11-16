library(rvest)
library(dplyr)
library(tidyverse)

html <- read_html("https://pib.gov.in/PressReleasePage.aspx?PRID=1888090")

tables <- html_table(html)

poll <- tables[[1]]

poll <- as.data.frame(poll)

colnames(poll) <- poll[1,]
poll <- poll[-1,]

write.csv(poll , "Pandemic_pollution.csv")
