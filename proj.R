library(dplyr)
library(rvest)

html <- read_html("https://www.worldwildlife.org/species/directory?direction=desc&sort=extinction_status")
html2 <- read_html("https://www.worldwildlife.org/species/directory?direction=desc&page=2&sort=extinction_status")

animals <- html_table(html)
animals <- animals[[1]]
animals2 <- html_table(html2)
animals2 <- animals2[[1]]


species_tags <- gsub(" ", "-" , tolower(animals$`Common name`))
links <- paste("https://www.worldwildlife.org/species/" ,species_tags , sep = "")
#find a way to scrape all the attributes of each species

new <- bind_rows(animals,animals2)

html3 <- read_html("https://awionline.org/content/list-endangered-species")

animals3 <- html_table(html3)

animals3
