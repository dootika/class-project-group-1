
library(readxl)
library(openxlsx)
# Problem 1
ARI_death=read_excel("no_deaths_2011.xlsx")
World_AQI = read_excel("aap_pm_database_may2014.xls")

ARI_death <- as.data.frame(ARI_death)
ARI_death <- ARI_death[-1,]
ARI_death <- ARI_death[,-8]

names(ARI_death)[2] <- "Male Cases"
names(ARI_death)[3] <- "Male Deaths"
names(ARI_death)[4] <- "Female Cases"
names(ARI_death)[5] <- "Female Deaths"
names(ARI_death)[6] <- "Total Cases"
names(ARI_death)[7] <- "Total Deaths"
names(ARI_death)[1] <- "State"

#combining Jammu Kashmir
x <- as.numeric(ARI_death[16,2:7])
y <- as.numeric(ARI_death[15,2:7])
ARI_death[15,] = c("Jammu and Kashmir" , x+y)
ARI_death <- ARI_death[-16,]
ARI_death[5,"State"] = "Bihar"


#adding populations

library(dplyr)
library(rvest)

html <- read_html("https://en.wikipedia.org/wiki/List_of_states_and_union_territories_of_India_by_population")
pop <- html_table(html)

pop <- pop[[2]]
pop <- as.data.frame(pop)
head(pop)
State <- pop[-1,2]
populations <- pop[-1,3]
State[33] = "Dadra and Nagar Haveli"
populations[33] = 343709
State[38] = "Daman and Diu"
populations[38] = 243247
populations <- as.numeric(gsub(",","",populations))
populations[10] <-49577103

state_populations <- data.frame(State, populations)
unique(temp$State)
states[19] = "Delhi"
states[25] = "Manipur"

Deaths_2011 <- merge(ARI_death , state_populations)

write.csv(Deaths_2011 , "Deaths_2011.csv")
