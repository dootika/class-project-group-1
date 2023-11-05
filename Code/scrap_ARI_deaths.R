
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

write.csv(ARI_death , file = "Deaths_2011.csv")
