library(dplyr)
library(tidyverse)
library(RColorBrewer)
library(ggplot2)


air_quality <- read.csv("./Data_Air_Quality/air_quality.csv")

air_qual <- as_tibble(air_quality)

air_qual$PM10_Annual_Average_g_m3 = as.numeric(air_qual$PM10_Annual_Average_g_m3)


#code for state_wise_pollution
air_qualf <- air_qual %>% group_by(State) %>% summarise(SO2_Annual_Average_g_m3 = mean(SO2_Annual_Average_g_m3) ,
                                           NO2_Annual_Average_g_m3 = mean(NO2_Annual_Average_g_m3), 
                                           PM10_Annual_Average_g_m3 = mean(PM10_Annual_Average_g_m3 , na.rm = TRUE))


child_2012 <- read.csv("./Data_Respiratory_illnesses/child_2012.csv")

child_78 <- read.csv("./Data_Respiratory_illnesses/child_78.csv")

#code for quality_vs_ARI
Deaths_2011 <- read.csv("./Data_Respiratory_illnesses/Deaths_2011.csv")
air
child_2012 <- child_2012[,-1]
Deaths_2011 <- Deaths_2011[,-1]
temp <- as.data.frame(air_qualf)

temp$State[6] = "Dadra and Nagar Haveli"
temp$State[7] = "Daman and Diu"
temp$State[13] = "Jammu and Kashmir"
temp$State[22] = "Odisha"
temp$State[26] = "Tamil Nadu"

comp <- merge (temp , Deaths_2011)
# comp2 <- merge(comp , Deaths_2011)
write.csv(comp , file = "quality_vs_ARI.csv")

