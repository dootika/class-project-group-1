library(dplyr)
library(tidyverse)
library(RColorBrewer)
library(ggplot2)


air_quality <- read.csv("./Data_Air_Quality/air_quality.csv")

table(air_quality$Type)

air_qual <- as_tibble(air_quality)

air_qual$PM10_Annual_Average_g_m3 = as.numeric(air_qual$PM10_Annual_Average_g_m3)

air_qualf <- air_qual %>% group_by(State) %>% summarise(SO2_Annual_Average_g_m3 = mean(SO2_Annual_Average_g_m3) ,
                                           NO2_Annual_Average_g_m3 = mean(NO2_Annual_Average_g_m3), 
                                           PM10_Annual_Average_g_m3 = mean(PM10_Annual_Average_g_m3 , na.rm = TRUE))



#visualizing pollution data
plot(air_qualf$SO2_Annual_Average_g_m3, air_qualf$NO2_Annual_Average_g_m3 , xlab = "SO2 annual average in g/m^3"
      ,ylab = "NO2 annual average in g/m^2" , main = "statewise NO2 vs SO2")
plot(air_qualf$SO2_Annual_Average_g_m3, air_qualf$PM10_Annual_Average_g_m3 , xlab = "SO2 annual average in g/m^3"
     ,ylab = "PM10 annual average in g/m^2" , main = "statewise PM10 vs SO2")
plot(air_qualf$NO2_Annual_Average_g_m3, air_qualf$PM10_Annual_Average_g_m3 , xlab = "NO2 annual average in g/m^3"
     ,ylab = "PM10 annual average in g/m^2" , main = "statewise PM10 vs NO2")




#correlation between indicators
plot(air_qual$SO2_Annual_Average_g_m3,air_qual$NO2_Annual_Average_g_m3)
plot(air_qual$SO2_Annual_Average_g_m3,air_qual$PM10_Annual_Average_g_m3)
plot(air_qual$NO2_Annual_Average_g_m3,air_qual$PM10_Annual_Average_g_m3)


child_2012 <- read.csv("./Data_Respiratory_illnesses/child_2012.csv")

child_2012[child_2012$State %in% air_qualf$State ,]


#visualizing RI data
plot(child_2012$Total_Children_Acute_Respiratory_Infection , child_2012$Total_Women_Aware_Danger_Signs_ARI)
plot(child_2012$Rural_Children_Acute_Respiratory_Infection , child_2012$Rural_Women_Aware_Danger_Signs_ARI)
plot(child_2012$Urban_Children_Acute_Respiratory_Infection , child_2012$Urban_Women_Aware_Danger_Signs_ARI)



child_78 <- read.csv("./Data_Respiratory_illnesses/child_78.csv")

child_78[child_78$State %in% child_78$State ,]


#visualizing RI data
plot(child_2012$Total_Children_Acute_Respiratory_Infection , child_2012$Total_Women_Aware_Danger_Signs_ARI)
plot(child_2012$Rural_Children_Acute_Respiratory_Infection , child_2012$Rural_Women_Aware_Danger_Signs_ARI)
plot(child_2012$Urban_Children_Acute_Respiratory_Infection , child_2012$Urban_Women_Aware_Danger_Signs_ARI)

child_2012$State
air_qualf$State

temp <- as.data.frame(air_qualf)
comp <- merge (temp , child_2012)

#correlation
plot(comp$SO2_Annual_Average_g_m3 , comp$Total_Children_Acute_Respiratory_Infection)
plot(comp$NO2_Annual_Average_g_m3 , comp$Total_Children_Acute_Respiratory_Infection)
plot(comp$PM10_Annual_Average_g_m3 , comp$Total_Children_Acute_Respiratory_Infection)

 