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
air_qualf <- as.data.frame(air_qualf)
air_qualf$State[6] = "Dadra and Nagar Haveli"
air_qualf$State[7] = "Daman and Diu"
air_qualf$State[13] = "Jammu and Kashmir"
air_qualf$State[22] = "Odisha"
air_qualf$State[26] = "Tamil Nadu"
air_qualf$State[5] = "Chhattisgarh"

PM25 <- read.csv("./Data_Respiratory_illnesses/PM25.csv")

air_qualf <- merge(air_qualf , PM25)

write.csv(statewise_pollution , "statewise_pollution.csv")
#code for quality_vs_ARI
Deaths_2011 <- read.csv("./Data_Respiratory_illnesses/Deaths_2011.csv")

ARI_and_factors <- read.csv("./Data_Respiratory_illnesses/State_wise_ARI_and_factors.csv")
ARI_and_factors <- ARI_and_factors[,-1]

Deaths_2011 <- Deaths_2011[,-1]

comp <- merge (air_qualf , Deaths_2011)
comp2 <- merge(air_qualf , ARI_and_factors)

# comp2 <- merge(comp , Deaths_2011)
write.csv(comp , file = "quality_vs_deaths.csv")
write.csv(comp2 , file = "quality_vs_ARI.csv")

#Relevant plots
correlation_coefficient <- cor(comp2$NO2_Annual_Average_g_m3, comp2$Prevalence_of_ARI_under_5_years)
ggplot(comp2, aes(x = NO2_Annual_Average_g_m3, y = Prevalence_of_ARI_under_5_years )) +
  geom_point() +
  labs(title = paste("Scatterplot for ARI vs NO2")) + 
  xlab("NO2 annual average in g/m3") + 
  ylab("Prevelance of ARI under 5 years") + geom_smooth(method = "lm", se = FALSE, color = "blue") +
  annotate("text", x = min(comp2$NO2_Annual_Average_g_m3), y = max(comp2$Prevalence_of_ARI_under_5_years), label = paste("Corr:", round(correlation_coefficient, 2)) ,size = 4)+
  theme(
    plot.title = element_text(size = 16),  # Title font size
    axis.title.x = element_text(size = 14),  # X-axis label font size
    axis.title.y = element_text(size = 14),  # Y-axis label font size
    axis.text.x = element_text(size = 12),  # X-axis text (tick labels) font size
    axis.text.y = element_text(size = 12),  # Y-axis text (tick labels) font size
    text = element_text(size = 12)  # General text font size
  )

ggsave("ARI_vs_NO2.jpeg")


correlation_coefficient <- cor(comp2$SO2_Annual_Average_g_m3, comp2$Prevalence_of_ARI_under_5_years)
ggplot(comp2, aes(x = SO2_Annual_Average_g_m3, y = Prevalence_of_ARI_under_5_years )) +
  geom_point() +
  labs(title = paste("Scatterplot for ARI vs SO2")) + 
  xlab("SO2 annual average in g/m3") + 
  ylab("Prevelance of ARI under 5 years") + geom_smooth(method = "lm", se = FALSE, color = "blue") +
  annotate("text", x = min(comp2$SO2_Annual_Average_g_m3), y = max(comp2$Prevalence_of_ARI_under_5_years), label = paste("Corr:", round(correlation_coefficient, 2)) ,size = 4)+
  theme(
    plot.title = element_text(size = 16),  # Title font size
    axis.title.x = element_text(size = 14),  # X-axis label font size
    axis.title.y = element_text(size = 14),  # Y-axis label font size
    axis.text.x = element_text(size = 12),  # X-axis text (tick labels) font size
    axis.text.y = element_text(size = 12),  # Y-axis text (tick labels) font size
    text = element_text(size = 12)  # General text font size
  )
ggsave("ARI_vs_SO2.jpeg")




correlation_coefficient <- cor(comp2$PM10_Annual_Average_g_m3, comp2$Prevalence_of_ARI_under_5_years)
ggplot(comp2, aes(x = PM10_Annual_Average_g_m3, y = Prevalence_of_ARI_under_5_years )) +
  geom_point() +
  labs(title = paste("Scatterplot for ARI vs PM10")) + 
  xlab("PM10 annual average in g/m3") + 
  ylab("Prevelance of ARI under 5 years") + geom_smooth(method = "lm", se = FALSE, color = "blue") +
  annotate("text", x = min(comp2$PM10_Annual_Average_g_m3), y = max(comp2$Prevalence_of_ARI_under_5_years), label = paste("Corr:", round(correlation_coefficient, 2)) , size = 4) +
  theme(
    plot.title = element_text(size = 16),  # Title font size
    axis.title.x = element_text(size = 14),  # X-axis label font size
    axis.title.y = element_text(size = 14),  # Y-axis label font size
    axis.text.x = element_text(size = 12),  # X-axis text (tick labels) font size
    axis.text.y = element_text(size = 12),  # Y-axis text (tick labels) font size
    text = element_text(size = 12)  # General text font size
  )
ggsave("ARI_vs_PM10.jpeg")


correlation_coefficient <- cor(comp2$PM2.5_ug_m3, comp2$Prevalence_of_ARI_under_5_years)
ggplot(comp2, aes(x = PM2.5_ug_m3, y = Prevalence_of_ARI_under_5_years )) +
  geom_point() +
  labs(title = paste("Scatterplot for ARI vs PM2.5")) + 
  xlab("PM2.5 annual average in ug/m3") + 
  ylab("Prevelance of ARI under 5 years") + geom_smooth(method = "lm", se = FALSE, color = "blue") +
  annotate("text", x = min(comp2$PM2.5_ug_m3), y = max(comp2$Prevalence_of_ARI_under_5_years), label = paste("Corr:", round(correlation_coefficient, 2)), size = 4) +
  theme(
    plot.title = element_text(size = 16),  # Title font size
    axis.title.x = element_text(size = 14),  # X-axis label font size
    axis.title.y = element_text(size = 14),  # Y-axis label font size
    axis.text.x = element_text(size = 12),  # X-axis text (tick labels) font size
    axis.text.y = element_text(size = 12),  # Y-axis text (tick labels) font size
    text = element_text(size = 12)  # General text font size
  )

ggsave("ARI_vs_PM2.5.jpeg")

#tobacco

correlation_coefficient <- cor(comp2$Tobacco_use_men, comp2$Prevalence_of_ARI_under_5_years)

ggplot(comp2, aes(x = Tobacco_use_men, y = Prevalence_of_ARI_under_5_years )) +
  geom_point() +
  labs(title = paste("Scatterplot for ARI vs Tobacco use in men")) + 
  xlab("Percent tobacco use in men") + 
  ylab("Prevelance of ARI under 5 years") + geom_smooth(method = "lm", se = FALSE, color = "green") +
  annotate("text", x = min(comp2$Tobacco_use_men), y = max(comp2$Prevalence_of_ARI_under_5_years), label = paste("Corr:", round(correlation_coefficient, 2)) , size = 4) +
  theme(
    plot.title = element_text(size = 16),  # Title font size
    axis.title.x = element_text(size = 14),  # X-axis label font size
    axis.title.y = element_text(size = 14),  # Y-axis label font size
    axis.text.x = element_text(size = 12),  # X-axis text (tick labels) font size
    axis.text.y = element_text(size = 12),  # Y-axis text (tick labels) font size
    text = element_text(size = 12)  # General text font size
  )

ggsave("ARI_vs_Tobacco_men.jpeg")


correlation_coefficient <- cor(comp2$Tobacco_use_women, comp2$Prevalence_of_ARI_under_5_years)

ggplot(comp2, aes(x = Tobacco_use_women, y = Prevalence_of_ARI_under_5_years )) +
  geom_point() +
  labs(title = paste("Scatterplot for ARI vs Tobacco use in women")) + 
  xlab("Percent tobacco use in women") + 
  ylab("Prevelance of ARI under 5 years") + geom_smooth(method = "lm", se = FALSE, color = "green") +
  annotate("text", x = min(comp2$Tobacco_use_women), y = max(comp2$Prevalence_of_ARI_under_5_years), label = paste("Corr:", round(correlation_coefficient, 2)) , 4) +
  theme(
    plot.title = element_text(size = 16),  # Title font size
    axis.title.x = element_text(size = 14),  # X-axis label font size
    axis.title.y = element_text(size = 14),  # Y-axis label font size
    axis.text.x = element_text(size = 12),  # X-axis text (tick labels) font size
    axis.text.y = element_text(size = 12),  # Y-axis text (tick labels) font size
    text = element_text(size = 12)  # General text font size
  )

ggsave("ARI_vs_Tobacco_women.jpeg")

#state max death/case
Deaths_2011[order(Deaths_2011$Total.Deaths.Cases , decreasing = TRUE),]

ordered_deaths <- Deaths_2011[order(Deaths_2011$Total.Deaths.Cases , decreasing = FALSE),]
ordered_deaths <- ordered_deaths[1:7,]

# Create the barplot
ggplot(ordered_deaths, aes(x = State , y = Total.Deaths.Cases)) +
  geom_bar(stat = "identity", fill = "purple") +
  labs(title = paste("Barplot for states with highest deaths/cases")) + 
  xlab("States") +
  ylab("Deaths/Cases")
ggsave("statewise_deaths_cases.jpeg")


