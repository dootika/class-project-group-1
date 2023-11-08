# Load the required library
library(tidyverse)
library(dplyr)

# Define the URL for the getting dataset
url <- 'https://api.data.gov.in/resource/c66bdb52-ef4e-4f5a-aa06-b2b5a6e09d3a?api-key=579b464db66ec23bdd0000015115b9e83ec44fb969475c8cdea3f9be&format=csv&limit=2000'
NHFS_data=read.csv(url)
NHFS_data=as_tibble(NHFS_data)
NHFS_data=NHFS_data[c(1,2,3,12,15,16,66,112,113)]
names(NHFS_data)=c("State",'Survey','Area','Households_using_clean_fuel_for_cooking','Women_who_are_literate','Men_who_are_literate','Prevalence_of_ARI_under_5_years','Tobacco_use_women','Tobacco_use_men')
NHFS_data=NHFS_data[NHFS_data[,3]=='Total',]

NHFS_data1 <- NHFS_data %>% group_by(State) %>% summarise( Households_using_clean_fuel_for_cooking = mean(Households_using_clean_fuel_for_cooking,na.rm = TRUE) ,
                                                        Women_who_are_literate = mean(Women_who_are_literate,na.rm = TRUE), 
                                                                      Men_who_are_literate = mean(Men_who_are_literate,na.rm = TRUE) ,Prevalence_of_ARI_under_5_years=mean(Prevalence_of_ARI_under_5_years,na.rm = TRUE),Tobacco_use_women=mean(Tobacco_use_women,na.rm = TRUE),Tobacco_use_men=mean(Tobacco_use_men,na.rm = TRUE))
NHFS_data1[c(7,16),1]=c('Chattisgarh','Jammu and Kashmir')
write.csv(NHFS_data1,file="State_wise_ARI_and_factors.csv",append=FALSE)
