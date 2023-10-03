# Load the required library
library(XML)
library(tidyverse)
library(dplyr)

# Define the URL for the GET request
url <- 'https://api.data.gov.in/resource/ab5ed247-4652-4833-8b1c-c7a4f5284162?api-key=579b464db66ec23bdd0000015115b9e83ec44fb969475c8cdea3f9be&format=xml&limit=1000'
# Send a GET request and extract XML content as a character string
response <- httr::GET(url)
xml_content <- httr::content(response, "text", encoding = "UTF-8")

# Parse the XML content
xml_content <- xmlParse(xml_content, useInternalNodes = TRUE)

# Extract specific nodes from the XML
items <- getNodeSet(xml_content, "//item")

sl_no <- character()
state <- character()
total_children_acute_respiratory_infection <- numeric()
rural_children_acute_respiratory_infection <- numeric()
urban_children_acute_respiratory_infection <- numeric()
total_women_aware_danger_signs_ari <- numeric()
rural_women_aware_danger_signs_ari <- numeric()
urban_women_aware_danger_signs_ari <- numeric()

# Loop through the extracted nodes and populate the vectors
for (item in items) {
  sl_no <- c(sl_no, xmlValue(xmlChildren(item)$sl_no_))
  state <- c(state, xmlValue(xmlChildren(item)$india_states_union_territories))
  total_children_acute_respiratory_infection <- c(total_children_acute_respiratory_infection, as.numeric(xmlValue(xmlChildren(item)$children_with_acute_respiratory_infection_or_fever_in_last_2_weeks_and_saught_advice_treatment___total)))
  rural_children_acute_respiratory_infection <- c(rural_children_acute_respiratory_infection, as.numeric(xmlValue(xmlChildren(item)$children_with_acute_respiratory_infection_or_fever_in_last_2_weeks_and_saught_advice_treatment___rural)))
  urban_children_acute_respiratory_infection <- c(urban_children_acute_respiratory_infection, as.numeric(xmlValue(xmlChildren(item)$children_with_acute_respiratory_infection_or_fever_in_last_2_weeks_and_saught_advice_treatment___urban)))
  total_women_aware_danger_signs_ari <- c(total_women_aware_danger_signs_ari, as.numeric(xmlValue(xmlChildren(item)$women_aware_about_danger_signs_of_acute_respiratory_infection___total)))
  rural_women_aware_danger_signs_ari <- c(rural_women_aware_danger_signs_ari, as.numeric(xmlValue(xmlChildren(item)$women_aware_about_danger_signs_of_acute_respiratory_infection___rural)))
  urban_women_aware_danger_signs_ari <- c(urban_women_aware_danger_signs_ari, as.numeric(xmlValue(xmlChildren(item)$women_aware_about_danger_signs_of_acute_respiratory_infection___urban)))
}

# Create a data frame from the vectors
child_2012 <- data.frame(
  Sl_No = sl_no,
  State = state,
  Total_Children_Acute_Respiratory_Infection = total_children_acute_respiratory_infection,
  Rural_Children_Acute_Respiratory_Infection = rural_children_acute_respiratory_infection,
  Urban_Children_Acute_Respiratory_Infection = urban_children_acute_respiratory_infection,
  Total_Women_Aware_Danger_Signs_ARI = total_women_aware_danger_signs_ari,
  Rural_Women_Aware_Danger_Signs_ARI = rural_women_aware_danger_signs_ari,
  Urban_Women_Aware_Danger_Signs_ARI = urban_women_aware_danger_signs_ari
)


child_2012 <- child_2012[-c(1:14,19,34,37,47:49),]
child_2012 <- child_2012[,-1]








