# Load the required library
library(XML)
library(tidyverse)
library(dplyr)

# Define the URL for the GET request
url <- 'https://api.data.gov.in/resource/1e23f069-a792-4f2f-8c86-d9885e84f2d0?api-key=579b464db66ec23bdd0000015115b9e83ec44fb969475c8cdea3f9be&format=xml&limit=10000'

# Send a GET request and extract XML content as a character string
response <- httr::GET(url)
xml_content <- httr::content(response, "text", encoding = "UTF-8")

# Parse the XML content
xml_content <- xmlParse(xml_content, useInternalNodes = TRUE)

# Extract specific nodes from the XML
items <- getNodeSet(xml_content, "//item")

sl_no <- character()
state <- character()
total_children_suffered_ari <- numeric()
rural_children_suffered_ari <- numeric()
urban_children_suffered_ari <- numeric()
total_children_treated_ari <- numeric()
rural_children_treated_ari <- numeric()
urban_children_treated_ari <- numeric()
total_children_treated_government_health_facility <- numeric()
rural_children_treated_government_health_facility <- numeric()
urban_children_treated_government_health_facility <- numeric()
total_children_treated_private_health_facility <- numeric()
rural_children_treated_private_health_facility <- numeric()
urban_children_treated_private_health_facility <- numeric()

# Loop through the extracted nodes and populate the vectors
for (item in items) {
  sl_no <- c(sl_no, xmlValue(xmlChildren(item)$sl_no_))
  state <- c(state, xmlValue(xmlChildren(item)$india_states_union_territories))
  total_children_suffered_ari <- c(total_children_suffered_ari, as.numeric(xmlValue(xmlChildren(item)$children_suffered_from_ari___total)))
  rural_children_suffered_ari <- c(rural_children_suffered_ari, as.numeric(xmlValue(xmlChildren(item)$children_suffered_from_ari___rural)))
  urban_children_suffered_ari <- c(urban_children_suffered_ari, as.numeric(xmlValue(xmlChildren(item)$children_suffered_from_ari___urban)))
  total_children_treated_ari <- c(total_children_treated_ari, as.numeric(xmlValue(xmlChildren(item)$children_under_age_five_for_whom_treatment_was_sought_from_health_facility_with_symptoms_of_acute_respiratory_infection__ari____total)))
  rural_children_treated_ari <- c(rural_children_treated_ari, as.numeric(xmlValue(xmlChildren(item)$children_under_age_five_for_whom_treatment_was_sought_from_health_facility_with_symptoms_of_acute_respiratory_infection__ari____rural)))
  urban_children_treated_ari <- c(urban_children_treated_ari, as.numeric(xmlValue(xmlChildren(item)$children_under_age_five_for_whom_treatment_was_sought_from_health_facility_with_symptoms_of_acute_respiratory_infection__ari____urban)))
  total_children_treated_government_health_facility <- c(total_children_treated_government_health_facility, as.numeric(xmlValue(xmlChildren(item)$children_under_age_five_for_whom_treatment_was_sought_from_health_facility_with_symptoms_of_acute_respiratory_infection__ari____government_health_facility___total)))
  rural_children_treated_government_health_facility <- c(rural_children_treated_government_health_facility, as.numeric(xmlValue(xmlChildren(item)$children_under_age_five_for_whom_treatment_was_sought_from_health_facility_with_symptoms_of_acute_respiratory_infection__ari____government_health_facility___rural)))
  urban_children_treated_government_health_facility <- c(urban_children_treated_government_health_facility, as.numeric(xmlValue(xmlChildren(item)$children_under_age_five_for_whom_treatment_was_sought_from_health_facility_with_symptoms_of_acute_respiratory_infection__ari____government_health_facility___urban)))
  total_children_treated_private_health_facility <- c(total_children_treated_private_health_facility, as.numeric(xmlValue(xmlChildren(item)$children_under_age_five_for_whom_treatment_was_sought_from_health_facility_with_symptoms_of_acute_respiratory_infection__ari____private_health_facility___total)))
  rural_children_treated_private_health_facility <- c(rural_children_treated_private_health_facility, as.numeric(xmlValue(xmlChildren(item)$children_under_age_five_for_whom_treatment_was_sought_from_health_facility_with_symptoms_of_acute_respiratory_infection__ari____private_health_facility___rural)))
  urban_children_treated_private_health_facility <- c(urban_children_treated_private_health_facility, as.numeric(xmlValue(xmlChildren(item)$children_under_age_five_for_whom_treatment_was_sought_from_health_facility_with_symptoms_of_acute_respiratory_infection__ari____private_health_facility___urban)))
}

# Create a data frame from the vectors
child_78 <- data.frame(
  Sl_No = sl_no,
  State = state,
  Total_Children_Suffered_ARI = total_children_suffered_ari,
  Rural_Children_Suffered_ARI = rural_children_suffered_ari,
  Urban_Children_Suffered_ARI = urban_children_suffered_ari,
  Total_Children_Treated_ARI = total_children_treated_ari,
  Rural_Children_Treated_ARI = rural_children_treated_ari,
  Urban_Children_Treated_ARI = urban_children_treated_ari,
  Total_Children_Treated_Government_Health_Facility = total_children_treated_government_health_facility,
  Rural_Children_Treated_Government_Health_Facility = rural_children_treated_government_health_facility,
  Urban_Children_Treated_Government_Health_Facility = urban_children_treated_government_health_facility,
  Total_Children_Treated_Private_Health_Facility = total_children_treated_private_health_facility,
  Rural_Children_Treated_Private_Health_Facility = rural_children_treated_private_health_facility,
  Urban_Children_Treated_Private_Health_Facility = urban_children_treated_private_health_facility
)





