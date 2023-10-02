# Load the required library
library(XML)
library(tidyverse)
library(dplyr)

# Define the URL for the GET request
url <- 'https://api.data.gov.in/resource/33a89527-b545-4389-8738-8d7736aca508?api-key=579b464db66ec23bdd0000015115b9e83ec44fb969475c8cdea3f9be&format=xml&limit=100000'

# Send a GET request and extract XML content as a character string
response <- httr::GET(url)
xml_content <- httr::content(response, "text", encoding = "UTF-8")

# Parse the XML content
xml_content <- xmlParse(xml_content, useInternalNodes = TRUE)

# Extract specific nodes from the XML
items <- getNodeSet(xml_content, "//item")

# Initialize empty vectors for each column
state <- character()
city <- character()
type <- character()
category_of_es <- character()
so2_annual_average_g_m3 <- character()
air_quality_of_so2 <- character()
no2_annual_average_g_m3 <- character()
air_quality_of_no2 <- character()
pm10_annual_average_g_m3 <- character()
air_quality_of_pm10 <- character()

# Loop through the extracted nodes and populate the vectors
for (item in items) {
  state <- c(state, xmlValue(xmlChildren(item)$state_))
  city <- c(city, xmlValue(xmlChildren(item)$city))
  type <- c(type, xmlValue(xmlChildren(item)$type))
  category_of_es <- c(category_of_es, xmlValue(xmlChildren(item)$category_of_es))
  so2_annual_average_g_m3 <- c(so2_annual_average_g_m3, xmlValue(xmlChildren(item)$so2_annual_average_g_m3_))
  air_quality_of_so2 <- c(air_quality_of_so2, xmlValue(xmlChildren(item)$air_quality_of_so2))
  no2_annual_average_g_m3 <- c(no2_annual_average_g_m3, xmlValue(xmlChildren(item)$no2_annual_average_g_m3_))
  air_quality_of_no2 <- c(air_quality_of_no2, xmlValue(xmlChildren(item)$air_quality_of_no2))
  pm10_annual_average_g_m3 <- c(pm10_annual_average_g_m3, xmlValue(xmlChildren(item)$pm10_annual_average_g_m3_))
  air_quality_of_pm10 <- c(air_quality_of_pm10, xmlValue(xmlChildren(item)$air_quality_of_pm10))
}

# Create a data frame from the vectors
air_quality <- data.frame(
  State = state,
  City = city,
  Type = type,
  Category_of_ES = category_of_es,
  SO2_Annual_Average_g_m3 = so2_annual_average_g_m3,
  Air_Quality_of_SO2 = air_quality_of_so2,
  NO2_Annual_Average_g_m3 = no2_annual_average_g_m3,
  Air_Quality_of_NO2 = air_quality_of_no2,
  PM10_Annual_Average_g_m3 = pm10_annual_average_g_m3,
  Air_Quality_of_PM10 = air_quality_of_pm10
)


