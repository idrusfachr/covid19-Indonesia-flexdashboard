library(httr)
library(jsonlite)
library(tidyr)

getData <- function(endpoint){
  response <- GET(endpoint)
  text_json <- content(response, "text")
  text <- fromJSON(text_json, flatten = TRUE)
  text_df <- as.data.frame(text)

}


cleanData <- function(df) {
  #Remove "feature.properties.xxxx" prefix on column name
  
  library(dplyr)
  cleaned_data <- df %>%
    rename_all(list(~ gsub("features.properties.|update.", "", names(df))))
  
  return(cleaned_data)
}



