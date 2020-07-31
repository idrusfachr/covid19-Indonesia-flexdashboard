library(httr)
library(jsonlite)
library(tidyr)

getDailyData <- function(endpoint){
  response <- GET(endpoint)
  text_json <- content(response, "text")
  text <- fromJSON(text_json, flatten = TRUE)
  text_df <- as.data.frame(text)
  
  #handle if todays data on API have not updated yet, then direct download the csv file
  if(is.na(text_df[text_df$features.properties.Tanggal == Sys.Date(), c("features.properties.Jumlah_Kasus_Kumulatif")]) == TRUE){
    return(read.csv("https://opendata.arcgis.com/datasets/db3be1cadcf44b6fa709274c12726c59_0.csv"))
  }
  else {
    return(text_df)
  }
  
}

getProvinceData <- function(endpoint){
  response <- GET(endpoint)
  text_json <- content(response, "text")
  text <- fromJSON(text_json, flatten = TRUE)
  text_df <- as.data.frame(text)
  
  #handle if todays data have not updated yet
  if(max(text_df$Tanggal) < Sys.Date()){
    return(read.csv("https://opendata.arcgis.com/datasets/685be21cd0034247b5ceeac996d947fe_0.csv"))
  }
  else {
    return(text_df)
  }
 
}

cleanData <- function(df) {
  #Remove "feature.properties.xxxx" prefix on column name
  #convert date from factor to Date format
  
  library(dplyr)
  cleaned_data <- df %>%
    rename_all(list(~ gsub("features.properties.", "", names(df)))) %>%
    mutate(Tanggal = as.Date(Tanggal))
  
  return(cleaned_data)
}



