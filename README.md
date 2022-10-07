ST558 Project 2
================
Shyam Gadhwala & Kamlesh Pandey
9/28/2022

# About the API

DONKI

3.  

# Required library

Following packages are used for this project

``` r
library(httr)
library(tidyverse)
library(jsonlite)
library(dplyr)
```

``` r
asteroidData <- function(start_date, end_date){
  baseURL <- 'https://api.nasa.gov/neo/rest/v1/'
  apiKey <- 'igUogzKaubKUi5TTgsbYcdVgU8pICrvizcCrCtY5'
  endpoint <- 'feed'
  url <- paste0(baseURL, endpoint, '?startDate=', start_date, '&endDate=', end_date,'&api_key=', apiKey)
  
  res <- GET(url)
  asteroidData <- fromJSON(rawToChar(res$content))
  
  ######### only seven days limit
  magnitude <- c()
  dMin  <- c()
  dMax <- c()
  velocity <- c()
  Date <- c()
  missDist <- c()
  
  for (i in 1:length(asteroidData$near_earth_objects)) {
    absMag <- asteroidData$near_earth_objects[[i]]$absolute_magnitude_h
    dMin_  <- asteroidData$near_earth_objects[[i]]$estimated_diameter$miles$estimated_diameter_min
    dMax_ <- asteroidData$near_earth_objects[[i]]$estimated_diameter$miles$estimated_diameter_max
    tempDate <- c()
    tempVel <- c()
    tempDist <- c()
    
     # for close approach
    for (j in 1:length(asteroidData$near_earth_objects[[i]]$close_approach_data)){
      vel <- asteroidData$near_earth_objects[[i]]$close_approach_data[[j]]$relative_velocity$miles_per_hour
      tempVel <- append(tempVel, vel)
      date <- asteroidData$near_earth_objects[[i]]$close_approach_data[[j]]$close_approach_date
      tempDate <- append(tempDate, date)
      dist <- asteroidData$near_earth_objects[[i]]$close_approach_data[[j]]$miss_distance$miles
      tempDist <- append(tempDist, dist)
    }
    
    missDist <- append(missDist, tempDist)
    Date <- append(Date, tempDate)
    velocity <-append(velocity, tempVel)
    magnitude <- append(magnitude, absMag)
    dMin <- append(dMin, dMin_)
    dMax <- append(dMax, dMax_)
    
  }
  aesData <- tibble(Magnitude = magnitude, Minimum_Diameter =dMin, Maximum_Diameter = dMax, Approach_Velocity = velocity, Approach_Date = Date, Miss_Distance = missDist)
  
return (list(url = url, data = aesData))
}
```

``` r
cmeData <- function(startDate, endDate, speed = 0, halfAngle = 0){
  baseUrl <- 'https://api.nasa.gov/DONKI/'
  apiKey <- 'igUogzKaubKUi5TTgsbYcdVgU8pICrvizcCrCtY5'
  
  targetUrl <- paste0(baseUrl, "CMEAnalysis?", "startDate=", startDate, 
                      "&endDate=", endDate, "&speed=", speed,
                      "&halfAngle=", halfAngle, "&api_key=", api_key)
  
  #TODO - if else checks for data types and everything, EDA
  
  jsonContent <- fromJSON(rawToChar(GET(targetUrl)$content))
  
  time <- jsonContent$time21_5
  lat <- jsonContent$latitude
  lon <- jsonContent$longitude
  halfAngle_ <- jsonContent$halfAngle
  speed_ <- jsonContent$speed
  type <- jsonContent$type
  
  cmeDataTibble <- tibble(time = time, latitude = lat, longitude = lon, halfAngle = halfAngle_,speed = speed_, type = type)

  
  return(list(url = targetUrl, data = cmeDataTibble))
}
```

``` r
apiSelection <- function(api, ...){
  
  if (tolower(api) == tolower("Coronal Mass Ejection (CME) Analysis")){
    # call the cme function
    cmeData(...)
    
  }
  else if(tolower(api) == tolower("Interplanetary Shock (IPS)")){
    # call the ips function
    
  }
  else{
    return("This api is not yet supported. Please select from either 'Coronal Mass Ejection (CME) Analysis' or 'Interplanetary Shock (IPS)'.")
  }
}
```
