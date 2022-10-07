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

# The NASA API has

``` r
URL     <- 'https://api.nasa.gov/DONKI/'

api_key <-  'igUogzKaubKUi5TTgsbYcdVgU8pICrvizcCrCtY5'

fillapi <- function(url, startEnd, endDate){
  return(paste0(url, "CME?", "startDate=", startEnd, "&endDate=", endDate, "&api_key=", api_key))
}

# Accessing data from API
#res <- GET(url = paste(URL, api.key, sep = '?api_key='))
res <- GET(fillapi(URL, "2019-01-01", "2021-01-01"))

#print(str(res, max.level = 1))

# converting into a data frame

nasa.data <- fromJSON(rawToChar(res$content))

# to know more about the structure of the datatset
#print(str(nasa.data, max.level = 1))

#print(nasa.data)
#names(nasa.data)
```

``` r
op <- as_tibble(matrix(nasa.data$cmeAnalyses))
op
```

    ## # A tibble: 210 × 1
    ##    V1           
    ##    <list>       
    ##  1 <df [1 × 11]>
    ##  2 <df [1 × 11]>
    ##  3 <df [1 × 11]>
    ##  4 <df [1 × 11]>
    ##  5 <df [2 × 11]>
    ##  6 <df [2 × 11]>
    ##  7 <df [3 × 11]>
    ##  8 <df [1 × 11]>
    ##  9 <df [1 × 11]>
    ## 10 <df [1 × 11]>
    ## # … with 200 more rows

``` r
#op$centroid_coordinates
```
