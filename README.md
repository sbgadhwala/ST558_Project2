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
library(jsonlite)
library(dplyr)
```

# The NASA API has

``` r
URL     <- 'https://api.nasa.gov/EPIC/api/natural/images'

api.key <-  'Z42ast1lzFG4H5UE4FxRuYTVIrKPtYZqgoAzNM8A'

# Accessing data from API
res <- GET(paste(URL, api.key, sep = '?api_key='))

str(res, max.level = 1)
```

    ## List of 10
    ##  $ url        : chr "https://api.nasa.gov/EPIC/api/natural?api_key=Z42ast1lzFG4H5UE4FxRuYTVIrKPtYZqgoAzNM8A"
    ##  $ status_code: int 200
    ##  $ headers    :List of 21
    ##   ..- attr(*, "class")= chr [1:2] "insensitive" "list"
    ##  $ all_headers:List of 2
    ##  $ cookies    :'data.frame': 2 obs. of  7 variables:
    ##  $ content    : raw [1:11781] 5b 7b 22 69 ...
    ##  $ date       : POSIXct[1:1], format: "2022-10-01 02:33:51"
    ##  $ times      : Named num [1:6] 0.9866 0.0841 0.1873 0.503 1.9831 ...
    ##   ..- attr(*, "names")= chr [1:6] "redirect" "namelookup" "connect" "pretransfer" ...
    ##  $ request    :List of 7
    ##   ..- attr(*, "class")= chr "request"
    ##  $ handle     :Class 'curl_handle' <externalptr> 
    ##  - attr(*, "class")= chr "response"

``` r
# converting into a data frame

nasa.data <- fromJSON(rawToChar(res$content))

# to know more about the structure of the datatset
str(nasa.data, max.level = 1)
```

    ## 'data.frame':    12 obs. of  11 variables:
    ##  $ identifier           : chr  "20220929000830" "20220929015632" "20220929034434" "20220929053237" ...
    ##  $ caption              : chr  "This image was taken by NASA's EPIC camera onboard the NOAA DSCOVR spacecraft" "This image was taken by NASA's EPIC camera onboard the NOAA DSCOVR spacecraft" "This image was taken by NASA's EPIC camera onboard the NOAA DSCOVR spacecraft" "This image was taken by NASA's EPIC camera onboard the NOAA DSCOVR spacecraft" ...
    ##  $ image                : chr  "epic_1b_20220929000830" "epic_1b_20220929015632" "epic_1b_20220929034434" "epic_1b_20220929053237" ...
    ##  $ version              : chr  "03" "03" "03" "03" ...
    ##  $ centroid_coordinates :'data.frame':   12 obs. of  2 variables:
    ##  $ dscovr_j2000_position:'data.frame':   12 obs. of  3 variables:
    ##  $ lunar_j2000_position :'data.frame':   12 obs. of  3 variables:
    ##  $ sun_j2000_position   :'data.frame':   12 obs. of  3 variables:
    ##  $ attitude_quaternions :'data.frame':   12 obs. of  4 variables:
    ##  $ date                 : chr  "2022-09-29 00:03:42" "2022-09-29 01:51:44" "2022-09-29 03:39:46" "2022-09-29 05:27:48" ...
    ##  $ coords               :'data.frame':   12 obs. of  5 variables:

``` r
names(nasa.data)
```

    ##  [1] "identifier"            "caption"              
    ##  [3] "image"                 "version"              
    ##  [5] "centroid_coordinates"  "dscovr_j2000_position"
    ##  [7] "lunar_j2000_position"  "sun_j2000_position"   
    ##  [9] "attitude_quaternions"  "date"                 
    ## [11] "coords"

``` r
op <- as_tibble(nasa.data$coords)
op$centroid_coordinates
```

    ##          lat         lon
    ## 1  -1.889648 -172.902832
    ## 2  -1.853027  160.056152
    ## 3  -1.882324  133.059082
    ## 4  -1.933594  106.040039
    ## 5  -1.940918   79.064941
    ## 6  -1.933594   52.075195
    ## 7  -1.977539   25.004883
    ## 8  -1.999512   -1.977539
    ## 9  -1.977539  -29.003906
    ## 10 -1.977539  -55.986328
    ## 11 -1.984863  -83.020020
    ## 12 -2.065430 -137.043457





function (ar1, ar2 = "strng", arg3 = 'string2') {
  var 1 = ''
  if tolower(arg1) == 'syring'{
  var1 = 'new term'
  
  }
  api






}
