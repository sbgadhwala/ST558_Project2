ST558 Project 2
================
Shyam Gadhwala & Kamlesh Pandey

-   [Requirement](#requirement)
-   [Asteroid - NeoWs API](#asteroid---neows-api)
-   [Coronal Mass Ejection (CME)
    Analysis](#coronal-mass-ejection-cme-analysis)
    -   [Coronal Mass Ejection (CME) Analysis
        API:](#coronal-mass-ejection-cme-analysis-api)
    -   [Function to get the CME data:](#function-to-get-the-cme-data)
-   [Parent Wrapper Function](#parent-wrapper-function)
-   [Exploratory Data Analysis for Asteroid
    Data](#exploratory-data-analysis-for-asteroid-data)
-   [Exploratory Data Analysis (EDA) of Coronal Mass Ejection (CME)
    Analysis
    API.](#exploratory-data-analysis-eda-of-coronal-mass-ejection-cme-analysis-api)

This vignette is based on the NASA API. The primary purpose of this
vignette is to download data from the API and explore visualization
package ggplot for exploratory data analysis purpose.

[NASA API](https://api.nasa.gov/index.html)

# Requirement

For this project following packages are used.

[httr](https://httr.r-lib.org/) to provide a wrapper function and
customized to the demand of modern web APIs.

[jsonlite](https://cran.r-project.org/web/packages/jsonlite/vignettes/json-aaquickstart.html)
to provide flexibility in mapping json and R data

[lubridate](https://lubridate.tidyverse.org/) to manipulate date and
time

[ggplot](https://ggplot2.tidyverse.org/) used for creating graphics

[tidyverse](https://www.tidyverse.org/) for data analysis purpose

``` r
library(httr)
library(tidyverse)
library(jsonlite)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(jpeg)
library(lubridate)
library(GGally)
```

# Asteroid - NeoWs API

NeoWs (Near Earth object Web Service) is a REST API for near earth
Asteroid information. Near Earth Objects (NEOs) are comets and asteroids
and due to the gravitational attraction of nearby planet they enter into
earth gravitational orbit

This API has near earth objects (NEO) tracking and the data set has 99%
asteroids and only 1% comet data.

Parameter information:

Magnitude (H): Asteroid absolute magnitude (in general, smaller H
implies larger asteroid diameter). This parameter is undefined for
comets.

Close-Approach (CA) Date : Date (TBD) of closest Earth approach.

V Relative : Object velocity relative to the earth

Maximum_Diameter (miles): Estimated maximum diameter in miles

Minimum_Diameter (miles): Estimated minimum diameter in miles

``` r
asteroidData <- function(start_date, end_date, ...){
  baseURL <- 'https://api.nasa.gov/neo/rest/v1/'
  apiKey <- 'igUogzKaubKUi5TTgsbYcdVgU8pICrvizcCrCtY5'
  endpoint <- 'feed'
  url <- paste0(baseURL, endpoint, '?startDate=', start_date, '&endDate=', end_date,'&api_key=', apiKey)
  
  # GET request for API
  res <- GET(url)
  asteroidData <- fromJSON(rawToChar(res$content))
  
  # check for  correct date format
  dateFormat <- "%Y-%m-%d"
  
  # check if start and end dates are in correct format or not
  checkStart <- tryCatch(!is.na(as.Date(start_date, dateFormat)), 
             error = function(err) 
             {TRUE})
  checkEnd <- tryCatch(!is.na(as.Date(end_date, dateFormat)), 
             error = function(err) 
             {TRUE})
  
  # if eother the start or the end dates are not in correct format stop the program
  if (checkStart == FALSE | checkEnd == FALSE){
    message <- paste('[ERROR..!!] Either your start or end date is not is correct YYYY-MM-DD format')
    stop(message)
  }
  # if both start and end dates are in correct format 
  else if(checkStart == TRUE & checkEnd == TRUE){
    
    # check if end_date > start_date, if true stop the program
    if (ymd(end_date) < ymd(start_date)){
      message <- paste('End date' , end_date, 'should be greater than the start date', start_date)
      stop(message())
      
      } 
    # if dates are in correct format then execute the code
    else if (ymd(end_date) > ymd(start_date)){
        
        # check for the diff between the start and end date
        diffDate <- difftime(ymd(end_date), ymd(start_date), units = 'days')
        
        if (diffDate > 8){
          message <- paste('[WARNING..!!] The difference between the date range should be less than 8 days', 
                           'for this current date range the API will return future Approach date ')
          print(message)
        }
        
        # API Parameters      
        magnitude <- c()
        dMin  <- c()
        dMax <- c()
        velocity <- c()
        Date <- c()
        missDist <- c()
        orbitBody <- c()
        isHazard <- c()
          
        for (i in 1:length(asteroidData$near_earth_objects)) {
          absMag <- asteroidData$near_earth_objects[[i]]$absolute_magnitude_h
          dMin_  <- asteroidData$near_earth_objects[[i]]$estimated_diameter$miles$estimated_diameter_min
          dMax_ <- asteroidData$near_earth_objects[[i]]$estimated_diameter$miles$estimated_diameter_max
          isHazard_ <- asteroidData$near_earth_objects[[i]]$is_potentially_hazardous_asteroid
          
          tempDate <- c()
          tempVel <- c()
          tempDist <- c()
          tempOrbit <- c()
          
          # for close approach
          for (j in 1:length(asteroidData$near_earth_objects[[i]]$close_approach_data)){
            # approach velocity from the API
            vel <- asteroidData$near_earth_objects[[i]]$close_approach_data[[j]]$relative_velocity$kilometers_per_hour
            # storing in a temporary factor
            tempVel <- append(tempVel, vel)
            # getting date from the API
            date <- asteroidData$near_earth_objects[[i]]$close_approach_data[[j]]$close_approach_date
            # storing in a temporary factor
            tempDate <- append(tempDate, date)
            # miss distance from the API
            dist <- asteroidData$near_earth_objects[[i]]$close_approach_data[[j]]$miss_distance$astronomical
            # storing in a temporary factor
            tempDist <- append(tempDist, dist)
            # getting orbiting body from the API
            orbit <- asteroidData$near_earth_objects[[i]]$close_approach_data[[j]]$orbiting_body
            # storing in a temporary factor
            tempOrbit <- append(tempOrbit, orbit)
            
          }
          
          missDist <- append(missDist, as.numeric(tempDist))
          Date <- append(Date, tempDate)
          velocity <-append(velocity, as.numeric(tempVel))
          magnitude <- append(magnitude, absMag)
          dMin <- append(dMin, dMin_)
          dMax <- append(dMax, dMax_)
          orbitBody <- append(orbitBody, tempOrbit)
          isHazard <- append(isHazard, isHazard_)
          
  }
  
      }
  }
  #final tibble
  aesData <- tibble('Magnitude' = magnitude, 
                    'Minimum_Diameter' = dMin, 
                    'Maximum_Diameter' = dMax, 
                    'Relative_Velocity' = velocity,
                    'Approach_Date' = Date, 
                    'Miss_Distance' = missDist,
                    'Orbiting_Body' = orbitBody,
                    'Is_Potentially_Hazardous_Asteroid' = isHazard)

# This function will return the URL of the API and the dataset generated
return (list(url = url, data = aesData))
}
```

# Coronal Mass Ejection (CME) Analysis

The sun of our solar system has multiple atmospheric layers. The inner
layers are the Core, Radiative Zone and Convection Zone. The outer
layers are the Photo sphere, the Chromosphere, the Transition Region and
the Corona. Coronal Mass Ejection (CME) phenomena is when there are huge
explosions of plasma and field from the sun’s corona layer. These
explosions typically have certain path and speed when they are emitted
in the heliosphere. Sometimes intense CMEs travel at speeds more than
3000 km/s. The radiation or the solar wind takes up to 17-18hours to
reach earth. Hence, to predict severe solar radiation, we need to see
the characteristics of the explosions, and find any correlations between
its properties.

## Coronal Mass Ejection (CME) Analysis API:

This api is a part of Space Weather Database of Notifications,
Knowledge, Information (DONKI) online tool for space weather related
APIs and data. As a part of DONKI, the following api has been released
by NASA that helps with the data of such CME events:

[Coronal Mass Ejection
(CME)](https://api.nasa.gov/index.html#donkiCMEAnalysis)

Some of the modifications offered in this API are:  
\* Start Date: Desired starting date from which the data wants to be
collected (default value is 30 days prior to current UTC time) \* End
Date: Desired ending date till when the data wants to be collected
(default value is current UTC time) \* Most Accurate Only (default set
to True) \* Complete Entry Only (default set to True) \* Speed (lower
limit): Speed of the coronal mass ejection event (default set to 0) \*
Half Angle (lower limit): the angle of the coronal mass ejection event
with respect to vertical axis (default set to 0) \* Catalog (default set
to ALL) \* Keyword (default set to NONE)

In this vignette, we have focused on the main 4 modifications, \* Start
Date \* End Date \* Speed \* Half Angle

## Function to get the CME data:

This is a helper function that will call the API endpoint according to
the modification values. This function takes the above mentioned 4
modifications as input; the start date, the end date, minimum speed, and
minimum half angle.

The start date and end date are mandatory for any user to enter, while
speed and half angle both have a default value of 0, so if the values
for speed or half angle are not specified, it will take 0 as the input.

To make the API function more rigid, I have implemented many checks that
includes the start and end date format check, speed and half angle
values and data type check, and throw warnings and errors accordingly.
If the input data is correct then the modification values are appended
to the base URL of the API, which is called and a JSON is returned as a
result which will have the details of the CME event between the two
dates having mentioned speed and half angle characteristics.

The data from the JSON extracted here are: \* time: Time of the CME
event \* latitude: The latitude value of the coordinate where the CME
event took place \* longitude: The longitude value of the coordinate
where the CME event took place \* halfAngle: The half angle value of the
trajectory of the explosion \* speed: The speed of the explosion \*
type: Type of events (classifications include “C”, “O”, “R”, “S”)

A tibble is created with the above stated details and is returned from
the function along with the URL that was formed from the input values
for modifications.

``` r
cmeData <- function(startDate, endDate, speed = 0, halfAngle = 0, ...){
  baseUrl <- 'https://api.nasa.gov/DONKI/'
  apiKey <- 'igUogzKaubKUi5TTgsbYcdVgU8pICrvizcCrCtY5'
  
  checkStart <- !is.na(parse_date_time(startDate, orders = "ymd"))
  if(!checkStart){
    errorMessage <- "Please enter the Start Date in the YYYY-mm-dd format and try again."
    stop(errorMessage)
  }
  
  checkEnd <- !is.na(parse_date_time(endDate, orders = "ymd"))
  if(!checkEnd){
    errorMessage <- "Please enter the End Date in the YYYY-mm-dd format and try again."
    stop(errorMessage)
  }
  
  if (as.Date(startDate) > as.Date(endDate)){
    errorMessage <- "The start date cannot be after the end date. Please enter the dates again."
    stop(errorMessage)
  }
  
  if (!is.numeric(speed)){
    errorMessage <- "Speed can only take numeric values. Please enter speed again."
    stop(errorMessage)
  }
  
  if (!is.numeric(halfAngle)){
    errorMessage <- "Half Angle can only take numeric values. Please enter half angle again."
    stop(errorMessage)
  }
  
  if (speed < 0){
    warning("Warning: ", "The speed cannot be negative. Proceeding with its default value of 0.")
    speed = 0
  }
  
  if (halfAngle < 0){
    warning("Warning: ", "The half angle cannot be negative. Proceeding with its default value of 0.")
    halfAngle = 0
  }
  
  targetUrl <- paste0(baseUrl, "CMEAnalysis?", "startDate=", startDate, 
                      "&endDate=", endDate, "&speed=", speed,
                      "&halfAngle=", halfAngle, "&api_key=", apiKey)
  
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

# Parent Wrapper Function

The following the is the main parent wrapper function. This function
takes the following input:  
\* api: The API that the user is interested in calling (Coronal Mass
Ejection (CME) Analysis or Asteroids - NeoWs)

For this, the user can either give the abbreviated or the full name of
the api, either will lead to the same call of the designated API.

-   …: This takes the additional input for particular APIs. For Coronal
    Mass Ejection (CME) Analysis API, it will read the start date, end
    date, speed and half angle. For Asteroids - NeoWs, it will read the
    start date and end date.

Note: If any API other than the above mentioned two is called, an error
message will be thrown stating that this API is not yet supported.

``` r
apiSelection <- function(api, ...){
  
  if (tolower(api) == tolower("Coronal Mass Ejection (CME) Analysis") | 
      tolower(api) == tolower("cme")){
    return(cmeData(...))
    
  }
  else if(tolower(api) == tolower("Asteroids - NeoWs")){
    return(asteroidData(...))
    
  }
  else{
    return("This api is not yet supported. Please select from either 'Coronal Mass Ejection (CME) Analysis' or 'Asteroid (AST)'.")
  }
}
```

# Exploratory Data Analysis for Asteroid Data

Calling the API for Asteroid-NEOs from the wrapper function using sample
start and end date. Here I have selected start date as 2017-01-01, and
the end date as 2020-01-01.

When the API returns the data, the useful data is converted to a tibble
and is showed below. I have also called the summary function to get an
overview of the data.

``` r
astDf <- apiSelection(api = "Asteroids - NeoWs", '2017-01-01', '2020-01-01')$data
```

    ## [1] "[WARNING..!!] The difference between the date range should be less than 8 days for this current date range the API will return future Approach date "

``` r
summary(astDf %>% select(Magnitude:Miss_Distance))
```

    ##    Magnitude     Minimum_Diameter  
    ##  Min.   :15.25   Min.   :0.001896  
    ##  1st Qu.:20.43   1st Qu.:0.020792  
    ##  Median :21.57   Median :0.080150  
    ##  Mean   :22.25   Mean   :0.133732  
    ##  3rd Qu.:24.50   3rd Qu.:0.135490  
    ##  Max.   :29.70   Max.   :1.471994  
    ##  Maximum_Diameter  Relative_Velocity
    ##  Min.   :0.00424   Min.   :  6882   
    ##  1st Qu.:0.04649   1st Qu.: 37687   
    ##  Median :0.17922   Median : 50262   
    ##  Mean   :0.29903   Mean   : 52273   
    ##  3rd Qu.:0.30296   3rd Qu.: 66063   
    ##  Max.   :3.29148   Max.   :123714   
    ##  Approach_Date      Miss_Distance     
    ##  Length:73          Min.   :0.003582  
    ##  Class :character   1st Qu.:0.193351  
    ##  Mode  :character   Median :0.293841  
    ##                     Mean   :0.284901  
    ##                     3rd Qu.:0.390529  
    ##                     Max.   :0.498633

``` r
# EDA on Asteroid data

plot1 <- ggplot(astDf, aes(x = Approach_Date, y = Miss_Distance))

plot1 + geom_point(aes(color = Is_Potentially_Hazardous_Asteroid, size = Maximum_Diameter), alpha = 0.7) +
  theme(axis.text.x = element_text(angle = 45), axis.text.y = element_blank())+
  theme(axis.text.y = element_blank(), 
        axis.ticks.y = element_blank()
        )
```

![](README_files/figure-gfm/unnamed-chunk-28-1.png)<!-- -->

``` r
# boxplot for min and max diamter

par(mfrow = c(1,2))
plot2 <- ggplot(astDf, aes(x = Is_Potentially_Hazardous_Asteroid,  y = Minimum_Diameter))

plot2 + 
  geom_boxplot() + geom_point(aes(color = Is_Potentially_Hazardous_Asteroid), position = 'jitter') + 
  labs('Box Plot for Minimum Diameter') + 
  xlab('If Asteroid Hazardous ') + 
  ylab('Minimum Diamter')
```

![](README_files/figure-gfm/unnamed-chunk-29-1.png)<!-- -->

``` r
library(ggplot2)

# correlation plot
numericalDf <- astDf [, c(1,2,3,4,6)]  

ggpairs(numericalDf, color = 'red', main = 'Scatter Plot',
        color = 'Is_Potentially_Hazardous_Asteroid')
```

![](README_files/figure-gfm/unnamed-chunk-30-1.png)<!-- -->

# Exploratory Data Analysis (EDA) of Coronal Mass Ejection (CME) Analysis API.

Calling the API for CME from the wrapper function using some sample
start and end date. Here I have selected start date as 2017-01-01, and
the end date as 2020-01-01. The speed and half angle are kept as default
that is 0.

When the API returns the data, the useful data is converted to a tibble
and is showed below. I have also called the summary function to get an
overview of the data.

``` r
cmeSampleData <- apiSelection("Coronal Mass Ejection (CME) Analysis", "2017-01-01", "2020-01-01")$data

print(cmeSampleData)
```

    ## # A tibble: 324 x 6
    ##    time    latitude longitude halfAngle
    ##    <chr>      <dbl>     <dbl>     <dbl>
    ##  1 2017-0~      -10      -105        20
    ##  2 2017-0~      -33       -93        26
    ##  3 2017-0~        3       100        22
    ##  4 2017-0~        1       -90        36
    ##  5 2017-0~       10      -155        10
    ##  6 2017-0~       11        90        15
    ##  7 2017-0~        0       -48        14
    ##  8 2017-0~       11       160        11
    ##  9 2017-0~       16       -80        21
    ## 10 2017-0~       21       105        26
    ## # ... with 314 more rows, and 2 more
    ## #   variables: speed <dbl>, type <chr>

``` r
summary(cmeSampleData %>% select(halfAngle, speed, latitude, longitude))
```

    ##    halfAngle         speed       
    ##  Min.   : 2.00   Min.   :  88.0  
    ##  1st Qu.:16.00   1st Qu.: 281.5  
    ##  Median :22.00   Median : 359.0  
    ##  Mean   :22.27   Mean   : 413.9  
    ##  3rd Qu.:27.00   3rd Qu.: 459.0  
    ##  Max.   :54.00   Max.   :2650.0  
    ##     latitude         longitude     
    ##  Min.   :-55.000   Min.   :-178.0  
    ##  1st Qu.: -7.000   1st Qu.: -88.0  
    ##  Median :  0.000   Median :   1.5  
    ##  Mean   :  2.506   Mean   :   2.5  
    ##  3rd Qu.: 11.000   3rd Qu.:  92.0  
    ##  Max.   : 90.000   Max.   : 178.0

To start with the initial EDA, using ggplot2 library, I have made a
scatter plot representing the latitude and longitude of each of the CME
events taking place on sun’s atmosphere. The different colored dots
represent the different event types, while the size of the dots
represent the spped of explosion. the smaller the dot, the lesser the
speed of the event and vice versa.

``` r
img <- readJPEG("img\\sun2.jpeg")

cmeSampleData$type <- as.factor(cmeSampleData$type)

show(ggplot(cmeSampleData, aes(x=latitude, y=longitude)) +
    geom_point(aes(color = type, size = speed)) +
    ylim(-180,180) +
    xlim(-90, 90)  +
  labs(title="Plot showing Corona Mass Ejection on Sun's corona",
        x ="Latitude", y = "Longitude"))
```

![](README_files/figure-gfm/unnamed-chunk-31-1.png)<!-- -->

To see it more in respect to the actual sun’s corona layer, I have
imported an image of sun and use it as a background to give a better
perspective of where the events took place. (the sun’s corona layer to
CME events is not to scale, and is only for representational purposes).

``` r
ggplot(cmeSampleData, aes(x=latitude, y=longitude)) +
    background_image(img) +
    geom_point(aes(color = type, size = speed)) +
    ylim(-180,180) +
    xlim(-90, 90)  +
  labs(title="Plot showing Corona Mass Ejection on Sun's corona",
        x ="Latitude", y = "Longitude")
```

![](README_files/figure-gfm/inital_plot-1.png)<!-- -->

To gain more insights about the data, I have segregated the speed,
location, and half angle from the data.  
\* The speed has 4 classifications; Slow Paced, Medium Paced, Fast Paced
and Hyper Paced. \* Location is divided into 4 zones based on their
latitude and longitude; North-East, North-West, South-East and
South-West. \* Half Angles are classified into 3 categories; low, medium
and high

After adding these classified variables, I am again printing the data.

``` r
cmeSampleData$type <- as.factor(cmeSampleData$type)

speedClassfication <- c("Slow Paced", "Medium Paced", "Fast Paced", "Hyper Paced")

cmeSampleData <- cmeSampleData %>% 
  mutate(speedC = as.factor(if_else(speed < 500, speedClassfication[1],
                                            if_else(speed < 1000,  speedClassfication[2],
                                                    if_else(speed < 2000, speedClassfication[3], speedClassfication[4])))))


zones <- c("North-East", "North-West", "South-East", "South-West")
cmeSampleData <- cmeSampleData %>%
  mutate(zone = as.factor(if_else(latitude>=0 & longitude>=0, zones[1],
                        if_else(latitude<=0 & longitude>=0,zones[2],
                                if_else(latitude<=0 & longitude<=0, zones[4],
                                        if_else(latitude>=0 & longitude<=0, zones[3], "Error"))))))

angles <- c("low", "medium", "high")
cmeSampleData <- cmeSampleData %>% 
  mutate(halfAngleC = as.factor(if_else(halfAngle <= 25, angles[1],
                                            if_else(halfAngle <= 45,  angles[2],
                                                     angles[3]))))

cmeSampleData
```

    ## # A tibble: 324 x 9
    ##    time    latitude longitude halfAngle
    ##    <chr>      <dbl>     <dbl>     <dbl>
    ##  1 2017-0~      -10      -105        20
    ##  2 2017-0~      -33       -93        26
    ##  3 2017-0~        3       100        22
    ##  4 2017-0~        1       -90        36
    ##  5 2017-0~       10      -155        10
    ##  6 2017-0~       11        90        15
    ##  7 2017-0~        0       -48        14
    ##  8 2017-0~       11       160        11
    ##  9 2017-0~       16       -80        21
    ## 10 2017-0~       21       105        26
    ## # ... with 314 more rows, and 5 more
    ## #   variables: speed <dbl>,
    ## #   type <fct>, speedC <fct>,
    ## #   zone <fct>, halfAngleC <fct>

Then from the data, I have grouped the data by combining zone, speed and
type of event, and for each group, I have calculated the number of
events, average Speed, Standard deviation of speed, average half angle
and standard deviation of half angle.

``` r
cmeSampleData %>%
  group_by(zone, speedC, type) %>%
  summarize(avgSpeed = mean(speed), sdSpeed = sd(speed),
            avgHalfAngle = mean(halfAngle), 
            sdHalfAngle = sd(halfAngle), count = n()) %>% arrange(zone, speedC)
```

    ## # A tibble: 11 x 8
    ## # Groups:   zone, speedC [11]
    ##    zone   speedC type  avgSpeed sdSpeed
    ##    <fct>  <fct>  <fct>    <dbl>   <dbl>
    ##  1 North~ Mediu~ C         627.   100. 
    ##  2 North~ Slow ~ S         325.    88.3
    ##  3 North~ Fast ~ O        1177.    62.0
    ##  4 North~ Hyper~ R        2460.   268. 
    ##  5 North~ Mediu~ C         620.    82.6
    ##  6 North~ Slow ~ S         326.    95.1
    ##  7 South~ Mediu~ C         634.   126. 
    ##  8 South~ Slow ~ S         313.    90.5
    ##  9 South~ Fast ~ O        1437     32.0
    ## 10 South~ Mediu~ C         691.   173. 
    ## 11 South~ Slow ~ S         328.    81.7
    ## # ... with 3 more variables:
    ## #   avgHalfAngle <dbl>,
    ## #   sdHalfAngle <dbl>, count <int>

Then contingency table is created for zone, speed and type of events,
and is showed below.

``` r
print(table(cmeSampleData$zone, cmeSampleData$speedC, cmeSampleData$type))
```

    ## , ,  = C
    ## 
    ##             
    ##              Fast Paced Hyper Paced
    ##   North-East          0           0
    ##   North-West          0           0
    ##   South-East          0           0
    ##   South-West          0           0
    ##             
    ##              Medium Paced Slow Paced
    ##   North-East           19          0
    ##   North-West           10          0
    ##   South-East           18          0
    ##   South-West           13          0
    ## 
    ## , ,  = O
    ## 
    ##             
    ##              Fast Paced Hyper Paced
    ##   North-East          0           0
    ##   North-West          3           0
    ##   South-East          0           0
    ##   South-West          3           0
    ##             
    ##              Medium Paced Slow Paced
    ##   North-East            0          0
    ##   North-West            0          0
    ##   South-East            0          0
    ##   South-West            0          0
    ## 
    ## , ,  = R
    ## 
    ##             
    ##              Fast Paced Hyper Paced
    ##   North-East          0           0
    ##   North-West          0           2
    ##   South-East          0           0
    ##   South-West          0           0
    ##             
    ##              Medium Paced Slow Paced
    ##   North-East            0          0
    ##   North-West            0          0
    ##   South-East            0          0
    ##   South-West            0          0
    ## 
    ## , ,  = S
    ## 
    ##             
    ##              Fast Paced Hyper Paced
    ##   North-East          0           0
    ##   North-West          0           0
    ##   South-East          0           0
    ##   South-West          0           0
    ##             
    ##              Medium Paced Slow Paced
    ##   North-East            0         75
    ##   North-West            0         56
    ##   South-East            0         54
    ##   South-West            0         71

``` r
print(knitr::kable(table(cmeSampleData$zone, cmeSampleData$speedC)))
```

    ## 
    ## 
    ## |           | Fast Paced| Hyper Paced| Medium Paced| Slow Paced|
    ## |:----------|----------:|-----------:|------------:|----------:|
    ## |North-East |          0|           0|           19|         75|
    ## |North-West |          3|           2|           10|         56|
    ## |South-East |          0|           0|           18|         54|
    ## |South-West |          3|           0|           13|         71|

``` r
print(knitr::kable(table(cmeSampleData$zone, cmeSampleData$speedC, cmeSampleData$type)))
```

    ## 
    ## 
    ## |Var1       |Var2         |Var3 | Freq|
    ## |:----------|:------------|:----|----:|
    ## |North-East |Fast Paced   |C    |    0|
    ## |North-West |Fast Paced   |C    |    0|
    ## |South-East |Fast Paced   |C    |    0|
    ## |South-West |Fast Paced   |C    |    0|
    ## |North-East |Hyper Paced  |C    |    0|
    ## |North-West |Hyper Paced  |C    |    0|
    ## |South-East |Hyper Paced  |C    |    0|
    ## |South-West |Hyper Paced  |C    |    0|
    ## |North-East |Medium Paced |C    |   19|
    ## |North-West |Medium Paced |C    |   10|
    ## |South-East |Medium Paced |C    |   18|
    ## |South-West |Medium Paced |C    |   13|
    ## |North-East |Slow Paced   |C    |    0|
    ## |North-West |Slow Paced   |C    |    0|
    ## |South-East |Slow Paced   |C    |    0|
    ## |South-West |Slow Paced   |C    |    0|
    ## |North-East |Fast Paced   |O    |    0|
    ## |North-West |Fast Paced   |O    |    3|
    ## |South-East |Fast Paced   |O    |    0|
    ## |South-West |Fast Paced   |O    |    3|
    ## |North-East |Hyper Paced  |O    |    0|
    ## |North-West |Hyper Paced  |O    |    0|
    ## |South-East |Hyper Paced  |O    |    0|
    ## |South-West |Hyper Paced  |O    |    0|
    ## |North-East |Medium Paced |O    |    0|
    ## |North-West |Medium Paced |O    |    0|
    ## |South-East |Medium Paced |O    |    0|
    ## |South-West |Medium Paced |O    |    0|
    ## |North-East |Slow Paced   |O    |    0|
    ## |North-West |Slow Paced   |O    |    0|
    ## |South-East |Slow Paced   |O    |    0|
    ## |South-West |Slow Paced   |O    |    0|
    ## |North-East |Fast Paced   |R    |    0|
    ## |North-West |Fast Paced   |R    |    0|
    ## |South-East |Fast Paced   |R    |    0|
    ## |South-West |Fast Paced   |R    |    0|
    ## |North-East |Hyper Paced  |R    |    0|
    ## |North-West |Hyper Paced  |R    |    2|
    ## |South-East |Hyper Paced  |R    |    0|
    ## |South-West |Hyper Paced  |R    |    0|
    ## |North-East |Medium Paced |R    |    0|
    ## |North-West |Medium Paced |R    |    0|
    ## |South-East |Medium Paced |R    |    0|
    ## |South-West |Medium Paced |R    |    0|
    ## |North-East |Slow Paced   |R    |    0|
    ## |North-West |Slow Paced   |R    |    0|
    ## |South-East |Slow Paced   |R    |    0|
    ## |South-West |Slow Paced   |R    |    0|
    ## |North-East |Fast Paced   |S    |    0|
    ## |North-West |Fast Paced   |S    |    0|
    ## |South-East |Fast Paced   |S    |    0|
    ## |South-West |Fast Paced   |S    |    0|
    ## |North-East |Hyper Paced  |S    |    0|
    ## |North-West |Hyper Paced  |S    |    0|
    ## |South-East |Hyper Paced  |S    |    0|
    ## |South-West |Hyper Paced  |S    |    0|
    ## |North-East |Medium Paced |S    |    0|
    ## |North-West |Medium Paced |S    |    0|
    ## |South-East |Medium Paced |S    |    0|
    ## |South-West |Medium Paced |S    |    0|
    ## |North-East |Slow Paced   |S    |   75|
    ## |North-West |Slow Paced   |S    |   56|
    ## |South-East |Slow Paced   |S    |   54|
    ## |South-West |Slow Paced   |S    |   71|

To further see the statistics about the CME events in each zone, I have
plotted the following The 4 different colors represent 4 different
zones, the shape of each event represents the type of the event, and the
size of each events represents the speed of ejection during the CME
event (Sun’s surface to event locations is not to scale and is only for
representation purposes).

``` r
#img <- readJPEG("img\\sun2.jpeg")

ggplot(cmeSampleData, aes(x=latitude, y=longitude)) +
    background_image(img) +
    geom_point(aes(color = zone, size = speedC, shape = type)) +
    #scale_shape_discrete(name = "Type", labels = c("S", "C")) +
    #scale_color_manual(values = c("C" = "#37a0bf", "S" = "green")) +
    scale_size_discrete(name = "speed", labels = c(speedClassfication[1], speedClassfication[2], speedClassfication[3], speedClassfication[4])) + 
    ylim(-180,180) +
    xlim(-90, 90) +
  
    annotate(geom="text", x=70, y=150, label=paste0("North-East Region\ncount : ", nrow(cmeSampleData %>% filter(zone==zones[1])), "\navgSpeed : ", round(mean((cmeSampleData %>% filter(zone == zones[1]))$speed), 2),
                                                  "\navgHalfAngle : ", round(mean((cmeSampleData %>% filter(zone == zones[1]))$halfAngle), 2)),
              color="White", size=4) + 
  
  annotate(geom="text", x=-70, y=150, label=paste0("North-West Region\ncount : ", nrow(cmeSampleData %>% filter(zone==zones[2])), "\navgSpeed : ", round(mean((cmeSampleData %>% filter(zone == zones[2]))$speed), 2),
                                                  "\navgHalfAngle : ", round(mean((cmeSampleData %>% filter(zone == zones[2]))$halfAngle), 2)),
              color="White", size=4) + 
  
  annotate(geom="text", x=-70, y=-150, label=paste0("South-West Region\ncount : ", nrow(cmeSampleData %>% filter(zone==zones[4])), "\navgSpeed : ", round(mean((cmeSampleData %>% filter(zone == zones[4]))$speed), 2),
                                                  "\navgHalfAngle : ", round(mean((cmeSampleData %>% filter(zone == zones[4]))$halfAngle), 2)),
              color="White", size=4) + 
  
  annotate(geom="text", x=70, y=-150, label=paste0("South-East Region\ncount : ", nrow(cmeSampleData %>% filter(zone==zones[3])), "\navgSpeed : ", round(mean((cmeSampleData %>% filter(zone == zones[3]))$speed), 2),
                                                  "\navgHalfAngle : ", round(mean((cmeSampleData %>% filter(zone == zones[3]))$halfAngle), 2)),
              color="White", size=4) +
  labs(title="Plot showing zone-wise Corona Mass Ejection statistics",
        x ="Latitude", y = "Longitude")
```

![](README_files/figure-gfm/cme_con_tbls-1.png)<!-- -->

A correlation plot between speed and half angle is also shown. A linear
model regression line is also fitted with the help of geom_smooth
function.

``` r
cor <- cor(cmeSampleData$halfAngle, cmeSampleData$speed)

ggplot(cmeSampleData, aes(x=halfAngle, y=speed)) +
  geom_point(aes(color = type)) + 
  geom_smooth(method = "lm",color="black") + 
  labs(title="Scatter Plot showing relation between speed and half angle of the CME event",
        x ="Half Angle", y = "Speed") + 
  annotate(geom="text", x=10, y=2000, label=paste0("Correlation between \nSpeed and Half angle = ", round(cor, 3)))
```

![](README_files/figure-gfm/cor-1.png)<!-- -->

Then for each zone, I have plotted the bar plots representing count of
CME events that are classified by their half angle values.

``` r
ggplot(cmeSampleData, aes(x = type)) + 
  geom_bar(aes(fill = halfAngleC), position = "dodge") + 
  scale_fill_discrete(name = "Half Angle") + 
  facet_grid(. ~ zone) + 
  labs(title="Bar plot showing count of CME events in each zone \nclassified into each type and half angle of the events ",
        x ="Type", y = "Count of CME events")
```

![](README_files/figure-gfm/barplot-1.png)<!-- -->

Lastly, I took the dates (time-stamp) of events, and extracted months
from them, and plotted a histogram the count of CME events occurring in
each month between the given input start and end dates, classified into
the type of events on the histograms bins.

``` r
dates = c()
for (i in 1:nrow(cmeSampleData)){
  dates <- append(dates, strsplit(cmeSampleData$time[i], split="T")[[1]][1])
}
cmeSampleData$date <- dates

months = c()
for (i in 1:nrow(cmeSampleData)){
  months <- append(months, strsplit(cmeSampleData$date[i], split="-")[[1]][2])
}
cmeSampleData$nummonth <- as.numeric(months)
allmonths <- c("Jan","Feb","Mar",
              "Apr","May","Jun",
              "Jul","Aug","Sep",
              "Oct","Nov","Dec")
cmeSampleData$month <- allmonths[cmeSampleData$nummonth]
cmeSampleData$month <- as.factor(cmeSampleData$month)

cmeSampleData$month <- factor(cmeSampleData$month, ordered = TRUE, levels = c("Jan","Feb","Mar",
              "Apr","May","Jun",
              "Jul","Aug","Sep",
              "Oct","Nov","Dec"))

ggplot(cmeSampleData %>% group_by(month) %>% mutate(count = n()), aes(x=month))+
  geom_histogram(aes(fill=type), stat="count") + 
  scale_fill_discrete(name = "Type") + 
  labs(title="Histogram showing count of CME events in month \nthat occurred between the given dates.",
        x ="Month", y = "Count of CME events")
```

    ## Warning: Ignoring unknown parameters:
    ## binwidth, bins, pad

![](README_files/figure-gfm/hist-1.png)<!-- -->
