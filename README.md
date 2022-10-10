ST558 Project 2
================
Shyam Gadhwala & Kamlesh Pandey

``` r
library(httr)
library(tidyverse)
library(jsonlite)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(jpeg)
library(lubridate)
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

``` r
asteroidData <- function(start_date, end_date, ...){
  baseURL <- 'https://api.nasa.gov/neo/rest/v1/'
  apiKey <- 'igUogzKaubKUi5TTgsbYcdVgU8pICrvizcCrCtY5'
  endpoint <- 'feed'
  url <- paste0(baseURL, endpoint, '?startDate=', start_date, '&endDate=', end_date,'&api_key=', apiKey)
  
  res <- GET(url)
  asteroidData <- fromJSON(rawToChar(res$content))
  
  # check for date format
  dateFormat <- "%Y-%m-%d"
  
  checkStart <- tryCatch(!is.na(as.Date(start_date, dateFormat)), 
             error = function(err) 
             {TRUE})
  
  checkEnd <- tryCatch(!is.na(as.Date(end_date, dateFormat)), 
             error = function(err) 
             {TRUE})
  
  # First Check for date format
  if (checkStart == FALSE | checkEnd == FALSE){
    message <- paste('[ERROR..!!] Either your start or end date is not is correct YYYY-MM-DD format')
    print(message)
    stop(message)
  }
  else if(checkStart == TRUE & checkEnd == TRUE){
    #check if end_date > start_date
    if (ymd(end_date) < ymd(start_date)){
      message <- paste('End date' , end_date, 'should be greater than the start date', start_date)
      print(message)
      stop(message())
      
      } else if (ymd(end_date) > ymd(start_date)){
        
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
            vel <- asteroidData$near_earth_objects[[i]]$close_approach_data[[j]]$relative_velocity$kilometers_per_hour
            tempVel <- append(tempVel, vel)
            date <- asteroidData$near_earth_objects[[i]]$close_approach_data[[j]]$close_approach_date
            tempDate <- append(tempDate, date)
            dist <- asteroidData$near_earth_objects[[i]]$close_approach_data[[j]]$miss_distance$astronomical
            tempDist <- append(tempDist, dist)
            orbit <- asteroidData$near_earth_objects[[i]]$close_approach_data[[j]]$orbiting_body
            tempOrbit <- append(tempOrbit, orbit)
            
          }
          
          missDist <- append(missDist, tempDist)
          Date <- append(Date, tempDate)
          velocity <-append(velocity, tempVel)
          magnitude <- append(magnitude, absMag)
          dMin <- append(dMin, dMin_)
          dMax <- append(dMax, dMax_)
          orbitBody <- append(orbitBody, tempOrbit)
          isHazard <- append(isHazard, isHazard_)
          
  }
  
      }
  }
  aesData <- tibble('Magnitude' = magnitude, 
                    'Minimum_Diameter' = dMin, 
                    'Maximum_Diameter' = dMax, 
                    'Relative_Velocity' = velocity,
                    'Approach_Date' = Date, 
                    'Miss_Distance' = missDist,
                    'Orbiting_Body' = orbitBody,
                    'Is_Potentially_Hazardous_Asteroid' = isHazard)
  
return (list(url = url, data = aesData))
  
}

data <- asteroidData('1994-10-10', '1994-12-10')$data
```

    ## [1] "[WARNING..!!] The difference between the date range should be less than 8 days for this current date range the API will return future Approach date "

``` r
# EDA on Asteroid data

asteroidData <- data

asteroidData
```

    ## # A tibble: 71 × 8
    ##    Magnitude Minimum_…¹ Maxim…² Relat…³ Appro…⁴ Miss_…⁵
    ##        <dbl>      <dbl>   <dbl> <chr>   <chr>   <chr>  
    ##  1      20.6     0.125   0.280  58294.… 2022-1… 0.4274…
    ##  2      22.1     0.0628  0.140  104578… 2022-1… 0.3748…
    ##  3      21.1     0.0995  0.223  85022.… 2022-1… 0.4388…
    ##  4      24.5     0.0208  0.0465 43495.… 2022-1… 0.0157…
    ##  5      21       0.104   0.233  67428.… 2022-1… 0.2604…
    ##  6      21.2     0.0950  0.213  7036.8… 2022-1… 0.2865…
    ##  7      24.2     0.0234  0.0524 68965.… 2022-1… 0.4814…
    ##  8      23       0.0415  0.0928 43343.… 2022-1… 0.1438…
    ##  9      25.2     0.0148  0.0331 31072.… 2022-1… 0.0714…
    ## 10      19.6     0.199   0.444  39910.… 2022-1… 0.2696…
    ## # … with 61 more rows, 2 more variables:
    ## #   Orbiting_Body <chr>,
    ## #   Is_Potentially_Hazardous_Asteroid <lgl>, and
    ## #   abbreviated variable names ¹​Minimum_Diameter,
    ## #   ²​Maximum_Diameter, ³​Relative_Velocity,
    ## #   ⁴​Approach_Date, ⁵​Miss_Distance

``` r
# First round up to two values 


plot1 <- ggplot(asteroidData, aes(x = Approach_Date, y = Miss_Distance))

plot1 + geom_point(aes(color = Is_Potentially_Hazardous_Asteroid, size = Maximum_Diameter), alpha = 0.7) +
  theme(axis.text.x = element_text(angle = 45), axis.text.y = element_blank())
```

![](README_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
# boxplot for min and max diamter
par(mfrow = c(1,2))
plot2 <- ggplot(asteroidData, aes(y = Minimum_Diameter))

plot2 + 
  geom_boxplot(aes(color=Is_Potentially_Hazardous_Asteroid))
```

![](README_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
plot3 <- ggplot(asteroidData, aes(y = Maximum_Diameter))

plot3 + 
  geom_boxplot(aes(color=Is_Potentially_Hazardous_Asteroid))
```

![](README_files/figure-gfm/unnamed-chunk-4-2.png)<!-- -->

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
  
  if (speed < 0){
    paste0("Warning: ", "The speed cannot be negative. Proceeding with its default value of 0.")
    speed = 0
  }
  
  if (halfAngle < 0){
    paste0("Warning: ", "The half angle cannot be negative. Proceeding with its default value of 0.")
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

``` r
cmeSampleData <- cmeData("2019-11-10", "2020-11-10")$data

summary(cmeSampleData %>% select(halfAngle, speed))
```

    ##    halfAngle         speed      
    ##  Min.   : 5.00   Min.   :120.0  
    ##  1st Qu.:12.00   1st Qu.:222.0  
    ##  Median :18.00   Median :277.0  
    ##  Mean   :18.12   Mean   :291.8  
    ##  3rd Qu.:24.00   3rd Qu.:348.0  
    ##  Max.   :33.00   Max.   :549.0

``` r
img <- readJPEG("C:\\Users\\sbgad\\Desktop\\EIhCLr.jpeg")

ggplot(cmeSampleData, aes(x=latitude, y=longitude)) +
    background_image(img) +
    geom_point(aes(color = as.factor(type), size = speed)) +
    #scale_color_discrete(name = "Type") +
    scale_color_manual(values = c("C" = "#37a0bf", "S" = "green")) +
    ylim(-150,150) +
    xlim(-30, 30)
```

![](README_files/figure-gfm/inital_plot-1.png)<!-- -->

``` r
cmeSampleData$type <- as.factor(cmeSampleData$type)

speedClassfication <- c("Slow paced", "Medium Paced", "Fast Paced", "Hyper Paced")

cmeSampleData <- cmeSampleData %>% 
  mutate(speedC = as.factor(if_else(speed < 200, speedClassfication[1],
                                            if_else(speed < 350,  speedClassfication[2],
                                                    if_else(speed < 500, speedClassfication[3], speedClassfication[4])))))


zones <- c("North-East", "North-West", "South-East", "South-West")

cmeSampleData <- cmeSampleData %>%
  mutate(zone = as.factor(if_else(latitude>=0 & longitude>=0, zones[1],
                        if_else(latitude<=0 & longitude>=0,zones[2],
                                if_else(latitude<=0 & longitude<=0, zones[4],
                                        if_else(latitude>=0 & longitude<=0, zones[3], "Error"))))))

cmeSampleData
```

    ## # A tibble: 97 × 8
    ##    time      latit…¹ longi…² halfA…³ speed type  speedC
    ##    <chr>       <dbl>   <dbl>   <dbl> <dbl> <fct> <fct> 
    ##  1 2019-11-…       9      33      12   383 S     Fast …
    ##  2 2019-11-…       7     -90       5   224 S     Mediu…
    ##  3 2019-12-…      -2     -91       8   259 S     Mediu…
    ##  4 2019-12-…      -1     -85      12   356 S     Fast …
    ##  5 2019-12-…      -4     -88      10   211 S     Mediu…
    ##  6 2019-12-…     -11     152      20   163 S     Slow …
    ##  7 2020-01-…      -2       9      19   227 S     Mediu…
    ##  8 2020-01-…      -5      12       6   205 S     Mediu…
    ##  9 2020-01-…       5    -115      17   193 S     Slow …
    ## 10 2020-01-…       2     -17      10   362 S     Fast …
    ## # … with 87 more rows, 1 more variable: zone <fct>,
    ## #   and abbreviated variable names ¹​latitude,
    ## #   ²​longitude, ³​halfAngle

``` r
cmeSampleData %>%
  group_by(zone, speedC, type) %>%
  summarize(avgSpeed = mean(speed), sdSpeed = sd(speed), avgHalfAngle = mean(halfAngle), sdHalfAngle = sd(halfAngle), count = n()) %>%
  arrange(zone, speedC)
```

    ## # A tibble: 12 × 8
    ## # Groups:   zone, speedC [12]
    ##    zone    speedC type  avgSp…¹ sdSpeed avgHa…² sdHal…³
    ##    <fct>   <fct>  <fct>   <dbl>   <dbl>   <dbl>   <dbl>
    ##  1 North-… Fast … S        368.   20.5     17.2    7.42
    ##  2 North-… Mediu… S        261.   41.4     17.6    6.94
    ##  3 North-… Slow … S        148.   21.7     16.6    8.30
    ##  4 North-… Fast … S        401    30.1     19.5    8.39
    ##  5 North-… Mediu… S        263.   36.8     16.4    6.93
    ##  6 North-… Slow … S        151.   23.0     20.8    4.71
    ##  7 South-… Fast … S        428    51.5     18.5    9.46
    ##  8 South-… Mediu… S        275.   46.7     17.3    6.57
    ##  9 South-… Slow … S        190.    3.65    24.8    6.61
    ## 10 South-… Fast … S        410.   39.9     18.2   10.2 
    ## 11 South-… Hyper… C        534.   19.6     21      8.83
    ## 12 South-… Mediu… S        263.   41.6     17.2    6.34
    ## # … with 1 more variable: count <int>, and abbreviated
    ## #   variable names ¹​avgSpeed, ²​avgHalfAngle,
    ## #   ³​sdHalfAngle

``` r
print(table(cmeSampleData$zone, cmeSampleData$speedC, cmeSampleData$type))
```

    ## , ,  = C
    ## 
    ##             
    ##              Fast Paced Hyper Paced Medium Paced
    ##   North-East          0           0            0
    ##   North-West          0           0            0
    ##   South-East          0           0            0
    ##   South-West          0           4            0
    ##             
    ##              Slow paced
    ##   North-East          0
    ##   North-West          0
    ##   South-East          0
    ##   South-West          0
    ## 
    ## , ,  = S
    ## 
    ##             
    ##              Fast Paced Hyper Paced Medium Paced
    ##   North-East          2           0           14
    ##   North-West          4           0           16
    ##   South-East          6           0           16
    ##   South-West          8           0           13
    ##             
    ##              Slow paced
    ##   North-East          4
    ##   North-West          5
    ##   South-East          5
    ##   South-West          0

``` r
img <- readJPEG("C:\\Users\\sbgad\\Desktop\\EIhCLr.jpeg")

ggplot(cmeSampleData, aes(x=latitude, y=longitude)) +
    background_image(img) +
    geom_point(aes(color = zone, size = speedC, shape = type)) +
    #scale_shape_discrete(name = "Type", labels = c("S", "C")) +
    #scale_color_manual(values = c("C" = "#37a0bf", "S" = "green")) +
    scale_size_discrete(name = "speed", labels = c("slow", "medium", "fast", "hyper")) + 
    ylim(-150,150) +
    xlim(-30, 30) +
  
    annotate(geom="text", x=20, y=100, label=paste0("North-East Region\ncount : ", nrow(cmeSampleData %>% filter(zone==zones[1])), "\navgSpeed : ", mean((cmeSampleData %>% filter(zone == zones[1]))$speed),
                                                  "\navgHalfAngle : ", mean((cmeSampleData %>% filter(zone == zones[1]))$halfAngle)),
              color="White", size=4) + 
  
  annotate(geom="text", x=-20, y=100, label=paste0("North-West Region\ncount : ", nrow(cmeSampleData %>% filter(zone==zones[2])), "\navgSpeed : ", mean((cmeSampleData %>% filter(zone == zones[2]))$speed),
                                                  "\navgHalfAngle : ", mean((cmeSampleData %>% filter(zone == zones[2]))$halfAngle)),
              color="White", size=4) + 
  
  annotate(geom="text", x=-20, y=-100, label=paste0("South-West Region\ncount : ", nrow(cmeSampleData %>% filter(zone==zones[4])), "\navgSpeed : ", mean((cmeSampleData %>% filter(zone == zones[4]))$speed),
                                                  "\navgHalfAngle : ", mean((cmeSampleData %>% filter(zone == zones[4]))$halfAngle)),
              color="White", size=4) + 
  
  annotate(geom="text", x=20, y=-100, label=paste0("South-East Region\ncount : ", nrow(cmeSampleData %>% filter(zone==zones[3])), "\navgSpeed : ", round(mean((cmeSampleData %>% filter(zone == zones[3]))$speed), 2),
                                                  "\navgHalfAngle : ", round(mean((cmeSampleData %>% filter(zone == zones[3]))$halfAngle), 2)),
              color="White", size=4) +
  labs(title="Plot showing zone-wise Corona Mass Ejection statistics",
        x ="Latitude", y = "Longitude")
```

![](README_files/figure-gfm/cme_con_tbls-1.png)<!-- -->

``` r
apiSelection <- function(api, ...){
  
  if (tolower(api) == tolower("Coronal Mass Ejection (CME) Analysis")){
    cmeData(...)
    
  }
  else if(tolower(api) == tolower("Asteroids - NeoWs")){
    asteroidData(...)
    
  }
  else{
    return("This api is not yet supported. Please select from either 'Coronal Mass Ejection (CME) Analysis' or 'Asteroid (AST)'.")
  }
}
```
