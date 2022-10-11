ST558 Project 2
================
Shyam Gadhwala & Kamlesh Pandey

-   <a href="#requirement" id="toc-requirement">Requirement</a>
-   <a href="#asteroid---neows-api" id="toc-asteroid---neows-api">Asteroid -
    NeoWs API</a>
-   <a href="#shyam" id="toc-shyam">Shyam</a>

This vignette is based on the NASA API. The primary purpose of this
vignette is to download data from the API and explore visualization
package ggplot for exploratory data analysis purpose.

[NASA API](https://api.nasa.gov/index.html)

# Requirement

For this project following packages are used.

[httr](https://httr.r-lib.org/) to provide a wrapper function and
customized to the demand of modern web APIs.

[jsonline](https://cran.r-project.org/web/packages/jsonlite/vignettes/json-aaquickstart.html)
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

astDf <- apiSelection(api = "Asteroids - NeoWs", '1994-10-10', '1994-12-10')$data
```

    ## [1] "[WARNING..!!] The difference between the date range should be less than 8 days for this current date range the API will return future Approach date "

``` r
# EDA on Asteroid data

plot1 <- ggplot(astDf, aes(x = Approach_Date, y = Miss_Distance))

plot1 + geom_point(aes(color = Is_Potentially_Hazardous_Asteroid, size = Maximum_Diameter), alpha = 0.7) +
  theme(axis.text.x = element_text(angle = 45), axis.text.y = element_blank())+
  theme(axis.text.y = element_blank(), 
        axis.ticks.y = element_blank()
        )
```

![](README_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

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

![](README_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
library(ggplot2)

# correlation plot

ggpairs(astDf, color = 'red', main = 'Scatter Plot')
```

![](README_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

# Shyam

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
cmeSampleData <- cmeData("2017-01-01", "2020-01-01")$data

#cmeSampleData <- cmeData("2019-11-10", "2020-11-10")$data

summary(cmeSampleData %>% select(halfAngle, speed, latitude, longitude))
```

    ##    halfAngle         speed           latitude      
    ##  Min.   : 2.00   Min.   :  88.0   Min.   :-55.000  
    ##  1st Qu.:16.00   1st Qu.: 281.5   1st Qu.: -7.000  
    ##  Median :22.00   Median : 359.0   Median :  0.000  
    ##  Mean   :22.27   Mean   : 413.9   Mean   :  2.506  
    ##  3rd Qu.:27.00   3rd Qu.: 459.0   3rd Qu.: 11.000  
    ##  Max.   :54.00   Max.   :2650.0   Max.   : 90.000  
    ##    longitude     
    ##  Min.   :-178.0  
    ##  1st Qu.: -88.0  
    ##  Median :   1.5  
    ##  Mean   :   2.5  
    ##  3rd Qu.:  92.0  
    ##  Max.   : 178.0

``` r
img <- readJPEG("img\\sun2.jpeg")

cmeSampleData$type <- as.factor(cmeSampleData$type)

show(ggplot(cmeSampleData, aes(x=latitude, y=longitude)) +
    geom_point(aes(color = type, size = speed)) +
    ylim(-150,150) +
    xlim(-90, 90)  +
  labs(title="Plot showing Corona Mass Ejection on Sun's corona",
        x ="Latitude", y = "Longitude"))
```

![](README_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
img <- readJPEG("img\\sun2.jpeg")

ggplot(cmeSampleData, aes(x=latitude, y=longitude)) +
    background_image(img) +
    geom_point(aes(color = type, size = speed)) +
    ylim(-180,180) +
    xlim(-90, 90)  +
  labs(title="Plot showing Corona Mass Ejection on Sun's corona",
        x ="Latitude", y = "Longitude")
```

![](README_files/figure-gfm/inital_plot-1.png)<!-- -->

``` r
#cmeSampleData$type <- as.factor(cmeSampleData$type)

cmeSampleData$type <- as.factor(cmeSampleData$type)

speedClassfication <- c("Slow paced", "Medium Paced", "Fast Paced", "Hyper Paced")

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

    ## # A tibble: 324 × 9
    ##    time       latit…¹ longi…² halfA…³ speed type  speedC
    ##    <chr>        <dbl>   <dbl>   <dbl> <dbl> <fct> <fct> 
    ##  1 2017-01-0…     -10    -105      20   645 C     Mediu…
    ##  2 2017-01-0…     -33     -93      26    88 S     Slow …
    ##  3 2017-01-1…       3     100      22   167 S     Slow …
    ##  4 2017-01-1…       1     -90      36   580 C     Mediu…
    ##  5 2017-01-1…      10    -155      10   628 C     Mediu…
    ##  6 2017-01-1…      11      90      15   365 S     Slow …
    ##  7 2017-01-1…       0     -48      14   353 S     Slow …
    ##  8 2017-01-1…      11     160      11   723 C     Mediu…
    ##  9 2017-01-1…      16     -80      21   429 S     Slow …
    ## 10 2017-01-2…      21     105      26   276 S     Slow …
    ## # … with 314 more rows, 2 more variables: zone <fct>,
    ## #   halfAngleC <fct>, and abbreviated variable names
    ## #   ¹​latitude, ²​longitude, ³​halfAngle

``` r
cmeSampleData %>%
  group_by(zone, speedC, type) %>%
  summarize(avgSpeed = mean(speed), sdSpeed = sd(speed),
            avgHalfAngle = mean(halfAngle), 
            sdHalfAngle = sd(halfAngle), count = n()) %>% arrange(zone, speedC)
```

    ## # A tibble: 11 × 8
    ## # Groups:   zone, speedC [11]
    ##    zone     speedC type  avgSp…¹ sdSpeed avgHa…² sdHal…³
    ##    <fct>    <fct>  <fct>   <dbl>   <dbl>   <dbl>   <dbl>
    ##  1 North-E… Mediu… C        627.   100.     22.3   10.6 
    ##  2 North-E… Slow … S        325.    88.3    22.1    8.03
    ##  3 North-W… Fast … O       1177.    62.0    40      8.72
    ##  4 North-W… Hyper… R       2460.   268.     50      5.66
    ##  5 North-W… Mediu… C        620.    82.6    29.2   11.5 
    ##  6 North-W… Slow … S        326.    95.1    20.9    7.01
    ##  7 South-E… Mediu… C        634.   126.     26.1   11.2 
    ##  8 South-E… Slow … S        313.    90.5    20.8    8.45
    ##  9 South-W… Fast … O       1437     32.0    41.3    8.02
    ## 10 South-W… Mediu… C        691.   173.     21.5    5.94
    ## 11 South-W… Slow … S        328.    81.7    20.5    7.66
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
    ##   North-East          0           0           19
    ##   North-West          0           0           10
    ##   South-East          0           0           18
    ##   South-West          0           0           13
    ##             
    ##              Slow paced
    ##   North-East          0
    ##   North-West          0
    ##   South-East          0
    ##   South-West          0
    ## 
    ## , ,  = O
    ## 
    ##             
    ##              Fast Paced Hyper Paced Medium Paced
    ##   North-East          0           0            0
    ##   North-West          3           0            0
    ##   South-East          0           0            0
    ##   South-West          3           0            0
    ##             
    ##              Slow paced
    ##   North-East          0
    ##   North-West          0
    ##   South-East          0
    ##   South-West          0
    ## 
    ## , ,  = R
    ## 
    ##             
    ##              Fast Paced Hyper Paced Medium Paced
    ##   North-East          0           0            0
    ##   North-West          0           2            0
    ##   South-East          0           0            0
    ##   South-West          0           0            0
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
    ##   North-East          0           0            0
    ##   North-West          0           0            0
    ##   South-East          0           0            0
    ##   South-West          0           0            0
    ##             
    ##              Slow paced
    ##   North-East         75
    ##   North-West         56
    ##   South-East         54
    ##   South-West         71

``` r
img <- readJPEG("img\\sun2.jpeg")

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

``` r
cor <- cor(cmeSampleData$halfAngle, cmeSampleData$speed)
  
ggplot(cmeSampleData, aes(x=halfAngle, y=speed)) +
    geom_point(aes(color = type)) + 
    geom_smooth(method = "lm")
```

![](README_files/figure-gfm/cor-1.png)<!-- -->

``` r
ggplot(cmeSampleData, aes(x = type)) + 
  geom_bar(aes(fill = halfAngleC), position = "dodge") + 
  scale_fill_discrete(name = "HalfAngle") + 
  facet_grid(. ~ zone)
```

![](README_files/figure-gfm/barplot-1.png)<!-- -->

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
  geom_histogram(aes(fill=type), stat="count")
```

    ## Warning: Ignoring unknown parameters: binwidth, bins,
    ## pad

![](README_files/figure-gfm/hist-1.png)<!-- -->

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
