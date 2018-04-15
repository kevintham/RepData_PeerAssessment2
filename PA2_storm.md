---
title: 'Reproducible Research: Peer Assessment 2'
author: "Kevin Tham"
date: "April 12, 2018"
output:
  html_document:
    keep_md: true
---



## Introduction

Storms and other severe weather events can cause both public health and economic problems for communities and municipalities. Many severe events can result in fatalities, injuries, and property damage, and preventing such outcomes to the extent possible is a key concern.

This project involves exploring the U.S. National Oceanic and Atmospheric Administration's (NOAA) storm database. This database tracks characteristics of major storms and weather events in the United States, including when and where they occur, as well as estimates of any fatalities, injuries, and property damage.

## Synopsis

We address the following questions through analysis of the NOAA Storm Database:

1. Across the United States, which types of events (as indicated in the EVTYPE variable) are most harmful with respect to population health?
2. Across the United States, which types of events have the greatest economic consequences?



## Data Processing

First, we load the required packages and install them first if they are not available:


```r
if (!require("pacman")) install.packages("pacman")
```

```
## Loading required package: pacman
```

```r
pacman::p_load(knitr, dplyr, ggplot2, tidyr, hexbin, timeDate)
```

Next we download and unzip the data file to read in the dataset:


```r
destfile <- 'repdata%2Fdata%2FStormData.csv.bz2'
url <- 'https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2'

if (!file.exists(destfile))
  download.file(url, destfile, method = "auto")

df <- read.csv(destfile)
```

We will explore the dataset using the `head()` and `str()` functions:


```r
head(df)
```

```
##   STATE__           BGN_DATE BGN_TIME TIME_ZONE COUNTY COUNTYNAME STATE
## 1       1  4/18/1950 0:00:00     0130       CST     97     MOBILE    AL
## 2       1  4/18/1950 0:00:00     0145       CST      3    BALDWIN    AL
## 3       1  2/20/1951 0:00:00     1600       CST     57    FAYETTE    AL
## 4       1   6/8/1951 0:00:00     0900       CST     89    MADISON    AL
## 5       1 11/15/1951 0:00:00     1500       CST     43    CULLMAN    AL
## 6       1 11/15/1951 0:00:00     2000       CST     77 LAUDERDALE    AL
##    EVTYPE BGN_RANGE BGN_AZI BGN_LOCATI END_DATE END_TIME COUNTY_END
## 1 TORNADO         0                                               0
## 2 TORNADO         0                                               0
## 3 TORNADO         0                                               0
## 4 TORNADO         0                                               0
## 5 TORNADO         0                                               0
## 6 TORNADO         0                                               0
##   COUNTYENDN END_RANGE END_AZI END_LOCATI LENGTH WIDTH F MAG FATALITIES
## 1         NA         0                      14.0   100 3   0          0
## 2         NA         0                       2.0   150 2   0          0
## 3         NA         0                       0.1   123 2   0          0
## 4         NA         0                       0.0   100 2   0          0
## 5         NA         0                       0.0   150 2   0          0
## 6         NA         0                       1.5   177 2   0          0
##   INJURIES PROPDMG PROPDMGEXP CROPDMG CROPDMGEXP WFO STATEOFFIC ZONENAMES
## 1       15    25.0          K       0                                    
## 2        0     2.5          K       0                                    
## 3        2    25.0          K       0                                    
## 4        2     2.5          K       0                                    
## 5        2     2.5          K       0                                    
## 6        6     2.5          K       0                                    
##   LATITUDE LONGITUDE LATITUDE_E LONGITUDE_ REMARKS REFNUM
## 1     3040      8812       3051       8806              1
## 2     3042      8755          0          0              2
## 3     3340      8742          0          0              3
## 4     3458      8626          0          0              4
## 5     3412      8642          0          0              5
## 6     3450      8748          0          0              6
```

```r
str(df)
```

```
## 'data.frame':	902297 obs. of  37 variables:
##  $ STATE__   : num  1 1 1 1 1 1 1 1 1 1 ...
##  $ BGN_DATE  : Factor w/ 16335 levels "1/1/1966 0:00:00",..: 6523 6523 4242 11116 2224 2224 2260 383 3980 3980 ...
##  $ BGN_TIME  : Factor w/ 3608 levels "00:00:00 AM",..: 272 287 2705 1683 2584 3186 242 1683 3186 3186 ...
##  $ TIME_ZONE : Factor w/ 22 levels "ADT","AKS","AST",..: 7 7 7 7 7 7 7 7 7 7 ...
##  $ COUNTY    : num  97 3 57 89 43 77 9 123 125 57 ...
##  $ COUNTYNAME: Factor w/ 29601 levels "","5NM E OF MACKINAC BRIDGE TO PRESQUE ISLE LT MI",..: 13513 1873 4598 10592 4372 10094 1973 23873 24418 4598 ...
##  $ STATE     : Factor w/ 72 levels "AK","AL","AM",..: 2 2 2 2 2 2 2 2 2 2 ...
##  $ EVTYPE    : Factor w/ 985 levels "   HIGH SURF ADVISORY",..: 834 834 834 834 834 834 834 834 834 834 ...
##  $ BGN_RANGE : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ BGN_AZI   : Factor w/ 35 levels "","  N"," NW",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ BGN_LOCATI: Factor w/ 54429 levels ""," Christiansburg",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ END_DATE  : Factor w/ 6663 levels "","1/1/1993 0:00:00",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ END_TIME  : Factor w/ 3647 levels ""," 0900CST",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ COUNTY_END: num  0 0 0 0 0 0 0 0 0 0 ...
##  $ COUNTYENDN: logi  NA NA NA NA NA NA ...
##  $ END_RANGE : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ END_AZI   : Factor w/ 24 levels "","E","ENE","ESE",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ END_LOCATI: Factor w/ 34506 levels ""," CANTON"," TULIA",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ LENGTH    : num  14 2 0.1 0 0 1.5 1.5 0 3.3 2.3 ...
##  $ WIDTH     : num  100 150 123 100 150 177 33 33 100 100 ...
##  $ F         : int  3 2 2 2 2 2 2 1 3 3 ...
##  $ MAG       : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ FATALITIES: num  0 0 0 0 0 0 0 0 1 0 ...
##  $ INJURIES  : num  15 0 2 2 2 6 1 0 14 0 ...
##  $ PROPDMG   : num  25 2.5 25 2.5 2.5 2.5 2.5 2.5 25 25 ...
##  $ PROPDMGEXP: Factor w/ 19 levels "","-","?","+",..: 17 17 17 17 17 17 17 17 17 17 ...
##  $ CROPDMG   : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ CROPDMGEXP: Factor w/ 9 levels "","?","0","2",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ WFO       : Factor w/ 542 levels ""," CI","%SD",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ STATEOFFIC: Factor w/ 250 levels "","ALABAMA, Central",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ ZONENAMES : Factor w/ 25112 levels "","                                                                                                               "| __truncated__,..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ LATITUDE  : num  3040 3042 3340 3458 3412 ...
##  $ LONGITUDE : num  8812 8755 8742 8626 8642 ...
##  $ LATITUDE_E: num  3051 0 0 0 0 ...
##  $ LONGITUDE_: num  8806 0 0 0 0 ...
##  $ REMARKS   : Factor w/ 436781 levels "","\t","\t\t",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ REFNUM    : num  1 2 3 4 5 6 7 8 9 10 ...
```

There are many variables present in this dataset, not all of which will be relevant to our analysis. We recap our goal:

1. To find out which **EVTYPE**(-s) is most harmful to **population health** across the US
2. To find out which **EVTYPE**(-s) have the greatest **economic consequences**

Therefore, we will narrow down the number of variables to those that are related to **EVTYPE**, **population health** and **economic consequences**.

Population health: 

* `FATALITIES`
* `INJURIES` 

Economic consequences: 

* `PROPDMG` 
* `PROPDMGEXP` 
* `CROPDMG` 
* `CROPDMGEXP` 

We will select these variables from the original dataset in addition to `EVTYPE` for obvious reasons, and `STATE__` in order to analyse the spatial variation of population health and economic consequences:


```r
df_filtered <- select(df, EVTYPE, STATE__, FATALITIES, INJURIES, 
                      PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP)
```

The proportion of missing values in the dataframe is first determined for the relevant variables:


```r
mean(is.na(df_filtered))
```

```
## [1] 0
```

Surprisingly there is a complete absence of missing variables.

Next we examine the variable `EVTYPE`:


```r
unique(df_filtered$EVTYPE)
```

It is clear that there are many duplicates, errors and variants present in the variable `EVTYPE`. Therefore we will proceed by cleaning up this variable, using regular expressions to substitute variable names with the correct variable.


```r
event <- df_filtered$EVTYPE
event <- gsub('.*TSTM.*', 'THUNDERSTORM WIND', event, ignore.case=TRUE)
event <- gsub('.*THUNDER.*', 'THUNDERSTORM WIND', event, ignore.case=TRUE)
event <- gsub('.*THUND.*', 'THUNDERSTORM WIND', event, ignore.case=TRUE)
event <- gsub('.*micro.*', 'THUNDERSTORM WIND', event, ignore.case=TRUE)
event <- gsub('.*SPOUT.*', 'WATERSPOUT', event, ignore.case=TRUE)
event <- gsub('.*TORNADO.*', 'TORNADO', event, ignore.case=TRUE)
event <- gsub('^HIGH WIND.*', 'HIGH WIND', event, ignore.case=TRUE)
event <- gsub('.*AVALANCHE.*', 'AVALANCHE', event, ignore.case=TRUE)
event <- gsub('.*BLIZZARD.*', 'BLIZZARD', event, ignore.case=TRUE)
event <- gsub('.*SNOW.*', 'HEAVY SNOW', event, ignore.case=TRUE)
event <- gsub('.*HURRICANE.*', 'HURRICANE/TYPHOON', event, ignore.case=TRUE)
event <- gsub('.*TYPHOON.*', 'HURRICANE/TYPHOON', event, ignore.case=TRUE)
event <- gsub('.*HEAT.*', 'HEAT', event, ignore.case=TRUE)
event <- gsub('.*WARM.*', 'HEAT', event, ignore.case=TRUE)
event <- gsub('.*HIGH TEMP.*', 'HEAT', event, ignore.case=TRUE)
event <- gsub('.*COLD.*', 'EXTREME COLD/WIND CHILL', event, ignore.case=TRUE)
event <- gsub('.*HAIL.*', 'HAIL', event, ignore.case=TRUE)
event <- gsub('.*flash.flood.*', 'FLASH FLOOD', event, ignore.case=TRUE)
event <- gsub('.*flash.*', 'FLASH FLOOD', event, ignore.case=TRUE)
event <- gsub('.*LIGHTNING.*', 'LIGHTNING', event, ignore.case=TRUE)
event <- gsub('(?<!FLASH )FLOOD.*', 'FLOOD', event, ignore.case=TRUE, perl=TRUE)
event <- gsub('.*FLOOD.*', 'FLOOD', event, ignore.case=TRUE)
event <- gsub('.*FUNNEL.*', 'FUNNEL CLOUD', event, ignore.case=TRUE)
event <- gsub('.*FIRE.*', 'WILDFIRE', event, ignore.case=TRUE)
event <- gsub('.*TROPICAL STORM.*', 'TROPICAL STORM', event, ignore.case=TRUE)
event <- gsub('.*CURRENT.*', 'RIP CURRENT', event, ignore.case=TRUE)
event <- gsub('.*DRY.*', 'DROUGHT', event, ignore.case=TRUE)
event <- gsub('(?<!FREEZING )RAIN.*', 'HEAVY RAIN', event, ignore.case=TRUE, perl=TRUE)
event <- gsub('.*RAIN.*', 'HEAVY RAIN', event, ignore.case=TRUE)

table(grep('(?<!freezing )rain',event,ignore.case=TRUE,value=TRUE, perl=TRUE))
```

```
## 
## HEAVY RAIN 
##      12161
```

```r
unique(event)
```

```
##   [1] "TORNADO"                        "THUNDERSTORM WIND"             
##   [3] "HAIL"                           "HEAVY RAIN"                    
##   [5] "HEAVY SNOW"                     "FLOOD"                         
##   [7] "WINTER STORM"                   "HURRICANE/TYPHOON"             
##   [9] "EXTREME COLD/WIND CHILL"        "LIGHTNING"                     
##  [11] "DENSE FOG"                      "RIP CURRENT"                   
##  [13] "HIGH WIND"                      "FUNNEL CLOUD"                  
##  [15] "HEAT"                           "WIND"                          
##  [17] "LIGHTING"                       "WALL CLOUD"                    
##  [19] "WATERSPOUT"                     "BLIZZARD"                      
##  [21] "WIND CHILL"                     "FREEZE"                        
##  [23] "HIGH TIDES"                     "RECORD HIGH"                   
##  [25] "ICE STORM"                      "RECORD LOW"                    
##  [27] "LOW TEMPERATURE RECORD"         "AVALANCHE"                     
##  [29] "MARINE MISHAP"                  "WIND CHILL/HIGH WIND"          
##  [31] "HIGH SEAS"                      "SEVERE TURBULENCE"             
##  [33] "WIND DAMAGE"                    "DUST STORM"                    
##  [35] "APACHE COUNTY"                  "SLEET"                         
##  [37] "DUST DEVIL"                     "WINTER STORM/HIGH WIND"        
##  [39] "WINTER STORM/HIGH WINDS"        "GUSTY WINDS"                   
##  [41] "STRONG WINDS"                   "HEAVY SURF"                    
##  [43] "HEAVY PRECIPATATION"            "HIGH SURF"                     
##  [45] "BLOWING DUST"                   "URBAN/SMALL"                   
##  [47] "WILDFIRE"                       "HIGH"                          
##  [49] "WINTER STORM HIGH WINDS"        "WINTER STORMS"                 
##  [51] "MUDSLIDES"                      "WINDS"                         
##  [53] "STRONG WIND"                    "URBAN AND SMALL"               
##  [55] "ICE"                            "DOWNBURST"                     
##  [57] "GUSTNADO AND"                   "DOWNBURST WINDS"               
##  [59] "DROUGHT"                        "FREEZING DRIZZLE"              
##  [61] "GLAZE"                          "UNSEASONABLY WET"              
##  [63] "WINTRY MIX"                     "WINTER WEATHER"                
##  [65] "NORMAL PRECIPITATION"           "STORM SURGE"                   
##  [67] "TROPICAL STORM"                 "URBAN AND SMALL STREAM"        
##  [69] "DAMAGING FREEZE"                "MUD SLIDE"                     
##  [71] "LIGNTNING"                      "FROST"                         
##  [73] "EXTREME WIND CHILLS"            "COOL AND WET"                  
##  [75] "GLAZE ICE"                      "HIGH  WINDS"                   
##  [77] "SMALL STREAM AND"               "MUD SLIDES"                    
##  [79] "EXTREME WIND CHILL"             "EXCESSIVE WETNESS"             
##  [81] "GRADIENT WINDS"                 "SLEET/ICE STORM"               
##  [83] "ROTATING WALL CLOUD"            "LARGE WALL CLOUD"              
##  [85] "GUSTNADO"                       "FOG"                           
##  [87] "WIND STORM"                     "AGRICULTURAL FREEZE"           
##  [89] "TUNDERSTORM WIND"               "COASTAL SURGE"                 
##  [91] "ICE FLOES"                      "HIGH WAVES"                    
##  [93] "LOW WIND CHILL"                 "URBAN/SMALL STREAM"            
##  [95] "TORNDAO"                        "GLAZE/ICE STORM"               
##  [97] "AVALANCE"                       "DUST STORM/HIGH WINDS"         
##  [99] "ICE JAM"                        "FROST\\FREEZE"                 
## [101] "HARD FREEZE"                    "BELOW NORMAL PRECIPITATION"    
## [103] "RECORD TEMPERATURES"            "OTHER"                         
## [105] "MUDSLIDE"                       "ICY ROADS"                     
## [107] "HEAVY MIX"                      "DAM FAILURE"                   
## [109] "THUDERSTORM WINDS"              "STORM FORCE WINDS"             
## [111] "SOUTHEAST"                      "FREEZING DRIZZLE AND FREEZING" 
## [113] "HEAVY PRECIPITATION"            "HIGH WATER"                    
## [115] "WET WEATHER"                    "BEACH EROSIN"                  
## [117] "LOW TEMPERATURE"                "HYPOTHERMIA"                   
## [119] "THUNERSTORM WINDS"              "MUD/ROCK SLIDE"                
## [121] "RAPIDLY RISING WATER"           "EARLY FREEZE"                  
## [123] "ICE/STRONG WINDS"               "EXTREME WIND CHILL/BLOWING SNO"
## [125] "EARLY FROST"                    "LANDSLIDE"                     
## [127] "EXCESSIVE"                      "HEAVY SEAS"                    
## [129] "DUSTSTORM"                      "?"                             
## [131] "HOT PATTERN"                    "WINTER MIX"                    
## [133] "EXCESSIVE PRECIPITATION"        "MILD PATTERN"                  
## [135] "LANDSLIDES"                     "HEAVY SHOWERS"                 
## [137] "SAHARAN DUST"                   "HEAVY SHOWER"                  
## [139] "HEAVY SWELLS"                   "URBAN SMALL"                   
## [141] "SMALL STREAM"                   "EXTREME WINDCHILL"             
## [143] "URBAN/SML STREAM FLD"           "Other"                         
## [145] "Temperature record"             "ROUGH SURF"                    
## [147] "Wind"                           "Heavy Surf"                    
## [149] "Dust Devil"                     "Wind Damage"                   
## [151] "Marine Accident"                "Freeze"                        
## [153] "Strong Wind"                    "COASTAL STORM"                 
## [155] "Wet Month"                      "Wet Year"                      
## [157] "Damaging Freeze"                "Beach Erosion"                 
## [159] "Icy Roads"                      "High Surf"                     
## [161] "Early Frost"                    "Wintry Mix"                    
## [163] "Ice Fog"                        "Landslump"                     
## [165] "Coastal Storm"                  "Winter Weather"                
## [167] "Strong Winds"                   "Strong winds"                  
## [169] "Mudslide"                       "Glaze"                         
## [171] "Freezing Fog"                   "Whirlwind"                     
## [173] "Heavy Precipitation"            "Record temperature"            
## [175] "Gusty Wind"                     "MIXED PRECIP"                  
## [177] "Black Ice"                      "Mudslides"                     
## [179] "Gradient wind"                  "Freezing Spray"                
## [181] "Summary Jan 17"                 "Summary of March 14"           
## [183] "Summary of March 23"            "Summary of March 24"           
## [185] "Summary of April 3rd"           "Summary of April 12"           
## [187] "Summary of April 13"            "Summary of April 21"           
## [189] "Summary August 11"              "Summary of April 27"           
## [191] "Summary of May 9-10"            "Summary of May 10"             
## [193] "Summary of May 13"              "Summary of May 14"             
## [195] "Summary of May 22 am"           "Summary of May 22 pm"          
## [197] "Summary of May 26 am"           "Summary of May 26 pm"          
## [199] "Metro Storm, May 26"            "Summary of May 31 am"          
## [201] "Summary of May 31 pm"           "Summary of June 3"             
## [203] "Summary of June 4"              "Summary June 5-6"              
## [205] "Summary June 6"                 "Summary of June 11"            
## [207] "Summary of June 12"             "Summary of June 13"            
## [209] "Summary of June 15"             "Summary of June 16"            
## [211] "Summary June 18-19"             "Summary of June 23"            
## [213] "Summary of June 24"             "Summary of June 30"            
## [215] "Summary of July 2"              "Summary of July 3"             
## [217] "Summary of July 11"             "Summary of July 22"            
## [219] "Summary July 23-24"             "Summary of July 26"            
## [221] "Summary of July 29"             "Summary of August 1"           
## [223] "Summary August 2-3"             "Summary August 7"              
## [225] "Summary August 9"               "Summary August 10"             
## [227] "Summary August 17"              "Summary August 21"             
## [229] "Summary August 28"              "Summary September 4"           
## [231] "Summary September 20"           "Summary September 23"          
## [233] "Summary Sept. 25-26"            "Summary: Oct. 20-21"           
## [235] "Summary: October 31"            "Summary: Nov. 6-7"             
## [237] "Summary: Nov. 16"               "wet micoburst"                 
## [239] "No Severe Weather"              "Summary of May 22"             
## [241] "Summary of June 6"              "Summary August 4"              
## [243] "Summary of June 10"             "Summary of June 18"            
## [245] "Summary September 3"            "Summary: Sept. 18"             
## [247] "Record Temperatures"            "Freezing Drizzle"              
## [249] "Sml Stream Fld"                 "MUDSLIDE/LANDSLIDE"            
## [251] "Saharan Dust"                   "Volcanic Ash"                  
## [253] "Volcanic Ash Plume"             "NONE"                          
## [255] "DAM BREAK"                      "BLACK ICE"                     
## [257] "BLOW-OUT TIDES"                 "UNSEASONABLY COOL"             
## [259] "Gusty Winds"                    "GUSTY WIND"                    
## [261] "Wintry mix"                     "Frost"                         
## [263] "Frost/Freeze"                   "URBAN/SML STREAM FLDG"         
## [265] "STRONG WIND GUST"               "LATE FREEZE"                   
## [267] "BLOW-OUT TIDE"                  "Hypothermia/Exposure"          
## [269] "HYPOTHERMIA/EXPOSURE"           "Mixed Precipitation"           
## [271] "Record High"                    "COASTALSTORM"                  
## [273] "Gusty winds"                    "SUMMARY OF MARCH 24-25"        
## [275] "SUMMARY OF MARCH 27"            "SUMMARY OF MARCH 29"           
## [277] "GRADIENT WIND"                  "gradient wind"                 
## [279] "Freezing drizzle"               "URBAN/SMALL STRM FLDG"         
## [281] "Heavy surf and wind"            "HIGH SWELLS"                   
## [283] "HIGH  SWELLS"                   "VOLCANIC ASH"                  
## [285] "BEACH EROSION"                  "WINTERY MIX"                   
## [287] "HOT SPELL"                      "UNSEASONABLY HOT"              
## [289] "WAKE LOW WIND"                  "COASTAL EROSION"               
## [291] "BITTER WIND CHILL"              "BITTER WIND CHILL TEMPERATURES"
## [293] "SEICHE"                         "HYPERTHERMIA/EXPOSURE"         
## [295] "ROCK SLIDE"                     "ICE PELLETS"                   
## [297] "PATCHY DENSE FOG"               "RECORD COOL"                   
## [299] "HOT WEATHER"                    "RECORD TEMPERATURE"            
## [301] "TROPICAL DEPRESSION"            "VOLCANIC ERUPTION"             
## [303] "COOL SPELL"                     "WIND ADVISORY"                 
## [305] "FIRST FROST"                    "VOG"                           
## [307] "MONTHLY PRECIPITATION"          "MONTHLY TEMPERATURE"           
## [309] "EXTREME WINDCHILL TEMPERATURES" "MIXED PRECIPITATION"           
## [311] "REMNANTS OF FLOYD"              "FREEZING FOG"                  
## [313] "DRIEST MONTH"                   "WIND AND WAVE"                 
## [315] " WIND"                          "RECORD PRECIPITATION"          
## [317] "ICE ROADS"                      "ROUGH SEAS"                    
## [319] "UNSEASONABLY COOL & WET"        "NON-SEVERE WIND DAMAGE"        
## [321] "LANDSLUMP"                      "WIND GUSTS"                    
## [323] "UNSEASONAL LOW TEMP"            "HIGH SURF ADVISORY"            
## [325] "GUSTY LAKE WIND"                "WINTER WEATHER MIX"            
## [327] "RED FLAG CRITERIA"              "WND"                           
## [329] "SMOKE"                          "EXTREMELY WET"                 
## [331] "ROGUE WAVE"                     "DUST DEVEL"                    
## [333] "PATCHY ICE"                     "NORTHERN LIGHTS"               
## [335] "   HIGH SURF ADVISORY"          "HAZARDOUS SURF"                
## [337] "FROST/FREEZE"                   "WINTER WEATHER/MIX"            
## [339] "ASTRONOMICAL HIGH TIDE"         "WHIRLWIND"                     
## [341] "ABNORMALLY WET"                 "ICE ON ROAD"                   
## [343] "DROWNING"                       "HIGH SURF ADVISORIES"          
## [345] "HEAVY SURF/HIGH SURF"           "SLEET STORM"                   
## [347] "STORM SURGE/TIDE"               "MARINE HIGH WIND"              
## [349] "TSUNAMI"                        "DENSE SMOKE"                   
## [351] "MARINE STRONG WIND"             "ASTRONOMICAL LOW TIDE"         
## [353] "VOLCANIC ASHFALL"
```

## Results

## Conclusion
