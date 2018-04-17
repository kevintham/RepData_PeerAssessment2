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
pacman::p_load(knitr, dplyr, ggplot2, tidyr, hexbin, timeDate, grid, gridExtra, gtable)
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
event <- gsub('.*HOT.*', 'HEAT', event, ignore.case=TRUE)
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
event <- gsub('.*dust.*', 'DUST STORM', event, ignore.case=TRUE)
#table(grep('tornado',event,ignore.case=TRUE,value=TRUE, perl=TRUE))
#unique(event)

df_filtered$EVTYPE <- event
```

Having completed the cleaning of the data, we will split the dataset into two parts, one to determine the effect of these weather events on human health and the other two determine their effect on the economy.


```r
health <- df_filtered %>% 
  select(EVTYPE, FATALITIES, INJURIES) %>%
  group_by(EVTYPE) %>% 
  summarise_all(funs(sum))

fatalities <- health %>%
  select(EVTYPE, FATALITIES) %>%
  arrange(desc(FATALITIES)) %>%
  top_n(10)
```

```
## Selecting by FATALITIES
```

```r
injuries <- health %>%
  select(EVTYPE, INJURIES) %>%
  arrange(desc(INJURIES)) %>%
  top_n(10)
```

```
## Selecting by INJURIES
```

```r
fatalities
```

```
## # A tibble: 10 x 2
##    EVTYPE                  FATALITIES
##    <chr>                        <dbl>
##  1 TORNADO                       5633
##  2 HEAT                          3178
##  3 FLOOD                         1525
##  4 LIGHTNING                      817
##  5 THUNDERSTORM WIND              759
##  6 RIP CURRENT                    577
##  7 EXTREME COLD/WIND CHILL        436
##  8 HIGH WIND                      293
##  9 AVALANCHE                      224
## 10 WINTER STORM                   206
```

```r
injuries
```

```
## # A tibble: 10 x 2
##    EVTYPE            INJURIES
##    <chr>                <dbl>
##  1 TORNADO              91364
##  2 THUNDERSTORM WIND     9573
##  3 HEAT                  9243
##  4 FLOOD                 8604
##  5 LIGHTNING             5231
##  6 ICE STORM             1975
##  7 WILDFIRE              1608
##  8 HIGH WIND             1471
##  9 HAIL                  1371
## 10 HURRICANE/TYPHOON     1333
```

Next we can examine the variables `PROPDMGEXP` and `CROPDMGEXP`:


```r
unique(df_filtered$PROPDMGEXP)
```

```
##  [1] K M   B m + 0 5 6 ? 4 2 3 h 7 H - 1 8
## Levels:  - ? + 0 1 2 3 4 5 6 7 8 B h H K m M
```

```r
unique(df_filtered$CROPDMGEXP)
```

```
## [1]   M K m B ? 0 k 2
## Levels:  ? 0 2 B k K m M
```

## Results

We select the top 10 event types that contribute the most harm to population health.


```r
h1 <- ggplot(fatalities, aes(x=reorder(EVTYPE, FATALITIES), y=FATALITIES)) +
  geom_bar(stat="identity") + theme(aspect.ratio=1.) + coord_flip() +
  labs(x="Event Types", y="No. of Fatalities")
h2 <- ggplot(injuries, aes(x=reorder(EVTYPE, INJURIES), y=INJURIES)) +
  geom_bar(stat="identity") + theme(aspect.ratio=1.) + coord_flip() +
  labs(x="Event Types", y="No. of Injuries")

g1 <- ggplotGrob(h1)
g2 <- ggplotGrob(h2)
plottitle <- textGrob(
  'Fig. 1: Top 10 events harmful to U.S. population health (1950-2011)', just='centre')

g1$widths[4] <- unit(3,"null")
g <- gtable_rbind(g1, g2, size='first')
g <- gtable_add_rows(g, grobHeight(plottitle)+unit(2,"mm"), pos=0)
g <- gtable_add_grob(g, plottitle, 1, 1, r=7)
g
```

```
## TableGrob (21 x 7) "layout": 35 grobs
##     z         cells       name                                  grob
## 1   0 ( 2-11, 1- 7) background        rect[plot.background..rect.50]
## 2   5 ( 6- 6, 3- 3)     spacer                        zeroGrob[NULL]
## 3   7 ( 7- 7, 3- 3)     axis-l    absoluteGrob[GRID.absoluteGrob.45]
## 4   3 ( 8- 8, 3- 3)     spacer                        zeroGrob[NULL]
## 5   6 ( 6- 6, 4- 4)     axis-t                        zeroGrob[NULL]
## 6   1 ( 7- 7, 4- 4)      panel               gTree[panel-1.gTree.25]
## 7   9 ( 8- 8, 4- 4)     axis-b    absoluteGrob[GRID.absoluteGrob.38]
## 8   4 ( 6- 6, 5- 5)     spacer                        zeroGrob[NULL]
## 9   8 ( 7- 7, 5- 5)     axis-r                        zeroGrob[NULL]
## 10  2 ( 8- 8, 5- 5)     spacer                        zeroGrob[NULL]
## 11 10 ( 5- 5, 4- 4)     xlab-t                        zeroGrob[NULL]
## 12 11 ( 9- 9, 4- 4)     xlab-b titleGrob[axis.title.x..titleGrob.31]
## 13 12 ( 7- 7, 2- 2)     ylab-l titleGrob[axis.title.y..titleGrob.28]
## 14 13 ( 7- 7, 6- 6)     ylab-r                        zeroGrob[NULL]
## 15 14 ( 4- 4, 4- 4)   subtitle  zeroGrob[plot.subtitle..zeroGrob.47]
## 16 15 ( 3- 3, 4- 4)      title     zeroGrob[plot.title..zeroGrob.46]
## 17 16 (10-10, 4- 4)    caption   zeroGrob[plot.caption..zeroGrob.48]
## 18  0 (12-21, 1- 7) background        rect[plot.background..rect.90]
## 19  5 (16-16, 3- 3)     spacer                        zeroGrob[NULL]
## 20  7 (17-17, 3- 3)     axis-l    absoluteGrob[GRID.absoluteGrob.85]
## 21  3 (18-18, 3- 3)     spacer                        zeroGrob[NULL]
## 22  6 (16-16, 4- 4)     axis-t                        zeroGrob[NULL]
## 23  1 (17-17, 4- 4)      panel               gTree[panel-1.gTree.65]
## 24  9 (18-18, 4- 4)     axis-b    absoluteGrob[GRID.absoluteGrob.78]
## 25  4 (16-16, 5- 5)     spacer                        zeroGrob[NULL]
## 26  8 (17-17, 5- 5)     axis-r                        zeroGrob[NULL]
## 27  2 (18-18, 5- 5)     spacer                        zeroGrob[NULL]
## 28 10 (15-15, 4- 4)     xlab-t                        zeroGrob[NULL]
## 29 11 (19-19, 4- 4)     xlab-b titleGrob[axis.title.x..titleGrob.71]
## 30 12 (17-17, 2- 2)     ylab-l titleGrob[axis.title.y..titleGrob.68]
## 31 13 (17-17, 6- 6)     ylab-r                        zeroGrob[NULL]
## 32 14 (14-14, 4- 4)   subtitle  zeroGrob[plot.subtitle..zeroGrob.87]
## 33 15 (13-13, 4- 4)      title     zeroGrob[plot.title..zeroGrob.86]
## 34 16 (20-20, 4- 4)    caption   zeroGrob[plot.caption..zeroGrob.88]
## 35 17 ( 1- 1, 1- 7)     layout                    text[GRID.text.91]
```

```r
g$heights
```

```
##  [1] 1grobheight+2mm          5.5pt                   
##  [3] 0cm                      0cm                     
##  [5] 0cm                      0cm                     
##  [7] 1null                    sum(2.75pt, 1grobheight)
##  [9] 1grobheight              0cm                     
## [11] 5.5pt                    5.5pt                   
## [13] 0cm                      0cm                     
## [15] 0cm                      0cm                     
## [17] 1null                    sum(2.75pt, 1grobheight)
## [19] 1grobheight              0cm                     
## [21] 5.5pt
```

```r
grid.newpage()
grid.draw(g)
```

![](PA2_storm_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

## Conclusion
