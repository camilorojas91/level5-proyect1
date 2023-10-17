---
title: "Proyecto 1"
author: "Camilo Rojas"
date: "2023-10-11"
output:
  pdf_document: default
---



```r
#tinytex::install_tinytex()
library(latexpdf)
library(knitr)
opts_chunk$set(echo = TRUE)
```

First question
==============

I load the dataset and named it as ***data***


```r
library(rio)

data <- import("./activity.csv")

data$steps <- as.numeric(data$steps)
data$date <- as.Date(data$date)
```

second question
=================



```r
total_steps <- sum(data$steps, na.rm = T)
```

1. the number total of steps in this dataset is \ensuremath{5.70608\times 10^{5}}

2. i did the histogram per day vs number of steps

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(ggplot2)

data %>%
 filter(!is.na(steps)) %>%
 ggplot() +
  aes(x = date, weight = steps) +
  geom_bar(fill = "#112446") +
  labs(
    x = "Day",
    y = "number of steps",
    title = "steps per day"
  ) +
  theme_classic()
```

![](project1_files/figure-latex/unnamed-chunk-4-1.pdf)<!-- --> 
3. the next table show report the mean and median of the total number of steps taken per day


```r
library(kableExtra)
```

```
## 
## Attaching package: 'kableExtra'
```

```
## The following object is masked from 'package:dplyr':
## 
##     group_rows
```

```r
library(dplyr)
data1 <- data %>% 
        select(date, steps) %>% 
        group_by(date) %>% 
        summarise(median = median(steps,na.rm = T), mean = mean(steps,na.rm = T))
data1$mean <- ifelse(is.nan(data1$mean),0,data1$mean)
data1$median <- ifelse(is.nan(data1$median),0,data1$median)
kbl(data1) %>% 
        kable_styling()
```

\begin{table}
\centering
\begin{tabular}[t]{l|r|r}
\hline
date & median & mean\\
\hline
2012-10-01 & NA & 0.0000000\\
\hline
2012-10-02 & 0 & 0.4375000\\
\hline
2012-10-03 & 0 & 39.4166667\\
\hline
2012-10-04 & 0 & 42.0694444\\
\hline
2012-10-05 & 0 & 46.1597222\\
\hline
2012-10-06 & 0 & 53.5416667\\
\hline
2012-10-07 & 0 & 38.2465278\\
\hline
2012-10-08 & NA & 0.0000000\\
\hline
2012-10-09 & 0 & 44.4826389\\
\hline
2012-10-10 & 0 & 34.3750000\\
\hline
2012-10-11 & 0 & 35.7777778\\
\hline
2012-10-12 & 0 & 60.3541667\\
\hline
2012-10-13 & 0 & 43.1458333\\
\hline
2012-10-14 & 0 & 52.4236111\\
\hline
2012-10-15 & 0 & 35.2048611\\
\hline
2012-10-16 & 0 & 52.3750000\\
\hline
2012-10-17 & 0 & 46.7083333\\
\hline
2012-10-18 & 0 & 34.9166667\\
\hline
2012-10-19 & 0 & 41.0729167\\
\hline
2012-10-20 & 0 & 36.0937500\\
\hline
2012-10-21 & 0 & 30.6284722\\
\hline
2012-10-22 & 0 & 46.7361111\\
\hline
2012-10-23 & 0 & 30.9652778\\
\hline
2012-10-24 & 0 & 29.0104167\\
\hline
2012-10-25 & 0 & 8.6527778\\
\hline
2012-10-26 & 0 & 23.5347222\\
\hline
2012-10-27 & 0 & 35.1354167\\
\hline
2012-10-28 & 0 & 39.7847222\\
\hline
2012-10-29 & 0 & 17.4236111\\
\hline
2012-10-30 & 0 & 34.0937500\\
\hline
2012-10-31 & 0 & 53.5208333\\
\hline
2012-11-01 & NA & 0.0000000\\
\hline
2012-11-02 & 0 & 36.8055556\\
\hline
2012-11-03 & 0 & 36.7048611\\
\hline
2012-11-04 & NA & 0.0000000\\
\hline
2012-11-05 & 0 & 36.2465278\\
\hline
2012-11-06 & 0 & 28.9375000\\
\hline
2012-11-07 & 0 & 44.7326389\\
\hline
2012-11-08 & 0 & 11.1770833\\
\hline
2012-11-09 & NA & 0.0000000\\
\hline
2012-11-10 & NA & 0.0000000\\
\hline
2012-11-11 & 0 & 43.7777778\\
\hline
2012-11-12 & 0 & 37.3784722\\
\hline
2012-11-13 & 0 & 25.4722222\\
\hline
2012-11-14 & NA & 0.0000000\\
\hline
2012-11-15 & 0 & 0.1423611\\
\hline
2012-11-16 & 0 & 18.8923611\\
\hline
2012-11-17 & 0 & 49.7881944\\
\hline
2012-11-18 & 0 & 52.4652778\\
\hline
2012-11-19 & 0 & 30.6979167\\
\hline
2012-11-20 & 0 & 15.5277778\\
\hline
2012-11-21 & 0 & 44.3993056\\
\hline
2012-11-22 & 0 & 70.9270833\\
\hline
2012-11-23 & 0 & 73.5902778\\
\hline
2012-11-24 & 0 & 50.2708333\\
\hline
2012-11-25 & 0 & 41.0902778\\
\hline
2012-11-26 & 0 & 38.7569444\\
\hline
2012-11-27 & 0 & 47.3819444\\
\hline
2012-11-28 & 0 & 35.3576389\\
\hline
2012-11-29 & 0 & 24.4687500\\
\hline
2012-11-30 & NA & 0.0000000\\
\hline
\end{tabular}
\end{table}
third question
=================

time series plot for 5-minute interval


```r
interval5 <- data %>% 
        filter(interval == 5)

interval5 %>%
 filter(!is.na(steps)) %>%
 ggplot() +
  aes(x = date, weight = steps) +
  geom_bar(fill = "#112446") +
  theme_minimal()
```

![](project1_files/figure-latex/unnamed-chunk-6-1.pdf)<!-- --> 

the day that has the maximun number of steps is 2012-10-10

fourd question
=================


```r
steps_na <- sum(is.na(data$steps))
date_na <- sum(is.na(data$date))
interval_na <- sum(is.na(data$interval))
```

1. In this dataset i could found that we has 2304 NA values at the steps column, 0 NA values at the date column and 0 NA values at the interval column 

2. fill de Na values in the dataset


```r
fill_data <- merge(data, data1, by = "date")

fill_data$steps_adj <- ifelse(is.na(fill_data$steps), fill_data$mean, fill_data$steps)
        

fill_data %>%
 ggplot() +
  aes(x = date, weight = steps) +
  geom_bar(fill = "#112446") +
  theme_minimal()
```

![](project1_files/figure-latex/unnamed-chunk-8-1.pdf)<!-- --> 

when all the values filled we can see that the values for the mean and median per day looks like 


```r
data2 <- fill_data %>% 
        select(date, steps) %>% 
        group_by(date) %>% 
        summarise(median = median(steps), mean = mean(steps))

kbl(data2) %>% 
        kable_styling()
```

\begin{table}
\centering
\begin{tabular}[t]{l|r|r}
\hline
date & median & mean\\
\hline
2012-10-01 & NA & NA\\
\hline
2012-10-02 & 0 & 0.4375000\\
\hline
2012-10-03 & 0 & 39.4166667\\
\hline
2012-10-04 & 0 & 42.0694444\\
\hline
2012-10-05 & 0 & 46.1597222\\
\hline
2012-10-06 & 0 & 53.5416667\\
\hline
2012-10-07 & 0 & 38.2465278\\
\hline
2012-10-08 & NA & NA\\
\hline
2012-10-09 & 0 & 44.4826389\\
\hline
2012-10-10 & 0 & 34.3750000\\
\hline
2012-10-11 & 0 & 35.7777778\\
\hline
2012-10-12 & 0 & 60.3541667\\
\hline
2012-10-13 & 0 & 43.1458333\\
\hline
2012-10-14 & 0 & 52.4236111\\
\hline
2012-10-15 & 0 & 35.2048611\\
\hline
2012-10-16 & 0 & 52.3750000\\
\hline
2012-10-17 & 0 & 46.7083333\\
\hline
2012-10-18 & 0 & 34.9166667\\
\hline
2012-10-19 & 0 & 41.0729167\\
\hline
2012-10-20 & 0 & 36.0937500\\
\hline
2012-10-21 & 0 & 30.6284722\\
\hline
2012-10-22 & 0 & 46.7361111\\
\hline
2012-10-23 & 0 & 30.9652778\\
\hline
2012-10-24 & 0 & 29.0104167\\
\hline
2012-10-25 & 0 & 8.6527778\\
\hline
2012-10-26 & 0 & 23.5347222\\
\hline
2012-10-27 & 0 & 35.1354167\\
\hline
2012-10-28 & 0 & 39.7847222\\
\hline
2012-10-29 & 0 & 17.4236111\\
\hline
2012-10-30 & 0 & 34.0937500\\
\hline
2012-10-31 & 0 & 53.5208333\\
\hline
2012-11-01 & NA & NA\\
\hline
2012-11-02 & 0 & 36.8055556\\
\hline
2012-11-03 & 0 & 36.7048611\\
\hline
2012-11-04 & NA & NA\\
\hline
2012-11-05 & 0 & 36.2465278\\
\hline
2012-11-06 & 0 & 28.9375000\\
\hline
2012-11-07 & 0 & 44.7326389\\
\hline
2012-11-08 & 0 & 11.1770833\\
\hline
2012-11-09 & NA & NA\\
\hline
2012-11-10 & NA & NA\\
\hline
2012-11-11 & 0 & 43.7777778\\
\hline
2012-11-12 & 0 & 37.3784722\\
\hline
2012-11-13 & 0 & 25.4722222\\
\hline
2012-11-14 & NA & NA\\
\hline
2012-11-15 & 0 & 0.1423611\\
\hline
2012-11-16 & 0 & 18.8923611\\
\hline
2012-11-17 & 0 & 49.7881944\\
\hline
2012-11-18 & 0 & 52.4652778\\
\hline
2012-11-19 & 0 & 30.6979167\\
\hline
2012-11-20 & 0 & 15.5277778\\
\hline
2012-11-21 & 0 & 44.3993056\\
\hline
2012-11-22 & 0 & 70.9270833\\
\hline
2012-11-23 & 0 & 73.5902778\\
\hline
2012-11-24 & 0 & 50.2708333\\
\hline
2012-11-25 & 0 & 41.0902778\\
\hline
2012-11-26 & 0 & 38.7569444\\
\hline
2012-11-27 & 0 & 47.3819444\\
\hline
2012-11-28 & 0 & 35.3576389\\
\hline
2012-11-29 & 0 & 24.4687500\\
\hline
2012-11-30 & NA & NA\\
\hline
\end{tabular}
\end{table}

fifth question
===============


```r
fill_data$weekday <- weekdays(fill_data$date)

fill_data$weekday1 <- ifelse(fill_data$weekday == "Saturday" | fill_data$weekday == "Sunday", "Weekend", "Weekday")

fill_data1 <- fill_data %>%
        select(steps_adj, interval, weekday1) %>% 
        group_by(interval, weekday1) %>% 
        summarise(mean(steps_adj))
```

```
## `summarise()` has grouped output by 'interval'. You can override using the
## `.groups` argument.
```

```r
ggplot(fill_data1) +
  aes(x = interval, y = `mean(steps_adj)`) +
  geom_step(colour = "#112446") +
  theme_minimal() +
  facet_wrap(vars(weekday1))
```

![](project1_files/figure-latex/unnamed-chunk-10-1.pdf)<!-- --> 
