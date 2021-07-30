---
title: "Sensor data description"
author: "DIME"
date: "13/12/2020"
output: 
 html_document:
    toc: true
    toc_depth: 3
    toc_float: true
    code_folding: hide
    toc_fold: TRUE
    highlight: tango
    keep_md: yes
    theme: cosmo
    number_sections: true
---

# Summary

* This document describes the sensor data for the matatu the sensor is placed in. We have been testing two sensors since September. Over the last 4 weeks, we have been trying to test different platforms to download the data from in order to get a dataset at the vehicle-time level which updates every 10 seconds. \
* Since we have been moving platforms, here we show the summary of 10 days of data in the last week of October and 2nd week for November.



# Number of days of data
* Data for days:\
* -- Oct 22- Oct 27 and,\
  -- Nov 11,12,13,19.\
* The sensor is on Alfred's car and a Matatu. This document has the information on the matatu.
* Gaps in data collection exists since we were trying 2-3 different platforms to store data on.
* Overall, the main variables of interest for us are the: \
    -lat/lon, \
    -speed,  \
    -harsh braking (dummy and magnitude) \
* Lat/lon, speed seem to be alright and making sense overall. 
* We need to bring up the following:
  -- Dates for which there are 0 values for speed and other variables, \
  -- Meaning if harsh braking/harsh acceleration is 0 \


```r
# Speed over time
sensor_data_days <- ggplot(sensor, aes(x=Time, y=Speed)) +
  geom_line(color="#69b3a2",  linetype=2) +
theme_ipsum() 
```
# Variable summary

## Speed
* We suspect the 0 in speed can be a combination of :\
  -- Less matatu movement due to covid, \
  -- They were having some technical issue in the device in the last week of October, first week of November due to which the device was not collecting any data. \
  -- We will make sure to check the reasons for this with Sam.
  

```r
sensor$speed_dum <- ifelse(sensor$Speed == 0, 1, 0)
kable(table(sensor$date, sensor$speed_dum), col.names = c("Non-zero speed", "Zero speed"), caption = "Speed = 0 km/h by date") %>%
  kable_styling(bootstrap_options = c("striped", "hover"))
```

<table class="table table-striped table-hover" style="margin-left: auto; margin-right: auto;">
<caption>Speed = 0 km/h by date</caption>
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> Non-zero speed </th>
   <th style="text-align:right;"> Zero speed </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 2020-10-22 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2020-10-23 </td>
   <td style="text-align:right;"> 2095 </td>
   <td style="text-align:right;"> 208 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2020-10-24 </td>
   <td style="text-align:right;"> 2352 </td>
   <td style="text-align:right;"> 437 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2020-10-25 </td>
   <td style="text-align:right;"> 4096 </td>
   <td style="text-align:right;"> 432 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2020-10-26 </td>
   <td style="text-align:right;"> 4178 </td>
   <td style="text-align:right;"> 399 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2020-10-27 </td>
   <td style="text-align:right;"> 3487 </td>
   <td style="text-align:right;"> 337 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2020-11-11 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 571 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2020-11-12 </td>
   <td style="text-align:right;"> 3756 </td>
   <td style="text-align:right;"> 8433 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2020-11-13 </td>
   <td style="text-align:right;"> 1436 </td>
   <td style="text-align:right;"> 2529 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2020-11-19 </td>
   <td style="text-align:right;"> 1360 </td>
   <td style="text-align:right;"> 824 </td>
  </tr>
</tbody>
</table>

## Harsh Acceleration
* Values represent magnitudes of harsh acceleration. 
* Values are being shown only when speed is non-zero


```r
speed_valid <- subset(sensor, sensor$Speed != 0)
kable(table(speed_valid$date, speed_valid$`Harsh Acceleration*`), caption = "Harsh Acceleration values by Date") %>%
  kable_styling(bootstrap_options = c("striped", "hover"))
```

<table class="table table-striped table-hover" style="margin-left: auto; margin-right: auto;">
<caption>Harsh Acceleration values by Date</caption>
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> ---- </th>
   <th style="text-align:right;"> 0.16 </th>
   <th style="text-align:right;"> 0.28 </th>
   <th style="text-align:right;"> 0.31 </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 2020-10-22 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2020-10-23 </td>
   <td style="text-align:right;"> 2095 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2020-10-24 </td>
   <td style="text-align:right;"> 2352 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2020-10-25 </td>
   <td style="text-align:right;"> 4096 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2020-10-26 </td>
   <td style="text-align:right;"> 4178 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2020-10-27 </td>
   <td style="text-align:right;"> 3486 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2020-11-12 </td>
   <td style="text-align:right;"> 3756 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2020-11-13 </td>
   <td style="text-align:right;"> 1436 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2020-11-19 </td>
   <td style="text-align:right;"> 1358 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
</tbody>
</table>

## Harsh Braking
* Values represent magnitude of harsh braking
* For instance 0.31 is harsher than 0.16
* Values are being shown only when speed is non-zero


```r
library(knitr)
library(kableExtra)

kable(table(speed_valid$date, speed_valid$`Harsh Braking*`), caption = "Harsh Braking values by Date") %>%
  kable_styling(bootstrap_options = c("striped", "hover"))
```

<table class="table table-striped table-hover" style="margin-left: auto; margin-right: auto;">
<caption>Harsh Braking values by Date</caption>
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> ---- </th>
   <th style="text-align:right;"> 0.00 </th>
   <th style="text-align:right;"> 0.19 </th>
   <th style="text-align:right;"> 0.22 </th>
   <th style="text-align:right;"> 0.25 </th>
   <th style="text-align:right;"> 0.28 </th>
   <th style="text-align:right;"> 0.31 </th>
   <th style="text-align:right;"> 0.33 </th>
   <th style="text-align:right;"> 0.36 </th>
   <th style="text-align:right;"> 0.42 </th>
   <th style="text-align:right;"> 0.50 </th>
   <th style="text-align:right;"> 0.65 </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 2020-10-22 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2020-10-23 </td>
   <td style="text-align:right;"> 2095 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2020-10-24 </td>
   <td style="text-align:right;"> 2352 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2020-10-25 </td>
   <td style="text-align:right;"> 4096 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2020-10-26 </td>
   <td style="text-align:right;"> 4162 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2020-10-27 </td>
   <td style="text-align:right;"> 3453 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 19 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2020-11-12 </td>
   <td style="text-align:right;"> 3747 </td>
   <td style="text-align:right;"> 9 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2020-11-13 </td>
   <td style="text-align:right;"> 1424 </td>
   <td style="text-align:right;"> 12 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2020-11-19 </td>
   <td style="text-align:right;"> 1346 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 8 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
</tbody>
</table>
##  Harsh Cornering
* Missing for all
* Values are being shown only when speed is non-zero


```r
library(knitr)
library(kableExtra)

kable(table(speed_valid$date, speed_valid$`Harsh Cornering*`), caption = "Harsh Cornering values by Date") %>%
  kable_styling(bootstrap_options = c("striped", "hover"))
```

<table class="table table-striped table-hover" style="margin-left: auto; margin-right: auto;">
<caption>Harsh Cornering values by Date</caption>
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> ---- </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 2020-10-22 </td>
   <td style="text-align:right;"> 4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2020-10-23 </td>
   <td style="text-align:right;"> 2095 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2020-10-24 </td>
   <td style="text-align:right;"> 2352 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2020-10-25 </td>
   <td style="text-align:right;"> 4096 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2020-10-26 </td>
   <td style="text-align:right;"> 4178 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2020-10-27 </td>
   <td style="text-align:right;"> 3487 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2020-11-12 </td>
   <td style="text-align:right;"> 3756 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2020-11-13 </td>
   <td style="text-align:right;"> 1436 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2020-11-19 </td>
   <td style="text-align:right;"> 1360 </td>
  </tr>
</tbody>
</table>

## Ignition
*Dummy whether ignition was on or off

```r
library(knitr)
library(kableExtra)

kable(table(speed_valid$date, speed_valid$`Ignition*`), caption = "Ignition (On/Off) by Date") %>%
  kable_styling(bootstrap_options = c("striped", "hover"))
```

<table class="table table-striped table-hover" style="margin-left: auto; margin-right: auto;">
<caption>Ignition (On/Off) by Date</caption>
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> Off </th>
   <th style="text-align:right;"> On </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 2020-10-22 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2020-10-23 </td>
   <td style="text-align:right;"> 43 </td>
   <td style="text-align:right;"> 2052 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2020-10-24 </td>
   <td style="text-align:right;"> 93 </td>
   <td style="text-align:right;"> 2259 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2020-10-25 </td>
   <td style="text-align:right;"> 46 </td>
   <td style="text-align:right;"> 4050 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2020-10-26 </td>
   <td style="text-align:right;"> 26 </td>
   <td style="text-align:right;"> 4152 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2020-10-27 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 3487 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2020-11-12 </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 3750 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2020-11-13 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1436 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2020-11-19 </td>
   <td style="text-align:right;"> 69 </td>
   <td style="text-align:right;"> 1291 </td>
  </tr>
</tbody>
</table>
## Braking - Event dummy 
* Dummy whether braking occurred or not
* Not sure of this variable, does it turn on when any brake is applied or when vehicle comes to a halt?
* Need to check whether is braking dummy or harsh braking dummy
* Values are being shown for all data and for the subset of values when speed is non-zero.


```r
library(knitr)
library(kableExtra)

kable(table(speed_valid$date, speed_valid$`Event: braking*`), caption = "Braking event by Date (Speed (Non-zero)") %>%
  kable_styling(bootstrap_options = c("striped", "hover"))
```

<table class="table table-striped table-hover" style="margin-left: auto; margin-right: auto;">
<caption>Braking event by Date (Speed (Non-zero)</caption>
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> ---- </th>
   <th style="text-align:right;"> Off </th>
   <th style="text-align:right;"> On </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 2020-10-22 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2020-10-23 </td>
   <td style="text-align:right;"> 2095 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2020-10-24 </td>
   <td style="text-align:right;"> 2352 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2020-10-25 </td>
   <td style="text-align:right;"> 4096 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2020-10-26 </td>
   <td style="text-align:right;"> 4162 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 16 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2020-10-27 </td>
   <td style="text-align:right;"> 3452 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 34 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2020-11-12 </td>
   <td style="text-align:right;"> 3747 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 9 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2020-11-13 </td>
   <td style="text-align:right;"> 1424 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 12 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2020-11-19 </td>
   <td style="text-align:right;"> 1344 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 14 </td>
  </tr>
</tbody>
</table>

```r
kable(table(sensor$date, sensor$`Event: braking*`), caption = "Braking event by Date (Speed includes zero)") %>%
  kable_styling(bootstrap_options = c("striped", "hover"))
```

<table class="table table-striped table-hover" style="margin-left: auto; margin-right: auto;">
<caption>Braking event by Date (Speed includes zero)</caption>
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> ---- </th>
   <th style="text-align:right;"> Off </th>
   <th style="text-align:right;"> On </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 2020-10-22 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2020-10-23 </td>
   <td style="text-align:right;"> 2303 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2020-10-24 </td>
   <td style="text-align:right;"> 2789 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2020-10-25 </td>
   <td style="text-align:right;"> 4528 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2020-10-26 </td>
   <td style="text-align:right;"> 4561 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 16 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2020-10-27 </td>
   <td style="text-align:right;"> 3789 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 34 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2020-11-11 </td>
   <td style="text-align:right;"> 571 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2020-11-12 </td>
   <td style="text-align:right;"> 12180 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 9 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2020-11-13 </td>
   <td style="text-align:right;"> 3953 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 12 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2020-11-19 </td>
   <td style="text-align:right;"> 2168 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 14 </td>
  </tr>
</tbody>
</table>
## Acceleration - Event dummy 
* Dummy whether harsh acceleration happened
* Values are being shown only when speed is non-zero


```r
library(knitr)
library(kableExtra)

kable(table(speed_valid$date, speed_valid$`Event: acceleration*`), caption = "Speeding event by Date") %>%
  kable_styling(bootstrap_options = c("striped", "hover"))
```

<table class="table table-striped table-hover" style="margin-left: auto; margin-right: auto;">
<caption>Speeding event by Date</caption>
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> ---- </th>
   <th style="text-align:right;"> Off </th>
   <th style="text-align:right;"> On (harsh acceleration) </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 2020-10-22 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2020-10-23 </td>
   <td style="text-align:right;"> 2095 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2020-10-24 </td>
   <td style="text-align:right;"> 2352 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2020-10-25 </td>
   <td style="text-align:right;"> 4096 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2020-10-26 </td>
   <td style="text-align:right;"> 4162 </td>
   <td style="text-align:right;"> 16 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2020-10-27 </td>
   <td style="text-align:right;"> 3452 </td>
   <td style="text-align:right;"> 34 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2020-11-12 </td>
   <td style="text-align:right;"> 3747 </td>
   <td style="text-align:right;"> 9 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2020-11-13 </td>
   <td style="text-align:right;"> 1424 </td>
   <td style="text-align:right;"> 12 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2020-11-19 </td>
   <td style="text-align:right;"> 1344 </td>
   <td style="text-align:right;"> 14 </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
</tbody>
</table>

# Speed over time for 19th November 2020

```r
subset <- subset(sensor, sensor$date == "2020-11-19")
sensor_data_19 <- ggplot(subset, aes(x=Time, y=Speed)) +
  geom_line(color="#69b3a2",  linetype=2) +
theme_ipsum() 
print(sensor_data_19)
```

![](sensor_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

# Speed tracing between 15:00 - 17:00 19th November 2020

```r
time_subset <- subset(subset, subset = hour %in% c(15,16))
sensor_data_15 <- ggplot(time_subset, aes(x=Time, y=Speed)) +
  geom_line(color="#69b3a2",  linetype=1) +
  ggtitle("Speed tracing 19th November 15:00-17:00") +
theme_ipsum() 
print(sensor_data_15)
```

![](sensor_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

# Map of route on 4 days




```r
ggmap(basemap) +
  geom_point(data = sensor %>%
               filter(date %in% as.Date(c("2020-10-24",
                                          "2020-10-25",
                                          "2020-10-26",
                                          "2020-10-27"))) %>%
               arrange(Speed),
             aes(x = lon, y=lat),
             size = 1.2,
             color = "black") +
  geom_point(data = sensor %>%
               filter(date %in% as.Date(c("2020-10-24",
                                        "2020-10-25",
                                        "2020-10-26",
                                        "2020-10-27"))) %>%
               arrange(Speed),
             aes(x = lon, y=lat, color = Speed),
             size = 1) +

  labs(title = "Routes and Speeds Over Example Four Day Period\nfor Matatu KBA 970C\n",
       color = "Speed\n(km/hr)") +
  scale_color_gradientn(colours = rev(brewer.pal(n = 7, name = "Spectral"))) + 
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5,
                                  face = "bold",
                                  size = 10),
        legend.position = "bottom") +
  facet_wrap(~date)
```

![](sensor_files/figure-html/unnamed-chunk-12-1.png)<!-- -->
