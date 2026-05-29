# Time-Series-Analysis-and-Return-Examination-on-a-Single-Stock-in-the-Dow-Jones-Index
Time Series Analysis and Return Examination on a DJIA stock selected by student number. The project analyzes stock price movements over time, examines return series, and explores key financial time series characteristics to better understand stock behavior and market dynamics.
"---
title: "Forecasting 1: Exploratory Data Analysis of DJIA"
author: "Hatice Yuksel"
date: "`r Sys.Date()`"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE)
library(tidyverse)
library(quantmod)
library(XML)
library(ggplot2)
```


## Step 0: Data Loading and Sampling

First, load the full DJIA dataset provided (`djdata.RData`). You must use the code block below to "seed" the random selection using your last **7 digit Student ID**. This ensures every student works on a randomized but reproducible task.



```{r sampling}
# Load your data (assumes the file is in your working directory)
load("djdata.RData") 

# there are two objects: 
# 1: djdata, xts object
# 2: djfata.df data frame 

# --- REPRODUCIBLE SAMPLING ---
student_id <- 2023804 # <--- CHANGE THIS TO YOUR STUDENT ID
set.seed(student_id)
my_stock <- sample(colnames(djdata), 1)

print(paste("My assigned stock for this project is:", my_stock))


```
## Part 1: Visual Interpretation

**Task:** Plot the daily closing price of your assigned stock.  

* **Plot:** Create a line chart showing the price evolution over time.  

* **Interpretation:** Describe the visual properties of the series. Does it exhibit a clear **trend**? Are there visible **cycles** or seasonal patterns? Identify any significant structural breaks (e.g., market crashes or sudden jumps).   



```{r plot-daily}
# Your code here
hati_data <- djdata[,my_stock]
data <- tibble(Date = index(hati_data),
Price = as.numeric(hati_data)
)

ggplot(data, aes(x = Date, y = Price)) +
geom_line(color = "navy", linewidth = 1) +
labs(
title = paste("Daily Closing Price of", my_stock),
x = "Date", y = "Closing Price"
 ) +
theme_minimal()
```

```{r}
#The line chart of the Daily Closing Price of PG shows a clear upward trend over the period indicating that the stock is increaed in value over time.There is no clear seasonal pattern. However data shows cyclical movements with alternating periods of increases and decreases.A significant structural break is observed around 2020,where price drops sharply but it recovers quickly.This probably shows a big market shock around that time. After 2020, the stock seems to grow faster compared to before.In the later years (around 2024–2025), the price becomes more unstable, with sharp increases and short drops.

```

## Part 2: Temporal Aggregation & Data Integrity
**Task:** Financial data often requires different frequencies for different models.  

1. **Conversion:** Aggregate your daily data into **Weekly** and **Monthly** series (using the last price of the period or the mean).  
2. **Missing Values:** Check for `NA` values in the daily, weekly, and monthly sets.  
3. **Handling NAs:** If NAs exist, explain how you would handle them (e.g., linear interpolation, carry-forward, or removal) and implement your choice.  

```{r aggregation}
# Your aggregation and NA check code here
weekly_avg <- apply.weekly(hati_data,mean)
monthly_avg <- apply.monthly(hati_data,mean)

sum(is.na(hati_data))
sum(is.na(weekly_avg))
sum(is.na(monthly_avg))


#No missing values were found in the daily,weekly and monthly datasets.Therefore no additional handling was required. If missing values were present i would handle them by removing them.
```

## Part 3: Preliminary Return Analysis

**Task:** Raw prices are often non-stationary. Economists usually prefer **Returns**.  

1. Calculate the **Daily Log Returns**, **Weekly Log Returns**, and **Monthly Log Returns**.   
2. **Analysis and Comparisons:**   
  + plot return series (daily, weekly and monthly)
  + plot histogram using daily and weekly data   
  + Compare the distributions for trading days using daily data  
  + obtain mean return per month, (as shown in **Time series graphics** part of the course)     
3. **Insight:** Comment on all of the plots and comparisons     

```{r returns}
# Formula hint: log_return = diff(log(price))
ret_daily <- diff(log(hati_data))
ret_weekly <- diff(log(weekly_avg))
ret_monthly <-diff(log(monthly_avg))

daily_tib <- data_frame(Date = index(ret_daily) , Return = as.numeric(ret_daily))
weekly_tib <- data_frame(Date = index(ret_weekly) , Return = as.numeric(ret_weekly))
monthly_tib <- data_frame(Date = index(ret_monthly) , Return = as.numeric(ret_monthly))

#daily return plot 
ggplot(daily_tib, aes(x = Date, y = Return)) +
  geom_line(color = "blue") +
  labs(title = "Daily Log Returns", x = "Date", y = "Return") +
  theme_minimal()

# Weekly returns plot
ggplot(weekly_tib, aes(x = Date, y = Return)) +
  geom_line(color = "lightblue") +
  labs(title = "Weekly Log Returns", x = "Date", y = "Return") +
  theme_minimal()


# Monthly returns plot
ggplot(monthly_tib, aes(x = Date, y = Return)) +
  geom_line(color = "steelblue") +
  labs(title = "Monthly Log Returns", x = "Date", y = "Return") +
  theme_minimal()


# Daily histogram
ggplot(daily_tib, aes(x = Return)) +
  geom_histogram(binwidth = 0.005, fill = "blue", color = "navy") +
  labs(title = "Histogram of Daily Log Returns", x = "Return", y = "Frequency") +
  theme_minimal()

# Weekly histogram
ggplot(weekly_tib, aes(x = Return)) +
  geom_histogram(binwidth = 0.01, fill = "lightblue", color = "navy") +
  labs(title = "Histogram of Weekly Log Returns", x = "Return", y = "Frequency") +
  theme_minimal()

library(dplyr)
daily_tib <- daily_tib %>%
  mutate(Weekday = weekdays(Date))  

daily_tib %>%
  group_by(Weekday) %>%
  summarise(
    Mean = mean(Return, na.rm = TRUE),
    SD = sd(Return, na.rm = TRUE),
    Median = median(Return, na.rm = TRUE),
    Min = min(Return, na.rm = TRUE),
    Max = max(Return, na.rm = TRUE),
    Count = n()
  )

library(ggplot2)

ggplot(daily_tib, aes(x = Weekday, y = Return)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Daily Log Returns by Weekday",
       x = "Weekday", y = "Return") +
  theme_minimal()

library(dplyr)
library(lubridate)  


daily_tib <- daily_tib %>%
  mutate(Month = floor_date(Date, unit = "month"))

monthly_mean <- daily_tib %>%
 group_by(Month) %>%
 summarise(Mean_Return = mean(Return, na.rm = TRUE))

monthly_mean


library(ggplot2)

ggplot(monthly_mean, aes(x = Month, y = Mean_Return)) +
  geom_line(color = "steelblue") +
  geom_point(color = "darkblue") +
  labs(title = "Mean Daily Returns per Month",
       x = "Month", y = "Mean Daily Return") +
  theme_minimal()

```
```{r}
#Looking at the boxplot,the median for all five trading days (monday to friday) are positionedvery close to 0.00 horizontal line.This indicated that there is no strong "day of the week effect". where one day outperforms the other.he line chart shows that while most days have returns near zero, the market has 'mood swings.' High volatility tends to happen in clusters—meaning one big price change is often followed by another. This proves that market risk changes over time rather than staying constant.Daily log returns This plot shows the daily log returns of a financial asset, where returns mostly fluctuate around zero but occasionally experience dramatic spikes during periods of economic stress, with volatility clustering together in bursts rather than being constant over time.And also i can tell the same thing for weekly log returns.Monthly log returns of a financial asset, where returns fluctuate around zero with occasional sharp drops during major market downturns, and while volatility varies over time, the returns generally recover quickly, reflecting the cyclical nature of financial markets.



```
## Submission Guidelines

* Submit both your `.Rmd` file and the knitted `HTML` file.  

* Ensure your Student ID is correctly entered in the `set.seed()` function; otherwise, your results will not match your assigned stock.


