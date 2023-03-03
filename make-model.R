library(tidyverse)
library(prophet)
library(readxl)
library(janitor)
library(lubridate)
library(scales)
library(MLmetrics)
library(timeDate)


make_model <- function(country_name){
  
  # read data
  data1 <- read_excel("online_retail_ii.xlsx", sheet = 1)
  data2 <- read_excel("online_retail_ii.xlsx", sheet = 2)
  
  # clean data
  data <- data1 %>%
    rbind(data2) %>%
    clean_names()
  
  data <- data %>%
    mutate(
      revenue = quantity*price,
      date = ymd(as.Date(invoice_date)),
      year = year(as.Date(date)),
      month = month(date),
      hour = hour(invoice_date)
    )
  
  daily_data <- data %>%
    filter(country == country_name, revenue > 0) %>%
    group_by(date) %>%
    summarize(daily_revenue = sum(revenue))
  
  # filter outliers and prepare for prophet model
  p_data <- daily_data %>%
    filter(daily_revenue < 150000) %>%
    select(date, daily_revenue) %>%
    rename(ds = date, y = daily_revenue)
  
  
  # create training, validation, testing sets
  x <- dim(p_data)[1]
  y <- round(x*0.7, 0)
  z <- x - y
  
  training <- p_data %>% slice_head(n=y)
  validation <- p_data %>% anti_join(training) %>% slice_head(n=z/3)
  testing <- p_data %>% anti_join(training) %>% slice_tail(n=(2*z/3))
  
  # Holiday df
  uk_holidays <- tibble(ds = holidayLONDON(year = 2009:2011) %>%
                          as.Date(),
                        holiday = "holiday")
  
  # train model, hyper-parameters tuned with train-model.R 
  m <- prophet(
    training, 
    seasonality.mode = "multiplicative", 
    seasonality.prior.scale	= 0.005,
    changepoint.prior.scale = 0.05
  )
  
  # create new df
  future <- make_future_dataframe(m, periods = z, freq = "day")
  
  # predict new revenue data points
  forecast <- predict(m, future)
  
  # returns forecasted data, training set, and full dataset
  return(list(forecast, testing, p_data))
}



