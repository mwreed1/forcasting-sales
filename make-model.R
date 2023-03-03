library(tidyverse)
library(prophet)
library(readxl)
library(janitor)
library(lubridate)
library(scales)
library(MLmetrics)
library(timeDate)


make_model <- function(country_name){
  data1 <- read_excel("online_retail_ii.xlsx", sheet = 1)
  data2 <- read_excel("online_retail_ii.xlsx", sheet = 2)
  
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
  
  # replace
  # country_name <- "United Kingdom"
  
  daily_data <- data %>%
    filter(country == country_name, revenue > 0) %>%
    group_by(date) %>%
    summarize(daily_revenue = sum(revenue))
  
  p_data <- daily_data %>%
    filter(daily_revenue < 150000) %>%
    select(date, daily_revenue) %>%
    rename(ds = date, y = daily_revenue)
  
  
  x <- dim(p_data)[1]
  y <- round(x*0.7, 0)
  z <- x - y
  
  training <- p_data %>% slice_head(n=y)
  validation <- p_data %>% anti_join(training) %>% slice_head(n=z/3)
  testing <- p_data %>% anti_join(training) %>% slice_tail(n=(2*z/3))
  
  # Replace
  uk_holidays <- tibble(ds = holidayLONDON(year = 2009:2011) %>%
                          as.Date(),
                        holiday = "holiday")
  
  m <- prophet(
    training, 
    seasonality.mode = "multiplicative", 
    seasonality.prior.scale	= 0.005,
    changepoint.prior.scale = 0.05
  )
  
  future <- make_future_dataframe(m, periods = z, freq = "day")
  
  forecast <- predict(m, future)
  
  return(list(forecast, testing, p_data))
}



