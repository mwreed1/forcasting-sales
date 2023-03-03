
library(tidyverse)
library(prophet)
library(readxl)
library(janitor)
library(lubridate)
library(scales)
library(MLmetrics)
library(timeDate)

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
country_name <- "France"

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


best = 10

param = list(0.001, 0.05, "additive")

# max are 0.5 and 10 min are 0.001 and 0.01

for(a in c(0.001, 0.005, 0.01, 0.05, 0.1, 0.5)){
  for(b in c(0.01, 0.05, 0.1, 0.5, 1, 5, 10)){
    for(c in c("additive", "multiplicative")){
      
      m <- prophet(
        # replace
        training, 
        seasonality.mode = c, 
        seasonality.prior.scale	= a,
        changepoint.prior.scale = b,
      )
      
      future <- make_future_dataframe(m, periods = z, freq = "day")
      
      forecast <- predict(m, future)
      
      pred_val <- forecast %>%
        slice_tail(n=z) %>%
        slice_head(n = z/3) %>%
        pull(yhat)
      true_val <- validation %>%
        pull(y)
      
      err <- MAPE(pred_val, true_val)
      
      if(err < best){
        param = list(a, b, c)
        best = err
        print("better param: ")
        print(param)
      }
      
    }
  }
}

