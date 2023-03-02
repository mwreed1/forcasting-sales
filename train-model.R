
library(tidyverse)
library(prophet)
library(readxl)
library(janitor)
library(lubridate)
library(scales)
library(MLmetrics)
library(timeDate)

best = 0.4138

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

