#load libraries 
pacman::p_load(tidyverse, plotly, eFRED)

#set working directory
setwd("C:/Users/matt2/OneDrive/Coding Research/financial research")

#set key to pull data from FRED
api_key <- "21489194ba838be7e47627eb82142f3a"
set_fred_key(api_key)

#search for various series
search_results <- fred_search("yield curve", args = list(limit = 3))
search_results

#create data frames sourced from FRED
yield_curve <- fred(spread = "T10Y2YM", all = TRUE)
default_rate <- fred(rate = "DRCCLACBS", all = TRUE)
saving_rate <- fred(rate = "PSAVERT", all = TRUE)
credit_debt <- fred(total = "TOTALSL", all = TRUE)
interest_rate <- fred(rate = "DFF", all = TRUE)

#clean data frames for use 
yield_curve <- yield_curve %>% 
  mutate(id = row_number()) %>% 
  mutate(date = as.Date(date)) %>% 
  mutate(test_norm = spread/max(spread, na.rm = TRUE))

default_rate <- default_rate %>% 
  mutate(id = row_number()) %>% 
  mutate(date = as.Date(date)) %>% 
  mutate(test_norm = rate/max(rate, na.rm = TRUE))

saving_rate <- saving_rate %>% 
  mutate(id = row_number()) %>% 
  mutate(date = as.Date(date)) %>% 
  mutate(test_norm = rate/max(rate, na.rm = TRUE))
  
credit_debt <- credit_debt %>% 
  mutate(id = row_number()) %>% 
  mutate(date = as.Date(date)) %>% 
  mutate(test_norm = total/max(total, na.rm = TRUE))

interest_rate <- interest_rate %>% 
  mutate(id = row_number()) %>% 
  mutate(date = as.Date(date)) %>% 
  mutate(test_norm = rate/max(rate, na.rm = TRUE))
         
        
#line plots

p <- yield_curve %>% 
  plot_ly(x = ~date) %>%
  add_lines(y = ~test_norm, name = "Yield Spread") %>% 
  add_lines(data = credit_debt, y = ~test_norm, name = "Credit Card Debt") %>%
  add_lines(data = saving_rate, y = ~test_norm, name = "Personal Savings Rate") %>% 
  add_lines(data = default_rate, y = ~test_norm, name = "Delinquency Rate") %>% 
  add_lines(data = interest_rate, y = ~test_norm, name = "Interest Rate") %>% 
  layout(
    title = "Historical Data of Consumer Financial Health",
    xaxis = list(
      rangeslider = list(type = "Period Range")
    ),
    yaxis = list(title = "Normalized Value")
  )

p


  
