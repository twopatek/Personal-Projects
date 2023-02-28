#load libraries 
pacman::p_load(tidyverse, lubridate, eFRED, plotly)

#set key to pull data from FRED
api_key <- "21489194ba838be7e47627eb82142f3a"
set_fred_key(api_key)

#search for various series
search_results <- fred_search("YOUR SEARCH", args = list(limit = 3))
search_results

# Set the series ID for the electricity cost per kilowatt hour
series_id <- "APU000072610"

# Set the start and end dates for the data
start_date <- ymd("2006-01-01")
end_date <- today()

# Pull the data from FRED
df <- fred(series_id) %>%
  as_tibble() %>% 
  drop_na()

names(df) <- c("date","value")

electricity_cost <- df %>% 
  rename(cost_per_kWh = value) %>% 
  mutate(monthly_cost = cost_per_kWh*1000) %>%  
  mutate(test = lag(cost_per_kWh)) %>% 
  mutate(pct_change = (cost_per_kWh - test) / test) %>% 
  select(-test)

# Create the plotly object
p <- electricity_cost %>% 
  plot_ly(x = ~date) %>%
  add_lines(y = ~monthly_cost, name = "Cost of Electricity per Kilowatt-Hour in U.S.", 
            hovertemplate = "<b>Date:</b> %{x}<br><b>Monthly Cost:</b> %{y:$,.2f}<br><b>Percent Change:</b> %{text:.1f}%<extra></extra>",
            text = ~round(pct_change*100, 1)) %>% 
  layout(
    title = "Assumptions: Monthly Household Use of 1,000 KW",
    xaxis = list(
      rangeslider = list(type = "Period Range")
    ),
    yaxis = list(title = "Cost of Electricity per Kilowatt-Hour in U.S.")
  )
  
# Show the plot
p

# add_annotations(
#   x = Sys.Date()-100, y = max(electricity_cost$monthly_cost)*0.9, 
#   text = "Note: Monthly cost assumes 1000 kWs per month", 
#   showarrow = FALSE, font = list(size = 12, color = "black")
# )
