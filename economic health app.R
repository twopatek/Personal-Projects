#load libraries 
pacman::p_load(tidyverse, plotly, eFRED, shiny, RColorBrewer, readxl)

# set key to pull data from FRED
api_key <- "21489194ba838be7e47627eb82142f3a"
set_fred_key(api_key)

# define a list of series to load
series_list <- list(
  yield_curve = list(value = "T10Y2YM"),
  default_rate = list(value = "DRCCLACBS"),
  saving_rate = list(value = "PSAVERT"),
  credit_debt = list(value = "TOTALSL"),
  interest_rate = list(value = "DFF"),
  bank_borrow = list(value = "H8B3094NCBA"),
  repo = list(value = "RRPONTSYD"),
  price_food = list(value = "CPIUFDSL"),
  loss_reserves = list(value = "TOTRESNS"),
  interest_payments = list(value = "A091RC1Q027SBEA")
)

# load and clean data frames using purrr::map()
data_list <- map(series_list, function(series) {
  fred_data <- fred(series = series$value, spread = series$spread, total = series$total, y = series$y, all = series$all)
  colnames(fred_data) <- c("date", "value") # rename columns
  fred_data %>%
    mutate(id = row_number()) %>% 
    mutate(value_norm = ifelse(is.na(value), NA, round((value / max(value, na.rm = TRUE)), 2))) %>%
    mutate(pct_change = ifelse(is.na(value), NA, round((value / lag(value) - 1), 3) * 100)) %>% 
    mutate(series = series$value) %>% # add a column to identify the series
    mutate(series_long = if_else(series == "T10Y2YM", "yield_curve",
                                 if_else(series == "DRCCLACBS", "default_rate",
                                         if_else(series == "PSAVERT", "saving_rate",
                                                 if_else(series == "TOTALSL", "credit_debt",
                                                         if_else(series == "DFF", "interest_rate",
                                                                 if_else(series == "H8B3094NCBA", "bank_borrow",
                                                                         if_else(series == "RRPONTSYD", "repo",
                                                                                 if_else(series == "CPIUFDSL", "price_food",
                                                                                         if_else(series == "TOTRESNS", "loss_reserves",
                                                                                                 if_else(series == "A091RC1Q027SBEA", "interest_payments",
                                                                                                         "")))))))))))
})


# convert the list to a shared data frame
df <- bind_rows(data_list)

ui <- fluidPage(
  titlePanel("Economic Health Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("series_long", "Select series:", choices = unique(df$series_long), multiple = TRUE),
      p("Use the date range slider below to select a specific period of time."),
      p("Hover your cursor over the line plot to see data series analytics."),
      p("When comparing multiple series, click the 'Compare Data on Hover' button in the top-right corner  "),
      p("*Shaded Areas Represent GDP-based Recessions"),
      p("Data Source: FRED")
    ),
    mainPanel(
      plotlyOutput("plot", 900, 600),
      textOutput("percent_change")  # Added text output
    )
  )
)

# server
server <- function(input, output, session) {
  
  # Define a color palette using RColorBrewer
  palette <- brewer.pal(n = length(unique(df$series_long)), name = "Set3")
  
  # Map each series to a color from the palette
  colors <- setNames(palette, unique(df$series_long))
  
  # filter data based on the selected series
  filtered_data <- reactive({
    df %>%
      filter(series_long %in% input$series_long)
  })
  
  # plot
  output$plot <- renderPlotly({
    data <- filtered_data()
    
    p <- plot_ly(data, x = ~date) %>%
      add_lines(y = ~value_norm, name = ~series_long, text = ~paste("Series: ", series_long, "<br>Date: ", date, "<br>Value: ", value, "<br>Pct Change: ", pct_change, "%"), hoverinfo = "text", color = ~series_long) %>%
      layout(
        title = "Historical Data of Consumer Financial Health",  # Add a title to the plot
        xaxis = list(
          rangeslider = list(type = "Period Range")
        ),
        yaxis = list(title = "Normalized Value")
      ) %>%
      layout(plot_bgcolor = "#f7f7f7") %>%
      layout(showlegend = TRUE) %>%
      layout(legend = list(x = 0, y = 1, bgcolor = "#E2E2E2", font = list(size = 10))) %>%
      layout(title = list(x = 0.5)) %>%
      layout(colorway = colors) %>%
      layout(dragmode = "select")
    
    # Add rectangular annotations using layout()
    p <- layout(p, shapes = list(
      list(
        type = "rect",
        x0 = as.Date("1979-10-01"),
        x1 = as.Date("1979-10-31"),
        y0 = 0,
        y1 = 1,
        fillcolor = "blue",
        opacity = 0.1
      ),
      list(
        type = "rect",
        x0 = as.Date("1981-04-01"),
        x1 = as.Date("1982-04-01"),
        y0 = 0,
        y1 = 1,
        fillcolor = "blue",
        opacity = 0.1
      ),
      list(
        type = "rect",
        x0 = as.Date("1989-10-01"),
        x1 = as.Date("1991-01-01"),
        y0 = 0,
        y1 = 1,
        fillcolor = "blue",
        opacity = 0.1
      ),
      list(
        type = "rect",
        x0 = as.Date("2001-01-01"),
        x1 = as.Date("2001-07-01"),
        y0 = 0,
        y1 = 1,
        fillcolor = "blue",
        opacity = 0.1
      ),
      list(
        type = "rect",
        x0 = as.Date("2007-10-01"),
        x1 = as.Date("2009-04-01"),
        y0 = 0,
        y1 = 1,
        fillcolor = "blue",
        opacity = 0.1
      ),
      list(
        type = "rect",
        x0 = as.Date("2020-01-01"),
        x1 = as.Date("2020-04-01"),
        y0 = 0,
        y1 = 1,
        fillcolor = "blue",
        opacity = 0.1
      )
    ))
    
    p
  })
  
}

shinyApp(ui = ui, server = server)

