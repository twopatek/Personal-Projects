#load libraries
pacman::p_load(shiny, tidyverse, readxl, scales)

#set working directory 
setwd("C:/Users/matt2/OneDrive/Coding Research/financial research")

#load excel files
csv2 <- fred(spread = "T10Y2YM", all = TRUE)
csv3 <- read_excel("recession.xlsx")

#clean data frames
predf2 <- csv2 %>% 
  mutate(test = ! is.na(spread)) %>% 
  filter(test != "FALSE") %>% 
  select(-test) %>% 
  mutate(date = as.Date(date, format = "%y.%m.%d")) %>% 
  mutate(year_month = substr(date,0,7)) %>% 
  mutate(id = row_number()) %>% 
  relocate(id, .before = date)

predf3 <- csv3 %>% 
  mutate(indicator = if_else(indicator=="1","3","0")) %>% 
  mutate(year_month = substr(date,0,7)) %>% 
  mutate(id = n()) %>% 
  relocate(id, .before = date)

#working data frames
df <- predf2
recession <- predf3

#to match recession data to yield curve data 
df <- df %>% 
  mutate(indexym = match(df$year_month,recession$year_month,nomatch = NA_integer_))

m <- merge(df,recession,by.x = "indexym",by.y = "id",all.x = TRUE) %>% 
  arrange(id) %>% 
  select(date.x,spread,indicator) %>% 
  rename(date = date.x)

# Define UI for application that plots yield curve
ui <- fluidPage(
  # Application title
  titlePanel("10-Year Treasury Maturity Minus 2-Year Treasury Maturity"),
  sidebarLayout(
    sidebarPanel(
      verbatimTextOutput(
        "text"
      ), width = 4
    ),
    mainPanel(
      plotOutput("yieldPlot",width = 850,height = 500),
      sliderInput("year","Year:",
                  min = as.Date("1976-06-01","%Y-%m-%d"),
                  max = as.Date("2023-01-27","%Y-%m-%d"),
                  value = as.Date(c("2019-01-01","2023-01-27")),
                  timeFormat = "%Y-%m")
      
    )
  )
)
# Define server logic required to plot yield curve
server <- function(input, output) {
  output$yieldPlot <- renderPlot({
    ggplot()+
      geom_line(data = m, aes(x = date, y = spread), alpha = .5, show.legend = F)+
      geom_hline(yintercept=0,color = "red")+
      theme_minimal()+
      scale_y_continuous(labels = function(x) paste0(x, "%"), breaks = c(3,2,1,0,-1,-2,-3))+
      scale_x_date(limits = input$year, 
                   labels = date_format("%Y %b"),
                   breaks = scales::breaks_pretty(15))+
      labs(title = "*Shaded Area Represents GDP-Based Recession Indicator Index", 
           x = "Date",y = "Percent")+
      theme(plot.margin = unit(c(1,1,1,0), "cm"))+
      ggplot2::annotate("rect",
               xmin = as.Date("1979-10-01","%Y-%m-%d"),
               xmax = as.Date("1979-10-31","%Y-%m-%d"), 
               ymin = -3, 
               ymax = 3,
               alpha = .1, fill = "blue")+
      ggplot2::annotate("rect",
               xmin = as.Date("1981-04-01","%Y-%m-%d"),
               xmax = as.Date("1982-04-01","%Y-%m-%d"), 
               ymin = -3, 
               ymax = 3,
               alpha = .1, fill = "blue")+
      ggplot2::annotate("rect",
               xmin = as.Date("1989-10-01","%Y-%m-%d"),
               xmax = as.Date("1991-01-01","%Y-%m-%d"), 
               ymin = -3, 
               ymax = 3,
               alpha = .1, fill = "blue")+
      ggplot2::annotate("rect",
               xmin = as.Date("2001-01-01","%Y-%m-%d"),
               xmax = as.Date("2001-07-01","%Y-%m-%d"), 
               ymin = -3, 
               ymax = 3,
               alpha = .1, fill = "blue")+
      ggplot2::annotate("rect",
               xmin = as.Date("2007-10-01","%Y-%m-%d"),
               xmax = as.Date("2009-04-01","%Y-%m-%d"), 
               ymin = -3, 
               ymax = 3,
               alpha = .1, fill = "blue")+
      ggplot2::annotate("rect",
               xmin = as.Date("2020-01-01","%Y-%m-%d"),
               xmax = as.Date("2020-04-01","%Y-%m-%d"), 
               ymin = -3, 
               ymax = 3,
               alpha = .1, fill = "blue")
    
  })
  
  output$text <- renderText(
    "The U.S. yield curve measures the “shape” of yields
on U.S. government debt as maturities increase. 
Typically, as you hold U.S. debt with a later 
maturity, you receive a higher interest rate 
compared to shorter rates.

This means that the U.S. yield curve has generally 
had an upward slope. However, sometimes short rates 
can rise above longer rates and the curve slopes 
downward. That’s inversion.
                              
For the yield curve to invert, the Fed is typically
raising short-term rates, just as we’ve seen in 
2022. Typically the Fed’s actions in raising 
interest rates can reduce economic activity and 
that can cause a recession.
    
The yield curve is pretty good historically, but 
not perfect. Going back to the 1980s, the track 
record of yield curve inversion is basically
perfect in calling recessions, but if you go 
further back you can find some false signals,
and recessions aren’t all that common.

The fact that the yield curve is a leading
recession indicator can also make it slightly
vague.We suspect a recession is coming, but we
don’t know precisely when, and some argue we may
already be in one.
    
Source: Forbes")
}

# Run the application 
shinyApp(ui = ui, server = server)
