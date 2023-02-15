library(tidyverse)
library(quantmod)
library(xts)
library(data.table)

#function to pull stock price data 
get_price_data <- function (symbols, start,end,freq="daily",format="wide") {
  
  price_data <- NULL
  
  for(sym in symbols)  {
    
    temp <- getSymbols(Symbols = sym, from=start ,to=end,auto.assign = FALSE, periodicity=freq)
    names(temp) <- c("Open","High","Low","Close","Volume","Adjusted")
    temp <- temp[,c("Close")]
    names(temp) <- sym
    
    if(is.null(price_data)){
      price_data <- temp
    } else {
      price_data <- merge(price_data,temp)
    }
  }
  price_data <- as.data.table(price_data)
  setnames(price_data,"index","date")
  
  if(format=="long") {
    price_data <- melt(price_data, id.vars="date")
    names(price_data) <- c("date","symbol","price")
  }
  
  return(price_data)
}

tsla <- get_price_data(symbols = c("TSLA"),
                                 start="2023-01-01",
                                 end="2023-02-05",
                                 freq="daily", 
                                 format = "long")



ggplot(data = tsla , aes(x=date, y=price))+
  
  geom_line(color = "darkgoldenrod", size = 1.5, alpha = .8)+
  
  scale_y_continuous(labels = function(x) paste0(x, "$"))+
  
  labs(x="",y="", title = "Tesla Stock Performance FY23")+
  
  theme(legend.position = "bottom", legend.title =element_blank())+
  
  theme_minimal()+
  
  geom_vline(xintercept = as.Date("2023-01-25","%Y-%m-%d"),
             color = "violet", size = 1.2, alpha = 0.5)+
  ggplot2::annotate("text", x = as.Date("2023-01-22","%Y-%m-%d"), y = 185, 
           label = "1/25 Earnings Call")+
  theme(plot.margin = margin(3, 0, 3, 0, "cm"))
  
  






