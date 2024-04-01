
# Load required R packages
library(tidyverse)

# Set highcharter options
options(highcharter.theme = hc_theme_smpl(tooltip = list(valueDecimals = 2)))

# Load Data 
olympicswimming <- read.csv("olympicswimming.csv")
#Filter out the medalists 
olympicmedalists <- olympicswimming %>% filter(Rank<4)

#load IOC code names
ioc_data<-read_csv("ioc_country_codes.csv")

ioc_data <- ioc_data[c("Country", "NOC")]

olympicmedalists$Country <- ioc_data$Country[match(olympicmedalists$Team, ioc_data$NOC)]

#Name countries whose IOC is not contained in IOC dataset 

# Find the row index where Team is "ROC"
roc_row_index <- which(olympicmedalists$Team == "ROC")

# Update the corresponding row in the "Country" column
olympicmedalists$Country[roc_row_index] <- "Russia"

# Find the row index where Team is "SGP"
roc_row_index <- which(olympicmedalists$Team == "SGP")

# Update the corresponding row in the "Country" column
olympicmedalists$Country[roc_row_index] <- "Singapore"

# Find the row index where Team is "ROU"
roc_row_index <- which(olympicmedalists$Team == "ROU")

# Update the corresponding row in the "Country" column
olympicmedalists$Country[roc_row_index] <- "Romania"

# Find the row index where Team is "USA"
roc_row_index <- which(olympicmedalists$Team == "USA")

# Update the corresponding row in the "Country" column
olympicmedalists$Country[roc_row_index] <- "United States of America"

#Identify Required Data 
speed <-olympicmedalists%>% filter(Rank==1) %>% group_by(Distance..in.meters.,Stroke,Gender)

#Standardise Timing Format 

library(hms)
times<- speed$Results
times1 <-strsplit(times,split="\\.")
times2 <-sapply(times1,function(x) x[1])
timessd <-sapply(times1,function(x) x[2])
timessd <-sapply(timessd,function(x) substr(x,0,3))
times3 <-strsplit(times2,split=":")
times3 <-sapply(times3,rev)
times3 <-lapply(times3, function(x) {
  while (length(x) < 3) {
    x <-c(x, "0")
  }
  x
})

weights <- c(1,60,3600)
secs <-sapply(times3, function(x) {
  temp=0
  for(y in 1:length(x)) {
    temp = temp + strtoi(weights[y] * strtoi(x[y]))
  }
  temp
})
secs <- secs + as.numeric(as.numeric(timessd)/1000)
library(dplyr)
timef <- secs %>% hms(seconds = .)

speed$Results <- timef
speed$Results <- secs
speedbywomensfreestyle <-speed %>% filter(Gender=="Women") %>% filter(Stroke == "Freestyle") %>% filter(Distance..in.meters. == "100m")

#Make App 

library(plotly)
library(shiny)

ui <- fluidPage(
  h1("Women 100m Freestyle Results "),
  plotlyOutput("graph")
)

server <- function(input, output) {
  output$graph <- renderPlotly({
    plot_ly(
      speedbywomensfreestyle,
      x = ~Year,
      y = ~Results,
      name = "data",
      type = "scatter",
      mode = "lines"
    )  %>%
      layout(hovermode = "x unified")
  })
}

shinyApp(ui, server)