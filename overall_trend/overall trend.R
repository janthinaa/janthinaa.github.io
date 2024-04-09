library(shiny)
library(plotly)
library(ggplot2)
library(highcharter)

# Load Data 
olympicswimming <- read.csv("olympicswimming.csv")
#Filter out the medalists 
olympicmedalists <- olympicswimming %>% filter(Rank<4)

#load IOC code names
ioc_data<-read.csv("ioc_country_codes.csv")

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

# Find the row index where Team is "SRB"
roc_row_index <- which(olympicmedalists$Team == "SRB")

# Update the corresponding row in the "Country" column
olympicmedalists$Country[roc_row_index] <- "Serbia"

# Find the row index where Team is "NlD"
roc_row_index <- which(olympicmedalists$Team == "NLD")

# Update the corresponding row in the "Country" column
olympicmedalists$Country[roc_row_index] <- "Netherlands"

# Find the row index where Team is "TTO"
roc_row_index <- which(olympicmedalists$Team == "TTO")

# Update the corresponding row in the "Country" column
olympicmedalists$Country[roc_row_index] <- "Trinidad and Tobago "


speed <-olympicmedalists%>% filter(Rank==1) %>% group_by(Distance..in.meters.,Stroke,Gender)
speed <- speed %>%
  mutate(EventType = paste(Gender,Distance..in.meters., Stroke, sep = " "))

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

speed$Results <- secs

rateofimprovement <- speed %>% group_by(Year, EventType) %>%
  summarise(mR = mean(Results, na.rm = T)) %>%
  group_by(EventType) %>%
  arrange(Year, .by_group = T) %>% 
  mutate(improv = mR - lag(mR)) %>%
  na.omit() 

speed_of_women <- subset(speed, !(EventType %in% c( "Women 1500m Freestyle", "Women 800m Freestyle", "Women 4x200 Freestyle")))

speed_of_women<-speed_of_women %>% filter(Gender == "Women")

speed_of_men <- subset(speed, !(EventType %in% c("Men 1500m Freestyle", "Men 400m Breaststroke", "Men 800m Freestyle","Men 4x200 Freestyle")))

speed_of_men<- speed_of_men %>% filter(Gender == "Men")

# Define UI for random distribution app ----
ui <- fluidPage(
  
  #Title ----
  titlePanel(""),
  
  # Main panel for displaying outputs ----
  mainPanel(
    
    # Tabset Panel ----
    tabsetPanel(type = "tabs",
                #100m Freestyle Panel 
                tabPanel("Women", plotOutput("hc1")),
                
                #200m Freestyle Panel 
                tabPanel("Men", plotOutput("hc2"))
    )
  )
)


# Define server logic for random distribution app ----
server <- function(input, output) {
  
  output$hc1 <- renderPlot ({
    ggplot(data=speed_of_women, aes(x=Year, y=Results, group=EventType, color=EventType)) + geom_point () + geom_smooth()
  })
  
  # Generate a summary of the data ----
  output$hc2 <- renderPlot ({
    ggplot(data=speed_of_men, aes(x=Year, y=Results, group=EventType, color=EventType)) + geom_point () + geom_smooth()
  })

 
  
}

# Create Shiny app ----
shinyApp(ui, server)

