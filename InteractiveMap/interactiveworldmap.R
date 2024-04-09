# Load required R packages
library(tidyverse)
library(highcharter) 

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

#Group Countries by number of medals 
medalspercountry <- olympicmedalists %>% group_by(Country) %>% summarise(Medals=n())

# Load the world Map data
data(worldgeojson, package = "highcharter")

ui <- fluidPage (
  titlePanel(""),
  mainPanel(highchartOutput("InteractiveMap"))
)

#Choose colours 
colors  <- c("#ffeda0","#9ecae1","#3182bd","#2c7fb8")

server <- function(input, output) {
  output$InteractiveMap <- renderHighchart({ highchart() %>%
      hc_add_series_map(worldgeojson, medalspercountry, value = "Medals", joinBy = c('name','Country'),
                        name = ""
      )  %>% 
      hc_colorAxis(minColor = "#a6bddb", maxColor="#2c7fb8", stops = color_stops(n=length(colors),colors=colors)) %>% 
      hc_chart(borderColor = "black") %>% 
      hc_title(text = "Oympic Swimming Medals by Country") %>% 
      hc_subtitle(text = "1912-2020")
  })
}

shinyApp (ui=ui, server = server)