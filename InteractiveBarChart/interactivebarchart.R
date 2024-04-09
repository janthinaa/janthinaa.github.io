
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

#Organise dataset 
medalcolour <- olympicmedalists %>% mutate(TypeofMedal = if_else(Rank==1, "Gold",        if_else(Rank==2,"Silver",
                                                                                                 if_else(Rank==3,"Bronze", as.character(Rank)))))

#Remove 0 values 
medalcolour <- medalcolour %>% filter(Rank>0)

#Group data by country and medal colour
medal_counts <- medalcolour %>% group_by(Country, TypeofMedal) %>% summarise(Count=n()) %>% arrange(desc(Count))

#Top 10 Countries by Medal Count 
toptenmedal_counts <- medal_counts %>% filter(Country %in% c("United States of America","Australia","Germany","Japan","United Kingdom","Hungary","Netherlands", "Canada","China", "France", "Sweden"))

toptenmedal_counts

#Create Shiny App 
ui <- fluidPage (
  titlePanel(""),
  mainPanel(highchartOutput("BarGraph"))
)

toptenmedal_counts$TypeofMedal <- factor(toptenmedal_counts$TypeofMedal, levels = c ("Gold","Silver", "Bronze"))

server <- function(input, output) {
  output$BarGraph <- renderHighchart({
    hchart(toptenmedal_counts, type = 'bar', hcaes(x = 'Country', y = 'Count', group = 'TypeofMedal', ),
           stacking = "normal"
    ) %>%
      hc_colors(c("#FFD700", "#C0C0C0","#CD7F32")) %>% 
      hc_title(text="Type of Medals won By Each Country") %>% 
      hc_subtitle(text = " Top 10 Countries by Medal Count") }) 
}

shinyApp (ui=ui, server = server)