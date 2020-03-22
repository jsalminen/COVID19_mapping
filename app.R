#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(dplyr)
library(stringr)

LAST_TESTDATA <- strptime("2020-03-22 08:43:23", "%Y-%m-%d %H:%M:%S",
                          tz = "Europe/Helsinki")

# Read data from Google Sheets
url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vSCKudWJa8n5kQHvbEc2Ew17HWr1fco6awwWoIeRz7NrRkDA_A3__UGN3GP13Rb1GvF5x2hx1p1JZ4V/pub?output=csv"

data <- readr::read_csv(url)
data$Kaupunki <- str_to_title(data$Kaupunki)
data$Timestamp <- strptime(data$Timestamp, "%d/%m/%Y %H:%M:%S", 
                           tz = "Europe/Helsinki") 

# Filter out test data
# data <- data %>% filter(Timestamp > LAST_TESTDATA)

# Read post-codes
postcodes <- readr::read_csv2("postcode_sample.csv")
cities <- readr::read_tsv("kunnat.tsv")

cities <- tidyr::separate(cities, Koordinaatit, c("lat", "lng"), sep = ", ")
cities$Kunta <- str_to_title(cities$Kunta)
cities$lat <- as.numeric(gsub("°N", "", cities$lat))
cities$lng <- as.numeric(gsub("°E", "", cities$lng))

# Summarise data
data_agg <- data %>% 
    group_by(Kaupunki) %>% 
    tally()

data_agg$label <- paste0("Tapauksia: ", data_agg$n)

# Join data
data_agg <- data_agg %>% 
    dplyr::left_join(cities, by = c("Kaupunki" = "Kunta"))


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("COVID19 Suomessa"),

    # Show a plot of the generated distribution
    mainPanel(
        leafletOutput(outputId = "COVIDmap"), 
        tableOutput("table_agg")
    
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$COVIDmap <- renderLeaflet({
        leaflet(data_agg) %>% 
            setView(lng = 27, lat = 65, zoom = 4.4)  %>% 
            #setting the view over ~ center of North America
            addTiles() %>% 
            addCircles(data = data_agg, 
                       lng = ~ lng, 
                       lat = ~ lat,
                       weight = 1, radius = ~sqrt(n) * 1000, 
                       label = ~label, 
                       color = "red", fillOpacity = 0.5)
    })
    
    output$table_agg <- renderTable(data_agg %>% 
                                        select(Kaupunki, n) %>% 
                                        rename(Tapauksia = n))
}

# Run the application 
shinyApp(ui = ui, server = server)
