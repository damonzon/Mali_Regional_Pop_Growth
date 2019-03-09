library(shinydashboard)
library(rgdal)
library(leaflet)
library(dplyr)
library(DT)

data2 <- read.csv("mali_pop_growth.csv")
admin1 <- readOGR("MLI_adm/MLI_adm1.shp")
bins <- c(35,40,50,55,60,80)
pal <- colorBin("RdYlBu", 
      domain = data2$growth_rate_percent, bins = bins)

ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Pop Growth by Region"),
  dashboardSidebar(
  h2("This is a Shiny rendition of a choropleth map 
     created in R with the Leaflet package."),
  h2("The regional color codes reprsent population
      growth rate pourcentages from 1998-2009."),
  h2("Bamako and Kidal have the highest rates.")
   ),
  
  dashboardBody(
    fluidRow( box(width = 12,
                  leafletOutput("mymap"))),
       h3("Data Source: https://en.wikipedia.org/wiki/Mali"),
       fluidRow( box(width = 12,
                  dataTableOutput("summary_table")))
    )
)

server <- function(input, output) {
  data_input <- data2
  labels <- paste("<p>", data2$region,"</p>",
    "<p>", "Pop_1998: ",data2$pop_1998,"</p>",     
    "<p>", "Pop_2009: ",data2$pop_2009,"</p>",
    "<p>", "Pop Growth Rate: ", 
      data2$growth_rate_percent, "%", "</p>")  
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      setView(lng = -4, lat = 17, zoom = 4) %>%
      addProviderTiles(providers$Stamen.Toner) %>%
      addPolygons(data = admin1,
                  fillColor = pal(data2$growth_rate_percent),
                  weight = 1,
                  smoothFactor = 1, 
                  color = "black",
                  fillOpacity = 0.8,
                  highlight = highlightOptions(
                    weight = 5,
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                label = lapply(labels, HTML)) %>%
                addLegend(pal = pal, 
                values = data2$growth_rate_percent, 
                opacity = 0.7, 
                title = NULL,
                position = "bottomleft")
  })
  
  output$summary_table = DT::renderDataTable(
    data2, options = list(lengthChange = FALSE)
  )
  
}

shinyApp(ui, server)
