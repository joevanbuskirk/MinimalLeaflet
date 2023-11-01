library(tidyverse)
library(shinydashboard)
library(shiny)
library(leaflet)
library(sf)

header <- dashboardHeader(title = "Minimal Example - Leaflet", 
                          titleWidth = '1000')

Places_df <- data.frame(Name = c('Enmore Theatre', 'Taronga Zoo'),
                        lat = c(-33.989556, -33.8435473),
                        lon = c(151.1741389, 151.2413418)) 


body <- dashboardBody(
  tags$style(HTML(paste(
    '.dataTables_wrapper',
    '.dataTables_filter {float: left; padding-left: 50px;}',
    '.dataTables_wrapper',
    '.dataTables_filter input{width: 500px;}'))),
  
  fluidRow(
    leafletOutput(outputId = "map", 
                  height = 500))
)


Places <- Places_df$Name

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", 
             selected = TRUE, 
             startExpanded = TRUE, 
             tabName = "Map",
             selectInput(inputId = "Point",
                         label = h4("Variable"),
                         choices = Places)
    )
  )
)

server <- function(input, output, session) {
  ## Interactive Map ###########################################
  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      setView(lng = 151.2,
              lat = -33.9,
              zoom = 11) %>% 
      addProviderTiles(providers$CartoDB.Positron) %>%
      addMapPane("Maps", zIndex = 410) 
  })
  # 
  
  ## Observe the events on the sidebar to update content
  observe({
    
    Place <- Places_df %>% 
      dplyr::filter(Name == input$Point)
    
    leafletProxy(mapId = "map", 
                 data = Place) %>%
      clearGroup('Total') %>%
      clearControls() %>%
      leaflet::addMarkers(Place,
                          lat = ~lat,
                          lng = ~lon,
                          group = 'Total',
                          options = pathOptions(pane = "Maps"),
                          label = ~lapply(glue::glue(
                            .sep = "<br/>",
                            "<b>{Name}</b>"),
                            htmltools::HTML)) 
    
  })
}


shinyApp(ui = dashboardPage(header, sidebar, body), server = server)
