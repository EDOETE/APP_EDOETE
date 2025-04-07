server <- function(input, output, session) {
 
  
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 6.5, lat = 44.5, zoom = 10)
  })
}