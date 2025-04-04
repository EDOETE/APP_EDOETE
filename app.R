
#install.packages("rsconnect")
#install.packages("bs4Dash")
#install.packages("leaflet")

#depploement shiny
library(rsconnect)
rsconnect::deployApp()

# Run the application 
shinyApp(ui = ui, server = server)
