library(shiny)
library(leaflet)
library(bs4Dash)

ui <- dashboardPage(
  header = dashboardHeader(
    title = dashboardBrand("Enquête Terminale Embrun", color = "primary")
  ),
  
  sidebar = dashboardSidebar(
    skin = "light",
    sidebarMenu(
      menuItem("Accueil", tabName = "accueil", icon = icon("home")),
      menuItem("Résultats globaux", tabName = "globaux", icon = icon("chart-bar")),
      menuItem("Par sous-groupes", tabName = "groupes", icon = icon("users")),
      menuItem("Cartographie", tabName = "carto", icon = icon("map"))
    )
  ),
  
  body = dashboardBody(
    tabItems(
      tabItem(tabName = "accueil", h2("Bienvenue sur l'application")),
      
      tabItem(tabName = "globaux", h2("Bienvenue sur globaux")),
      tabItem(tabName = "groupes", h2("Bienvenue sur groupes")),
      tabItem(tabName = "carto", h2("Bienvenue sur la carte"))
    )
  )
)
