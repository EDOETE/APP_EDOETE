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
      menuItem("Enquête", tabName = "questionnaire", icon = icon("chart-bar")),
      menuItem("Cohortes", tabName = "generation", icon = icon("users")),
      menuItem("À propos", tabName = "documentations", icon = icon("info-circle"))
    )
  ),
  
  body = dashboardBody(
    tags$head(
      tags$style(HTML("
        .carousel-inner img {
          width: 100%;
          height: 400px;
          object-fit: cover;
        }
      "))
    ),
    
    tabItems(
      tabItem(tabName = "accueil",
              h2("Bienvenue sur l'application"),
              
              # Carousel Bootstrap
              tags$div(
                id = "myCarousel",
                class = "carousel slide",
                `data-ride` = "carousel",
                
                # Indicateurs (les petits points)
                tags$ol(class = "carousel-indicators",
                        tags$li(`data-target` = "#myCarousel", `data-slide-to` = "0", class = "active"),
                        tags$li(`data-target` = "#myCarousel", `data-slide-to` = "1"),
                        tags$li(`data-target` = "#myCarousel", `data-slide-to` = "2")
                ),
                
                # Contenu des slides
                tags$div(class = "carousel-inner",
                         tags$div(class = "carousel-item active",
                                  tags$img(src = "image1.png", class = "d-block w-100")),
                         tags$div(class = "carousel-item",
                                  tags$img(src = "https://argouges.ent.auvergnerhonealpes.fr/lectureFichiergw.do?ID_FICHIER=7633", class = "d-block w-100")),
                         tags$div(class = "carousel-item",
                                  tags$img(src = "https://th.bing.com/th/id/OIP.ONekcOof-Sd5AdoBBJp6iQHaEK?w=1920&h=1080&rs=1&pid=ImgDetMain.png", class = "d-block w-100"))
                ),
                
                # Contrôles gauche/droite
                tags$a(class = "carousel-control-prev", href = "#myCarousel", role = "button", `data-slide` = "prev",
                       tags$span(class = "carousel-control-prev-icon", `aria-hidden` = "true"),
                       tags$span(class = "sr-only", "Précédent")),
                
                tags$a(class = "carousel-control-next", href = "#myCarousel", role = "button", `data-slide` = "next",
                       tags$span(class = "carousel-control-next-icon", `aria-hidden` = "true"),
                       tags$span(class = "sr-only", "Suivant"))
              )
      ),
      
      tabItem(tabName = "generation",
              h2("Bienvenue "),
              tabsetPanel(
                tabPanel("Cohorte 2025", h3("Analyse des données"),p("Analyse des données"),
                         tabsetPanel(
                         tabPanel("avant bac"),
                         tabPanel("apres bac", h3("Analyse des données"), plotOutput("plot")),
                         tabPanel("tableau de bords", h3("Infos"), p("graphs."))
                
                         
                         
                         
                         )),
                tabPanel("Cohorte 2026", h3("Analyse des données"), plotOutput("plot"))
                
              )
      ),
      
      tabItem(tabName = "questionnaire", h2("Bienvenue ")),
      tabItem(tabName = "documentations", h2("Bienvenue sur la carte"))
    )
  )
)


