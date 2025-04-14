library(shiny)
library(leaflet)
library(bs4Dash)

ui <- dashboardPage(
  header = dashboardHeader(
    title = tags$div(
        # Style CSS appliqué au  titre pour  centrer et la couleur de fond :
      style = "text-align: center; width: 100%; background-color: #007bff; color: white; padding: 10px;",
      "EDOETE"
    )
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
    /* Style carousel */
    .carousel-inner img {
      width: 100%;
      height: 400px;
      object-fit: cover;
    }

    /* Tous les onglets (actifs et inactifs) en gras */
    .nav-tabs > li > a {
      font-weight: bold;
      transition: background-color 0.3s;
    }

    /* Effet de survol */
    .nav-tabs > li > a:hover {
      background-color: #e9ecef; /* gris clair au survol */
      color: black;
    }
  "))
    )
    ,
    
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
              tabsetPanel( 
                tabPanel("COHORTE 2025",
                         
                         h3("Analyse des données"),p("Analyse des données"),
                         tabsetPanel(
                         tabPanel("avant bac"),
                         tabPanel("apres bac", h3("Analyse des données"), plotOutput("plot")),
                         tabPanel("tableaux de bord", h3("Infos"), p("graphs."))
                
                         
                         
                         
                         )),
                tabPanel("COHORTE 2026", h3("Analyse des données")),
                tabPanel("COHORTE 2027", h3("Analyse des données")),
                tabPanel("COHORTE 2028", h3("Analyse des données")),
                tabPanel("COHORTE 2029", h3("Analyse des données")),
                tabPanel("COHORTE 2030", h3("Analyse des données"))
               
                
              )
      ),
      
      tabItem(tabName = "questionnaire", h2("Bienvenue ")),
      tabItem(tabName = "documentations",)
    )
  )
)


