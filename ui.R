
library(shiny)
library(bs4Dash)
library(rnaturalearthdata)



#UI ======================================================

ui <- dashboardPage(
  
  # En tete de l'application-------------------------------------------------------------------------------
  
  header = dashboardHeader(
    title = tags$div(
      style = "text-align: center; width: 100%; background-color: #003366; color: white; padding: 16px;",
      "Opale"
    )
  ),
  
  # MENU PRINCIPAL-------------------------------------------------------------------------------
  
  sidebar = dashboardSidebar(
    skin = "light",
    sidebarMenu(
      id = "sidebar", #id du menu util pour naviguer dans le menu permettant davoir des liens interne
      menuItem("Accueil", tabName = "accueil", icon = icon("home")),
      menuItem("Enquête", tabName = "questionnaire", icon = icon("chart-bar")),
      menuItem("Cohortes", tabName = "generation", icon = icon("users")),
      menuItem("À propos", tabName = "apropos", icon = icon("info-circle"))
    )
  ),
  
  # Corps de l'application-------------------------------------------------------------------------------
  
  body = dashboardBody(
    
    
    #-------------------------------------------------------------------------------
    #  HTML et CSS pour l'entete et le carousel
    tags$head(
      tags$style(HTML("
        .main-header {
          background-color: #003366 !important;
        }
        .main-header .logo {
          background-color: #003366 !important;
          color: white !important;
        }
        .main-header .navbar {
          background-color: #003366 !important;
        }

        /* Style carousel */
        .carousel-inner img {
          width: 100%;
          height: 400px;
          object-fit: cover;
        }

        /* Onglets en gras */
        .nav-tabs > li > a {
          font-weight: bold;
          transition: background-color 0.3s;
        }

        /* Survol des onglets */
        .nav-tabs > li > a:hover {
          background-color: #e9ecef;
          color: black;
        }
        
 /*HTML et CSs pour le contour des tableaux et plot*/
        .styled-box {
  border: 1px solid #ccc;
  border-radius: 6px;
  padding: 15px;
  margin-bottom: 20px;
  background-color: #fff;
  box-shadow: 2px 2px 6px rgba(0, 0, 0, 0.05);
        }
 /*HTML et CSs pour le contour des tableaux */
 table {
      border-collapse: collapse;
      width: 100%;
      font-family: 'Segoe UI', Tahoma, sans-serif;
      font-size: 14px;
    }
    th, td {
      border: 1px solid #e0e0e0;
      padding: 8px 12px;
      text-align: left;
    }
    th {
      background-color: #f5f5f5;
      font-weight: 600;
    }
    tr:nth-child(even) {
      background-color: #fafafa;
    }

 /* Footer simple et propre */
      .main-footer {
        position: relative;
        bottom: 0;
        width: 100%;
        background-color: #003366;
        color: white;
        text-align: center;
        padding: 10px;
        font-size: 13px;
        margin-top: 30px;
      }



.info-card {
  background-color: white;
  border-radius: 12px;
  padding: 20px;
  margin-bottom: 20px;
  box-shadow: 0 4px 10px rgba(0, 0, 0, 0.1);
  transition: all 0.3s ease-in-out;
  height: 100%;
  text-align: center;
  color: #333;
}
.info-card:hover {
  transform: translateY(-5px);
  box-shadow: 0 8px 20px rgba(0, 0, 0, 0.15);
  background-color: #f8f9fa;
}
.info-card h4 {
  margin-top: 10px;
  font-weight: 600;
}
.info-card p {
  font-size: 14px;
  color: #555;
}




      "))
    ),
    
    # Contenu des différentes pages (onglets)-------------------------------------------------------------------------------
    
    tabItems(
      
      # Page Accueil========================================
      tabItem(tabName = "accueil",
            
              
              # Carousel d'images
              tags$div(
                id = "myCarousel",
                class = "carousel slide",
                `data-ride` = "carousel",
                
                # Indicateurs 
                tags$ol(class = "carousel-indicators",
                        tags$li(`data-target` = "#myCarousel", `data-slide-to` = "0", class = "active"),
                        tags$li(`data-target` = "#myCarousel", `data-slide-to` = "1"),
                        tags$li(`data-target` = "#myCarousel", `data-slide-to` = "2")
                ),
                
                # images importer 
                tags$div(class = "carousel-inner",
                         
                    
                         
                         #tags$div(class = "carousel-item active",
                                 # tags$img(src = "image5.jpg", class = "d-block w-100")
                        # ),
                         
                         tags$div(class = "carousel-item active",
                                  tags$img(src = "https://www.arearegionsud.com/wp-content/uploads/2017/04/area-Re%CC%81gion-Sud-realisation-construction-Lycee-Honore-Romane-Embrun-N7.jpeg", class = "d-block w-100")),
                         tags$div(class = "carousel-item",
                                  tags$img(src = "https://www.ac-aix-marseille.fr/sites/ac_aix_marseille/files/2021-05/21-05-06---cit-scolaire-embrun-2-18233.jpg", class = "d-block w-100")),
                        
                         tags$div(class = "carousel-item",
                                  tags$img(src = "https://www.reseau-canope.fr/fileadmin/user_upload//Projets/Forum_des_ecologues/edition_2022/photos2/Honore-Romane-Embrun-2.jpg", class = "d-block w-100"))
                ),
                
                # Controle 
                tags$a(class = "carousel-control-prev", href = "#myCarousel", role = "button", `data-slide` = "prev",
                       tags$span(class = "carousel-control-prev-icon", `aria-hidden` = "true"),
                       tags$span(class = "sr-only", "Précédent")),
                
                tags$a(class = "carousel-control-next", href = "#myCarousel", role = "button", `data-slide` = "next",
                       tags$span(class = "carousel-control-next-icon", `aria-hidden` = "true"),
                       tags$span(class = "sr-only", "Suivant"))
              ),
              
              
              
              tags$div(
                style = "background-color: #003366; color: white; 
           display: flex; justify-content: space-between; align-items: center; padding: 20px;  font-family: Arial, sans-serif;",
                
                # message de bienvenue
                tags$div(
                  style = "max-width: 400px; text-align: left; padding-right: 20px; position: relative;",
                  tags$h2("Bienvenue sur l'application ",tags$div(
                    style = "text-align: center; font-weight: bold; font-size: 24px;",
                    "Opale"
                  )
                  , style = "margin-bottom: 5px;"),
                  tags$p(
                    style = "text-align: center;font-size: 14px; margin: 0;",
                    "Orientation et projet d'avenir des lycéen.e.s d'Embrun"
                  ),
                  
                  # separateur vertical 
                  tags$div(
                    style = "
        position: absolute;
        top: 15%;
        bottom: 15%;
        right: 0;
        width: 2px;
        background-color: white;
        opacity: 0.6;
      "
                  )
                ),
                
                
                tags$div(
                  style = "font-size: 20px; font-weight: bold; padding-left: 15px; text-align: right;",
                  actionLink("go2024", "Cohorte 2024", style = "margin: 0 10px; text-decoration: none;"),
                  "|",
                  actionLink("go2025", "Cohorte 2025", style = "margin: 0 10px; text-decoration: none;")
                 # "|",
                  #actionLink("go2026", "Cohorte 2026", style = "margin: 0 10px; text-decoration: none;")
                )
              )
              
              ,
              tags$div(
                style = "padding: 30px; max-width: 1200px; margin: auto;",
                
                fluidRow(
                  column(
                    width = 6,
                    tags$a(
                      href = "https://www.atrium-sud.fr/web/lcl-honore-romane-052021",  # Remplace par l'URL réelle
                      target = "_blank",
                      style = "text-decoration: none;",
                      tags$div(
                        class = "info-card",
                        tags$img(src = "lyce.png", height = "90px", style = "display: block; margin: auto;"),
                       
                        p("Logée dans un environnement haut-alpin au spectacle quotidien incomparable, la Cité scolaire HONORÉ ROMANE accueille tous les jours les collégiens (550 élèves), les lycéens et étudiants (510 élèves) avec un internat de 150 élèves....")
                      )
                    )
                  ),
                  column(
                    width = 6,
                    tags$a(
                      href = "https://www.lped.fr",  # Remplace par l'URL réelle
                      target = "_blank",
                      style = "text-decoration: none;",
                      tags$div(
                        class = "info-card",
                        tags$img(src = "lped.png", height = "110px", style = "display: block; margin: auto;"),
                       
                        p("Nous sommes : un Laboratoire de recherche et d’enseignement public pluridisciplinaire, placé sous la double tutelle de l’AMU et de l’IRD, qui a pour vocation d’étudier, de façon disciplinaire et interdisciplinaire, 
            les dynamiques des sociétés et des écosystèmes ainsi que leurs interactions, dans différentes parties du monde...")
                      )
                    )
                  ),
                 )),
                
                  tags$div(
                    style = "padding: 30px; max-width: 1200px; margin: auto;",
                    fluidRow(
                    column(
                      width = 6,
                      tags$a(
                        href = "https://recover.paca.hub.inrae.fr/",  # Remplace par l'URL réelle
                        target = "_blank",
                        style = "text-decoration: none;",
                        tags$div(
                          class = "info-card",
                          tags$img(src = "INRAE.png", height = "50px", style = "display: block; margin: auto;"),
                          h4("RECOVER"),
                          p("L'UMR RECOVER est une unité mixte INRAE Aix-Marseille université centrée sur le fonctionnement des écosystèmes et les risques naturels. Ses objectifs sont :...")
                        )
                      )
                    ),
                  column(
                    width = 6,
                    tags$a(
                      href = "https://ferme.yeswiki.net/masspop/?PresentaTion",  # Remplace par l'URL réelle
                      target = "_blank",
                      style = "text-decoration: none;",
                      tags$div(
                        class = "info-card",
                        tags$img(src = "masspop.png", height = "90px", style = "display: block; margin: auto;"),
                        h4("Master Mathématiques appliquées, statistique"),
                        p("L’objectif principal du parcours MASS POP est de former des spécialistes en statistique des populations maîtrisant les formalismes et les outils mathématiques et informatiques de gestion, représentation et modélisation des données ainsi que les différentes étapes d’une étude en analyse des populations..."),
                        
                        
                      )
                    )
                  )
                ))
              
              
              
      ),
      
      
      #Page Cohortes ========================================
      tabItem(tabName = "generation",
              #tags$hr(),
              tabsetPanel(id = "main_tabs",
                tabPanel("COHORTE 2024",value = "cohorte_2024",
                         h3(""),
                         p(""),
                         
    
                         tabsetPanel(
                           tabPanel("Terminale",
                                    h3(""),
                                    fluidRow(
                                      bs4ValueBoxOutput("vbox1",width = 3),# "width"contole la longueur  du vbox1
                                      bs4ValueBoxOutput("vbox2",width = 3),
                                      bs4ValueBoxOutput("vbox3",width = 3),
                                      bs4ValueBoxOutput("vbox4",width = 3)
                                    
                                    ),
                                    
                                   
                  
                                    
                                  
                                    bs4Accordion(
                                      id = "analyses_2024", 
                                      bs4AccordionItem(
                                        title = tagList(icon("user-graduate"), "1 – Caractéristiques des élèves"),
                                        status = "primary",
                                        solidHeader = TRUE,
          
                                        
                                        fluidRow(
                                          column(
                                            width = 4,
                                            selectInput("varName", "Choisissez une variable :", choices = c(
                                              "Sexe" = "sexe",
                                              "Pays de naissance" = "pays de naissance",
                                              "Lieu de residence" = "logé.e",
                                              "Type de terminale" = "type de terminale",
                                              "bourse sur critères sociaux" = "bourse au cours de votre scolarité"
                                            
                                            )),
                                            
                                            mainPanel(
                                              h4(""),
                                              tableOutput("stats")
                                             
                                              
                                              
                                            )
                                            
                                          ), p(" "),
                                          column(
                                            width = 8,
                                            div(class = "styled-box",
                                                
                                                plotlyOutput("descriptive")
                                                
                                                
                                            ))
                                          
                                         
                                          
                                          
                                          
                                        ),

                                        tags$hr(style = "border-top: 2px solid blue;"),
                                        
                                        fluidRow(
                                          column(
                                            width = 4,
                                            selectInput("var1", "Variable en ligne :", choices = c(
                                              "Sexe" = "sexe",
                                           
                                              "Lieu de residence" = "logé.e",
                                              "Type de terminale" = "type de terminale",
                                              "bourse sur critères sociaux" = "bourse au cours de votre scolarité"
                                            )),
                                            selectInput("var2", "Variable en colonne  :", choices = c(
                                              
                    
                                            
                                              "Lieu de residence" = "logé.e",
                                              "Type de terminale" = "type de terminale",
                                              "bourse sur critères sociaux" = "bourse au cours de votre scolarité"
                                            )
                                            ),
                                            mainPanel(
                                              h4(""),
                                              tableOutput("tableau_croise"),
                                              verbatimTextOutput("khi2_result"),
                                              
                                              
                                            ) ),
                                          column(
                                            width = 8,
                                            div(class = "styled-box",
                                                
                                                plotlyOutput("graphique")
                                            ))
                                        ), tags$hr(style = "border-top: 2px solid blue;"),
                                          
 #filtre  caracteristique des eleves                                     
sidebarLayout(
  sidebarPanel(
    
    selectInput("sexe", "Sexe :", 
                choices = c("Tous", unique(donnees$sexe)), selected = "Tous"),
    
    selectInput("terminale", "type de terminale :", 
                choices = c("Tous", unique(donnees$`type de terminale`)), selected = "Tous"),
    
    selectInput("loge", "Logé.e :", 
                choices = c("Tous", unique(donnees$`logé.e`)), selected = "Tous"),
    
    selectInput("bourse", "bourse au cours de votre scolarité :", 
                choices = c("Tous", unique(donnees$`bourse au cours de votre scolarité`)), selected = "Tous")
  ),
  
  mainPanel(
    h3("Nombre d'élèves correspondant aux critères :"),
    infoBoxOutput("iobox_count", width = 4),
    
    
    h3("Tableau des résultats :"),
    DTOutput("table")
  )
),
                                      ),
                                      bs4AccordionItem(
                                        title = tagList(icon("users"), "2 – Entourage familial"),
                                        status = "info",
                                        solidHeader = TRUE,
                                        
                                        fluidRow(
                                          column(
                                            width = 4,
                                            selectInput("var_Famille", "Choisissez une variable pour la première analyse :", choices = c(
                                              "Diplôme de la mère ou responsable légale" = "diplôme de la mère ou responsable légale?",
                                              "Statut d’activité du père / référent légal" = "situation professionnelle du père ou responsable légal",
                                              "Statut d’activité de la mère / représentante légale" = "situation professionnelle(mère ou responsable)",
                                              "Diplôme du pere ou responsable légale" = "diplôme du père ou responsable"
                                              
                                            ))
                                          ),
                                          column(
                                            width = 8,
                                            div(class = "styled-box",
                                                plotlyOutput("descriptive2")
                                            )
                                          ), tableOutput("stats_2")
                                        ),
                                        
                                         #filtre  entourage familial  
                                        tags$hr(),                                  
sidebarLayout(
  sidebarPanel(
    
    selectInput("sexe", "Sexe :", 
                choices = c("Tous", unique(donnees$sexe)), selected = "Tous"),
    
    selectInput("loge", "Logé.e :", 
                choices = c("Tous", unique(donnees$`logé.e`)), selected = "Tous"),
    
    selectInput("bourse", "bourse au cours de votre scolarité :", 
                choices = c("Tous", unique(donnees$`bourse au cours de votre scolarité`)), selected = "Tous"),
    
    selectInput("statut d’activité du père / référent légal", "situation professionnelle du père ou responsable légal :", 
                choices = c("Tous", unique(donnees$`situation professionnelle du père ou responsable légal`)), selected = "Tous"),
    
    selectInput("Statut d’activité de la mère / représentante légale", "situation professionnelle(mère ou responsable :)", 
                choices = c("Tous", unique(donnees$`situation professionnelle(mère ou responsable)`)), selected = "Tous")
  ),
  
  mainPanel(
    h3("Nombre d'élèves correspondant aux critères :"),
    infoBoxOutput("iobox_count_familial", width = 4),
    
    
    h3("Tableau des résultats :"),
    DTOutput("table_familial")
  )
),
                                       
                                      ),  




                                      bs4AccordionItem(
                                        title = tagList(icon("futbol"), "3 – Emploi, loisirs"),
                                        status = "success",
                                        solidHeader = TRUE,
                                        fluidRow(
                                          column(
                                            width = 4,
                                            selectInput("var_emp_lois", "Choisissez une variable pour la première analyse :", choices = c(
                                              "emploi rémunéré" = "Avez-vous déjà eu un emploi rémunéré ?",
                                              "Répartition des élèves selon qu’ils, elles fréquentent ou non une bibliothèque municipale" = "inscrit ou fréquentez-vous une bibliothèque municipale ?",
                                              "Répartition des élèves qui ont eu une activité rémunérée selon le secteur d’activité" = "activité rémunérée selon le secteur",
                                              "Répartition des élèves selon le statut d’activité de la mère / représentante légale" = "diplôme du père ou responsable"
                                              
                                            ))
                                          ),
                                          column(
                                            width = 8,
                                            div(class = "styled-box",
                                                plotlyOutput("descriptive3")
                                            )
                                          )
                                        )
                                      ),
                                      bs4AccordionItem(
                                        title = tagList(icon("compass"), "4 – Orientation scolaire – Diplôme souhaité"),
                                        status = "warning",
                                        solidHeader = TRUE,
                                        
                                        column(
                                          width = 12,
                                          div(class = "styled-box",
                                              p("Dans quelle(s) ville(s) aimeriez-vous poursuivre vos études après votre Bac ?"),
                                              leafletOutput("carte", width = "100%", height = "78vh")
                                          )
                                        ),
                                        
                                        p(" "),
                                        fluidRow(
                                          column(
                                            width = 4,
                                            selectInput("var_Orientation", "Choisissez une variable pour la première analyse :", choices = c(
                                              "Suivre la même formation(pere,mere,frères et ou sœurs,famille proche)" = "suivre la même formation(pere,mere,frères et ou sœurs,famille proche)",
                                              "Répartition des élèves selon le niveau de diplôme souhaité" = "diplôme souhaiter"
                         
                                              
                                            ))
                                          ),
                                          column(
                                            width = 8,
                                            div(class = "styled-box",
                                                plotlyOutput("descriptive4")
                                            )
                                          )
                                        )
                                        
                                      )
                                      
                                    ),
                                    
                                    
                           ),
                            


                         
                           
                         
                           
                           
                           
                           tabPanel("Post bac", h5("Contenu à venir")),
                           tabPanel("Etude comparative", h5("Contenu à venir"))
                         )
                ),
                
                tabPanel("COHORTE 2025",value = "cohorte_2025",
                         h3(""),
                         p(""),
                         tabsetPanel(
                           tabPanel("Terminale",
                                    
                                    
                                    
                                    h3(""),
                                    fluidRow(
                                      bs4ValueBoxOutput("vbox_2025_1",width = 3),# "width"contole la longueur  du vbox1
                                      bs4ValueBoxOutput("vbox_2025_2",width = 3),
                                      bs4ValueBoxOutput("vbox_2025_3",width = 3),
                                      bs4ValueBoxOutput("vbox_2025_4",width = 3)
                                      
                                    ),#fin fluidRow
                                    
                                    bs4Accordion(
                                      id = "analyses_2025", 
                                      bs4AccordionItem(
                                        title = tagList(icon("user-graduate"), "1 – Caractéristiques des élèves"),
                                        status = "primary",
                                        solidHeader = TRUE,
                                        
                                        
                                        fluidRow(
                                          column(
                                            width = 4,
                                            selectInput("varName_2025", "Choisissez une variable :", choices = c(
                                              "Sexe" = "sexe",
                                              "Lieu de naissance" = "Où êtes-vous né.e ?",
                                              "Lieu de résidence" = "Etes-vous en internat au lycée cette année scolaire ?",
                                              "Type de terminale" = "Dans quel type de terminale êtes-vous inscrit.e ?",
                                              "bourse sur critères sociaux" = "Avez‐vous bénéficié d’une bourse au cours de votre scolarité au lycée ?"
                                              
                                            ))
                                            
                                          ), h4(" "),
                                          column(
                                            width = 8,
                                            div(class = "styled-box",
                                                
                                                plotlyOutput("descriptive_2025")
         
                                                
                                            ),
                                            tableOutput("stats_2025")
                                            )
                                          
                                          
                                          
                                          
                                          
                                        ),
                                        
                                        
                                        tags$hr(),
                                        
                                        
                                        fluidRow(
                                          column(
                                            width = 4,
                                            selectInput("var1_2025", "Variable en ligne :", choices = c(
                                              "Sexe" = "sexe",
                                              "Lieu de naissance" = "Où êtes-vous né.e ?",
                                              "Lieu de résidence" = "Etes-vous en internat au lycée cette année scolaire ?",
                                              "Type de terminale" = "Dans quel type de terminale êtes-vous inscrit.e ?",
                                              "Bourse sur critères sociaux" = "Avez‐vous bénéficié d’une bourse au cours de votre scolarité au lycée ?"
                                              
                                            )),
                                            selectInput("var2_2025", "Variable en colonne  :", choices = c(
                                             "Type de terminale" = "Dans quel type de terminale êtes-vous inscrit.e ?",
                                              "Lieu de résidence" = "Etes-vous en internat au lycée cette année scolaire ?",
                                              "Lieu de naissance" = "Où êtes-vous né.e ?",
                                              "Bourse sur critères sociaux" = "Avez‐vous bénéficié d’une bourse au cours de votre scolarité au lycée ?"
                                              
                                            )
                                            ),
                                            mainPanel(
                                              h4(""),

                                              
                                              
                                            ) ),
                                          column(
                                            width = 8,
                                            div(class = "styled-box",
                                                
                                                plotlyOutput("graphique_2025")
                                            ),
                                            
                                            tableOutput("tableau_croise_2025"),
                                          
                                            )
                                        ),
                                        
                                        tags$hr(),
                                        #filtre  caracteristique des éleves                                     
                                        sidebarLayout(
                                          sidebarPanel(
                                            
                                            selectInput("sexes", "Sexe :", 
                                                        choices = c("Tous", unique(donnees_2025$sexe)), selected = "Tous"),
                                            
                                            selectInput("Terminales", "Type de terminale :", 
                                                        choices = c("Tous", unique(donnees_2025$`Dans quel type de terminale êtes-vous inscrit.e ?`)), selected = "Tous"),
                                            
                                            selectInput("loges", "résidence :", 
                                                        choices = c("Tous", unique(donnees_2025$`Etes-vous en internat au lycée cette année scolaire ?`)), selected = "Tous"),
                                            
                                            selectInput("bourses", "Bourse sur critères sociaux au cours de votre scolarité au lycée :", 
                                                        choices = c("Tous", unique(donnees_2025$`Avez‐vous bénéficié d’une bourse au cours de votre scolarité au lycée ?`)), selected = "Tous")
                                          ),
                                          
                                          mainPanel(
                                            h3("Nombre d'élèves correspondant aux critères :"),
                                            infoBoxOutput("iobox_count_2025", width = 4),
                                            
                                            
                                            h3("Tableau des résultats :"),
                                            DTOutput("table_2025")
                                          )
                                        ),
                                        
                                        
                                        ),
                                      
                                      bs4AccordionItem(
                                        title = tagList(icon("users"), "2 – Entourage familial"),
                                        status = "info",
                                        solidHeader = TRUE,
                                        
                                        fluidRow(
                                         
                                            width = 4,
                                            selectInput("var_Famille_2025", "Choisissez une variable :", choices = c(
                                              "diplôme de la mère ou responsable légale?  " = "Quel est le diplôme de la mère ou responsable légale?",
                                              "situation professionnelle du père ou responsable légal  " = "Quel est la situation professionnelle du père ou responsable légal",
                                    
                                              "Métier père / Catégorie" = "Métier père / Catégorie",
                                              "Métier mère / Catégorie" = "Métier mère / Catégorie",
                                              "situation professionnelle de votre mère ou responsable légale" = "Quelle est la situation professionnelle de votre mère ou responsable légale?",
                                              "Avez-vous des frères, des soeurs, des demi-frères, des demi-soeurs ? " = "Avez-vous des frères, des soeurs, des demi-frères, des demi-soeurs ?",
                                              "Dans quelle commune réside votre mère ou responsable légale ?" = "Dans quelle commune réside votre mère ou responsable légale ?",
                                              "Dans quelle commune réside votre père ou responsable légal ?" = "Dans quelle commune réside votre père ou responsable légal ?"
                                            ))
                                            
                                          ,
                                          column(
                                            width = 8,
                                            div(class = "styled-box",
                                                plotlyOutput("descriptive22")
                                            )
                                          ), tableOutput("stats_22")
                                        ),
                                        
                                        #filtre  entourage familial  2025
                                        tags$hr(),                                  
                                        sidebarLayout(
                                          sidebarPanel(
                                            
                                            selectInput("sexes", "Sexe :", 
                                                        choices = c("Tous", unique(donnees_2025$sexe)), selected = "Tous"),
                                            
                                            selectInput("Terminales", "Type de terminale :", 
                                                        choices = c("Tous", unique(donnees_2025$`Dans quel type de terminale êtes-vous inscrit.e ?`)), selected = "Tous"),
                                            
                                            selectInput("loges", "résidence :", 
                                                        choices = c("Tous", unique(donnees_2025$`Etes-vous en internat au lycée cette année scolaire ?`)), selected = "Tous"),
                                            
                                            selectInput("bourses", "Bourse sur critères sociaux au cours de votre scolarité au lycée :", 
                                                        choices = c("Tous", unique(donnees_2025$`Avez‐vous bénéficié d’une bourse au cours de votre scolarité au lycée ?`)), selected = "Tous"),
                                         
                                            
                                            selectInput("statut d’activité du père / référent légal", "situation professionnelle du père ou responsable légal :", 
                                                        choices = c("Tous", unique(donnees_2025$`Quel est la situation professionnelle du père ou responsable légal`)), selected = "Tous"),
                                            
                                            selectInput("Statut d’activité de la mère / représentante légale", "situation professionnelle(mère ou responsable :)", 
                                                        choices = c("Tous", unique(donnees_2025$`Quelle est la situation professionnelle de votre mère ou responsable légale?`)), selected = "Tous")
                                          ),
                                          
                                          mainPanel(
                                            h3("Nombre d'élèves correspondant aux critères :"),
                                            infoBoxOutput("iobox_count_familial_2025", width = 4),
                                            
                                            
                                            h3("Tableau des résultats :"),
                                            DTOutput("table_familial_2025")
                                          )
                                        ),
                                        
                                      ),
                                        
                                        
                                                                            
                                       
                                        
                                        
                                        
                                      
                                      bs4AccordionItem(
                                        title = tagList(icon("futbol"), "3 – Emploi, loisirs"),
                                        status = "success",
                                        solidHeader = TRUE,),
                                      
                                      bs4AccordionItem(
                                        title = tagList(icon("compass"), "4 – Orientation scolaire – Diplôme souhaité"),
                                        status = "warning",
                                        solidHeader = TRUE,
                                        
                                        column(
                                          width = 12,
                                          div(class = "styled-box",
                                              p("Dans quelle(s) ville(s) aimeriez-vous poursuivre vos études après votre Bac ?"),
                                              leafletOutput("carte_2025", width = "100%", height = "78vh")
                                          )
                                        ),
                                        fluidRow(
                                          column(
                                            width = 4,
                                            selectInput("var_Orientation_2025", "Choisissez une variable :", choices = c(
                                              "Souhait de poursuite d'études" = "Souhaitez-vous poursuivre vos études après votre baccalauréat ?",
                                              "Niveau de diplôme souhaitez-vous obtenir ?" = "Quel niveau de diplôme souhaitez-vous obtenir?",
                                              "Avez-vous eu toutes les informations pour bien vous orienter après le bac ?" = "Avez-vous eu toutes les informations pour bien vous orienter après le bac ?",
                                              "Nombre de vœux formulés sur Parcoursup " = "Combien de vœux avez-vous formulés sur Parcoursup ?",
                                              "Vœu de préférence" = "Quel est votre vœu de préférence ?",
                                              "Pensez-vous suivre la même formation/orientation que l’un de vos proches (parents, frères, sœurs)?" = "Pensez-vous suivre la même formation/orientation que l’un de vos proches ?",
                                             "les critères qui ont le plus compté pour le choix de votre formation après le bac ?" ="Quels ont été les critères qui ont le plus compté pour le choix de votre formation après le bac ?"
                                            ))
                                            
                                          ), h4(" "),
                                          column(
                                            width = 8,
                                            div(class = "styled-box",
                                                
                                                plotlyOutput("descriptive4_2025")
                                                
                                                
                                            ),
                                            tableOutput("stats_2025_orientation")
                                          )
                                          
                                          
                                          
                                          
                                          
                                        ),
                                        
                                        
                                        
                                        ))
                                        
                                        
                                        
                                        
                                
                                    
                                    ),#fin tabPanel terminale
                           
                           
                          
                           
                           tabPanel("Post bac",h5("Contenu à venir")),
                           tabPanel("Etude comparative", h5("Contenu à venir"))
                         
                         
                         )
                )
                         
                #tabPanel("COHORTE 2026", h3("Analyse des données")),
                #tabPanel("COHORTE 2027", h3("Analyse des données")),
                #tabPanel("COHORTE 2028", h3("Analyse des données")),
                #tabPanel("COHORTE 2029", h3("Analyse des données")),
                #tabPanel("COHORTE 2030", h3("Analyse des données"))
              )
      ),
      
# Page Enquête ========================================
tabItem(tabName = "questionnaire",
        
        fluidRow(
          column(width = 3,
                 strong("Contexte :"),
                 p(" Ce projet s'inscrit dans le cadre d'une enquête menée au sein de la Cité scolaire d’Embrun auprès des élèves de Terminale, dans le but de mieux comprendre leurs aspirations futures et leur environnement socioculturel.")
          ),
          column(width = 3,
                 strong("Objectifs :"),
                 p("Le questionnaire vise à recueillir des données sur les caractéristiques individuelles des élèves, leur environnement familial et résidentiel, leurs choix d'orientation, leurs projets post-bac, ainsi que leurs expériences extrascolaires (stages, voyages, activités culturelles, etc.).")
          ),
          column(width = 3,
                 strong("Méthodologie :"),
                 p("Le questionnaire, structuré autour de neuf sections thématiques intégrant une variété de types de questions, a été administré en ligne via la plateforme KoboToolbox.")),
        
          column(
            width = 3,
            strong("Outil de collecte (KoboToolbox): "),
            p(
              "KoboToolbox est un outil de collecte de données conçu pour les environnements humanitaires, de développement, et de recherche. Il est utilisé par un large éventail d'organisations, y compris des agences des Nations Unies, des ONG, des instituts de recherche, et des gouvernements. ",
              a("Lien vers KoboToolbox", href = "https://www.kobotoolbox.org", target = "_blank")
            )
          )
          
          
        ),
        
        tags$hr(),
        
        h4("Accès au formulaire d’enquête"),
        
        # QR Code (le fichier doit être dans le dossier www/)
        img(src = "qr_kobo.png", height = "200px"),
        br(), br(),
        
        # Lien cliquable
        tags$a(
          href = "https://koboformng.osupytheas.fr/#/forms/aDyfTEdTEA7jE99rbstvDE",
          target = "_blank",
          "Cliquez ici pour accéder directement au formulaire",
          style = "font-size:16px; color:#007bff;"
        ),
       tags$hr(),
        # Ligne 3 : Quelques chiffres 
        fluidRow(
          column(
            width = 12, md = 8,
            h4("Quelques chiffres"),
            
              fluidRow(
                bs4InfoBoxOutput("ibox1", width = 5),
                bs4InfoBoxOutput("ibox2", width = 3)
                
              ),
             
            
            
          )
        
        )
        
        


              
      ),
      
      #Page À propos========================================
      tabItem(tabName = "apropos",
              fluidRow(
                column(
                  width = 10, offset = 1,
                  tags$div(
                    style = "padding: 20px; background-color: #f9f9f9; border-radius: 12px; box-shadow: 0px 0px 8px rgba(0,0,0,0.05);",
                    
                    tags$h2(icon("info-circle"), " À propos de l’application", style = "color: #2c3e50;"),
                    tags$hr(),
                    
                    tags$p(
                      "Cette application a été développée par ",
                      tags$strong("Ablaye Sow Sidibé"),
                      " ",
                      tags$a(href = "mailto:ablayesowsidibe@gmail.com", "📧 ablayesowsidibe@gmail.com")
                    ),
                    
                    tags$p(
                      "Elle a été réalisée dans le cadre d’un stage de Master 2 MASS POP (Mathématiques Appliquées aux Sciences Sociales – Parcours Population) à l’Université d’Aix-Marseille. Le stage s’est déroulé au sein des unités mixtes de recherche ",
                      tags$em("RECOVER (UMR AMU–INRAE)"),
                      " et ",
                      tags$em("LPED (UMR AMU–IRD)"),
                      "."
                    ),
                    
                    tags$h3("Le projet Opale"),
                    tags$p(
                      "L’application s’inscrit dans le cadre du projet ",
                      tags$strong("Opale"),
                      " (Enquête sur (Orientation et projet d'avenir des lycéen.e.s d'Embrun).), lancé en 2024 à l’initiative de l’équipe éducative, de la psychologue scolaire, et de chercheur·e·s de l’INED, de l’IRD et de l’Université d’Aix-Marseille."
                    ),
                    
                    tags$p(
                      "Ce projet vise à mieux comprendre les choix d’orientation post-bac et les trajectoires scolaires des lycéen·ne·s, en lien avec leur parcours personnel, leur environnement familial et leur territoire de vie. Une première enquête a été menée en juin 2024 auprès de 61 élèves volontaires (Cohorte 1), et une seconde vague est prévue en 2025."
                    ),
                    
                    tags$h3("Objectifs de l’application"),
                    tags$p("L’application a pour but de rendre accessibles et compréhensibles les résultats de l’enquête aux différents publics concernés :"),
                    tags$ul(
                      tags$li("élèves"),
                      tags$li("enseignant·e·s"),
                      tags$li("chercheur·e·s"),
                      tags$li("décideur·e·s")
                    ),
                    tags$p(
                      "Elle soutient les réflexions locales sur l’orientation, les aspirations des jeunes, ainsi que les dynamiques sociales et territoriales dans le contexte éducatif rural."
                    ),
                    
                    tags$h3("Fonctionnalités"),
                    tags$p("L’interface interactive permet :"),
                    tags$ul(
                      tags$li("une exploration des résultats par filtres (sexe, filière, bourse, etc.)"),
                      tags$li("l’affichage de graphiques dynamiques"),
                      tags$li("des tableaux de données"),
                      
                      
                        
                      ),
                      tags$h4("Partenaires du projet"),
                      tags$ul(
                        tags$li("Université d’Aix-Marseille – Master  Mathématiques Appliquées et Sciences Sociales : Analyse des Populations (MASS-POP)"),
                        tags$li("Laboratoire Population Environnement Développement (LPED)"),
                        tags$li("Risques, écosystèmes, vulnérabilité, environnement, résilience (RECOVER)"),
                        tags$li("Lycée Honoré Romane, Embrun"),
                    )
                  )
                )
              )
              
              ,
              
              )
    ),tags$footer(
      style = "background-color: #003366; padding: 20px; margin-top: 30px; border-top: 1px solid rgba(255, 255, 255, 0.3); color: white;",
      fluidRow(
        column(
          width = 4,
          p("À propos", style = "color: white;"),
          p("Orientation et projet d'avenir des lycéen.e.s d'Embrun", style = "color: white;"),
          h4("Partenaires du projet ", style = "color: white;"),
          p("Laboratoire Population Environnement Développement (LPED),Risques, écosystèmes, vulnérabilité, environnement, résilience (RECOVER), Lycée Honoré Romane(Embrun), Master Mathématiques Appliquées et Sciences Sociales : Analyse des Populations (MASS-POP)", style = "color: white;"),
          
          
         
        ),
        column(
          width = 4,
          p("Contact:"),
          tags$ul(
           
            tags$li("Bénédicte Gastineau, LPED)", style = "color: white;"),
            ),
          tags$ul(
            tags$a(href = "mailto:benedicte.gastineau@univ-amu.fr", "📧benedicte.gastineau@univ-amu.fr"),
            
          ),
         
          
        ) ,
        
        column(
          width = 4,
          p("Liens utiles", style = "color: white;"),
          
          h6("2024", style = "color: #ffffff; margin-top: 10px;"),
          #tags$ul(
           # style = "list-style: none; padding-left: 0;",   
           # tags$li(a(href = "Poster_2024.pdf", "Poster_Resultats_Opale", target = "_blank", style = "color: #66ccff; text-decoration: none;")),
            #tags$li(a(href = "TER_Embrun.pdf", "TER_Embrun", target = "_blank", style = "color: #66ccff; text-decoration: none;")),
            #tags$li(a(href = "Note_Resultats_Opale.pdf", "Note_Resultats_Opale", target = "_blank", style = "color: #66ccff; text-decoration: none;")),
            #tags$li(a(href = "RAPPORT_PROJET_EMBRUN.pdf", "RAPPORT_PROJET_EMBRUN", target = "_blank", style = "color: #66ccff; text-decoration: none;"))
          #),
          
          h6("2025", style = "color: #ffffff; margin-top: 10px;"),
          #tags$ul(
           # style = "list-style: none; padding-left: 0;",   
            #tags$li(a(href = "Présentation.pdf", "Présentation H. Romane mai 2025 -20 mai", target = "_blank", style = "color: #66ccff; text-decoration: none;"))
           
         # )
        )
        
      )
    )


  )
  
 
  
)

