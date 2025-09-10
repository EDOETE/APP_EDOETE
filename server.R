#install.packages("rnaturalearthdata")
library(shiny)
library(dplyr)
library(ggplot2)
library(bs4Dash)
library(leaflet)
library(sf)
library(plotly)
library(rnaturalearth)

server <- function(input, output, session) {
  # Valeur box 1
  output$vbox1 <- renderbs4ValueBox({
    bs4ValueBox(
      value = HTML("<span style='font-size: 2rem;'>71%</span>"),
      subtitle = "de filles",
      icon = icon("venus", style = "font-size: 70px;"),
      elevation = 2,
      width = 3,
      color = "lightblue"
    )
  })
  
  # Valeur box 2
  output$vbox2 <- renderbs4ValueBox({
    bs4ValueBox(
      value = HTML("<span style='font-size: 2rem;'>64%</span>"),
      subtitle = HTML("hors internat"),
      icon = icon("house-user", style = "font-size: 70px;"),
      elevation = 2,
      width = 3,
      color = "maroon"
    )
  })
  
  # Valeur box 3
  output$vbox3 <- renderbs4ValueBox({
    bs4ValueBox(
      value = HTML("<span style='font-size: 2rem;'>71%</span>"),
      subtitle = "en terminale générale",
      icon = icon("chalkboard-teacher", style = "font-size: 70px;"),
      elevation = 2,
      width = 3,
      color = "purple"
    )
  })
  
  # Valeur box 4
  output$vbox4 <- renderbs4ValueBox({
    bs4ValueBox(
      value = HTML("<span style='font-size: 2rem;'>61%</span>"),
      subtitle = HTML("non boursier·es"),
      icon = icon("money-bill-wave", style = "font-size: 70px;"),
      elevation = 2,
      width = 3,
      color = "maroon"
    )
  })
  
  # GRAPHIQUE DESCRIPTIF
  output$descriptive <- renderPlotly({
    var <- donnees[[input$varName]]
    if (is.null(var) || is.numeric(var)) return(NULL)  # On ignore les variables numériques
    
    freq <- prop.table(table(var)) * 100
    freq <- round(freq, 1)
    freq <- sort(freq, decreasing = FALSE)  # Tris croisser 
    
    freq_labels <- paste(freq, "%", sep = "")
    
    plot_ly(
      x = names(freq),
      y = as.numeric(freq),
      type = "bar",
      text = freq_labels,
      hoverinfo = "text",
      textposition = "none",
      marker = list(color = 'blue')
    ) %>%
      layout(
        title = paste("Répartition (%) de", input$varName),
        xaxis = list(
          title = input$varName,
          categoryorder = "array",
          categoryarray = names(freq),  # Respecte l'ordre trié
          showgrid = FALSE
        ),
        yaxis = list(title = "Pourcentage", showgrid = FALSE),
        dragmode = FALSE
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  
  
  # STATISTIQUES
  output$stats <- renderTable({
    
      stats <- donnees %>%
        group_by(.data[[input$varName]]) %>%
        summarise(
          Effectif = n(),
          Pourcentage = round(100 * n() / nrow(donnees), 1),
          .groups = "drop"
        )
    
    stats
  })
  
  # TABLEAU CROISÉ
  output$tableau_croise <- renderTable({
    req(input$var1, input$var2)
    tab <- table(donnees[[input$var1]], donnees[[input$var2]])
    total <- sum(tab)
    formatted <- matrix(paste0(tab),
                        nrow = nrow(tab), ncol = ncol(tab),
                        dimnames = dimnames(tab))
    
    col_totals <- colSums(tab)
    formatted <- rbind(formatted, Total = paste0(col_totals))
    row_totals <- rowSums(tab)
    formatted <- cbind(formatted, Total = c(paste0(row_totals), paste0(total)))
    
    as.data.frame.matrix(formatted)
  }, rownames = TRUE)
  
  khi2_test <- reactive({
    req(input$var1, input$var2)
    tab <- table(donnees[[input$var1]], donnees[[input$var2]])
    chisq.test(tab)
  })
  output$khi2_result <- renderPrint({
    khi2_test()
  })
  


  
  # GRAPHIQUE CROISÉ
  output$graphique <- renderPlotly({
    req(input$var1, input$var2)
    
    test <- khi2_test()
    is_significant <- test$p.value <= 0.05
    
    df <- donnees %>%
      filter(!is.na(.data[[input$var1]]), !is.na(.data[[input$var2]])) %>%
      count(.data[[input$var1]], .data[[input$var2]]) %>%
      group_by(.data[[input$var1]]) %>%
      mutate(pct = round(100 * n / sum(n), 1)) %>%
      ungroup()
    
    df$hover_text <- paste0(input$var1, ": ", df[[input$var1]],
                            "<br>", input$var2, ": ", df[[input$var2]],
                            "<br>", df$pct, "%")
    
    colors <- if (is_significant) c("blue", "orange") else c("grey", "lightgrey")
    
    plot_ly(
      data = df,
      x = ~.data[[input$var1]],
      y = ~pct,
      color = ~.data[[input$var2]],
      colors = colors,
      type = "bar",
      text = ~hover_text,
      hoverinfo = "text",
      textposition = "none",
      barmode = "group",
      marker = list(line = list(width = 0))
    ) %>%
      layout(
        title = paste("Répartition (%) de", input$var1, "selon", input$var2),
        xaxis = list(title = " ", showgrid = FALSE, tickangle = 30),
        yaxis = list(title = "Pourcentage", showgrid = FALSE),
        dragmode = FALSE
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  
  # DESCRIPTIVE 2
  output$descriptive2 <- renderPlotly({
    var <- donnees[[input$var_Famille]]
    if (is.null(var)) return(NULL)
    
    freq <- prop.table(table(var)) * 100
    freq <- round(freq, 1)
    
    plot_ly(
      x = names(freq),
      y = freq,
      type = "bar",
      text = paste(freq, "%"),
      hoverinfo = "text",
      textposition = "none",
      marker = list(color = 'blue')
    ) %>%
      layout(
        title = paste("Répartition de", input$var_Famille),
        xaxis = list(title = input$var_Famille, showgrid = FALSE),
        yaxis = list(title = "Pourcentage", showgrid = FALSE),
        dragmode = FALSE
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # STATISTIQUES 2
  output$stats_2 <- renderTable({
    
    stats_2 <- donnees %>%
      group_by(.data[[input$var_Famille]]) %>%
      summarise(
        Effectif = n(),
        Pourcentage = round(100 * n() / nrow(donnees), 1),
        .groups = "drop"
      )
    
    stats_2
  })
  
  
  # DESCRIPTIVE 3
  output$descriptive3 <- renderPlotly({
    var <- donnees[[input$var_emp_lois]]
    if (is.null(var)) return(NULL)
    
    freq <- prop.table(table(var)) * 100
    freq <- round(freq, 1)
    
    plot_ly(
      x = names(freq),
      y = freq,
      type = "bar",
      text = paste(freq, "%"),
      hoverinfo = "text",
      textposition = "none",
      marker = list(color = 'blue')
    ) %>%
      layout(
        title = paste("Répartition de", input$var_emp_lois),
        xaxis = list(title = input$var_emp_lois, showgrid = FALSE),
        yaxis = list(title = "Pourcentage", showgrid = FALSE),
        dragmode = FALSE
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  
  
  
  
  
  # DESCRIPTIVE 4
  output$descriptive4 <- renderPlotly({
    var <- donnees[[input$var_Orientation]]
    if (is.null(var)) return(NULL)
    
    freq <- prop.table(table(var)) * 100
    freq <- round(freq, 1)
    
    plot_ly(
      x = names(freq),
      y = freq,
      type = "bar",
      text = paste(freq, "%"),
      hoverinfo = "text",
      textposition = "none",
      marker = list(color = 'blue')
    ) %>%
      layout(
        title = paste("Répartition de", input$var_Orientation),
        xaxis = list(title = input$var_Orientation, showgrid = FALSE),
        yaxis = list(title = "Pourcentage", showgrid = FALSE),
        dragmode = FALSE
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  
  # CARTE
  centroides_data <- centroides_data %>%
  mutate(pct = round(100 * frequence / sum(frequence), 1))  # pourcentage arrondi à 1 décimale
  # Charger les pays du monde
  world <- ne_countries(scale = "medium", returnclass = "sf")
  
  # Garder uniquement la France
  france <- world[world$name == "France", ]
  
  # Carte avec surbrillance France
    
  output$carte <- renderLeaflet({
    leaflet(options = leafletOptions(
      preferCanvas = TRUE,
      dragging = TRUE,
      zoomControl = FALSE,
      scrollWheelZoom = FALSE,
      doubleClickZoom = FALSE,
      boxZoom = FALSE,
      keyboard = TRUE
    )) %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = 2.5, lat = 47, zoom = 6.2) %>%
      setMaxBounds(lng1 = -5, lat1 = 41, lng2 = 10, lat2 = 52) %>%
      
      addPolygons(
        data = france,
        fillColor = "#99ccff",   
        fillOpacity = 0.5,
        color = "#99ccff",        
        weight = 1
      ) %>%
      
      addMarkers(
        data = embrun_marker,
        lng = ~st_coordinates(geometry)[,1],
        lat = ~st_coordinates(geometry)[,2],
        popup = "Commune d'Embrun"
      ) %>%
      
      addCircles(
        data = centroides_data,
        lng = ~st_coordinates(geometry)[,1],
        lat = ~st_coordinates(geometry)[,2],
        radius = ~pct * 3000,
        weight = 0.5,
        color = "#666666",
        fillColor = "red",
        fillOpacity = 0.5,
        popup = ~paste0("<strong>", S_ville, "</strong><br>", pct, " %")
      ) %>%
      
      addLegend(
        position = "bottomright",
        title = "",
        colors = "red",
        labels = "Proportion d'eleve en % ",
        opacity = 0.5
      )
  })
  
  #FILTRE caracteristique des eleves
  
  filtered_data <- reactive({
    df <- donnees
    
    if (input$sexe != "Tous") {
      df <- df %>% filter(sexe == input$sexe)
    }
    if (input$terminale != "Tous") {
      df <- df %>% filter(`type de terminale` == input$terminale)
    }
    if (input$loge != "Tous") {
      df <- df %>% filter(`logé.e` == input$loge)
    }
    if (input$bourse != "Tous") {
      df <- df %>% filter(`bourse au cours de votre scolarité` == input$bourse)
    }
    return(df)
  })
  
  output$iobox_count <- renderInfoBox({
    count <- nrow(filtered_data())  # Appelle la reactive()
    
    infoBox(
      title = "",
      value = paste(count, "  élève(s)"),
      icon = icon("users"),
      color = "purple",
      fill = TRUE
    )
  })
  
  
  output$table <- renderDT({
    datatable(
      filtered_data()[, c("sexe", "type de terminale", "logé.e")],
      options = list(
        pageLength = 10,
        autoWidth = TRUE,
        dom = 'Bfrtip', # Ajoute les boutons
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
        class = 'stripe hover cell-border order-column'
      ),
      extensions = c('Buttons'),
      rownames = FALSE
    )
  })
  
  
  
  
  #filtre  entourage familial 
  
  filtered_data <- reactive({
    df <- donnees
    
    if (input$sexe != "Tous") {
      df <- df %>% filter(sexe == input$sexe)
    }
    
    if (input$loge != "Tous") {
      df <- df %>% filter(`logé.e` == input$loge)
    }
    if (input$bourse != "Tous") {
      df <- df %>% filter(`bourse au cours de votre scolarité` == input$bourse)
    }
    
    
    if (input$`statut d’activité du père / référent légal` != "Tous") {
      df <- df %>% filter(`situation professionnelle du père ou responsable légal` == input$`statut d’activité du père / référent légal`)
    }
    
    if (input$`Statut d’activité de la mère / représentante légale` != "Tous") {
      df <- df %>% filter(`situation professionnelle(mère ou responsable)` == input$`Statut d’activité de la mère / représentante légale`)
    }
    return(df)
  })
  
  output$iobox_count_familial  <- renderInfoBox({
    count <- nrow(filtered_data())  # Appelle la reactive()
    
    infoBox(
      title = "",
      value = paste(count, "  élève(s)"),
      icon = icon("users"),
      color = "purple",
      fill = TRUE
    )
  })
  
  
  output$table_familial  <- renderDT({
    datatable(
      filtered_data()[, c("sexe", "type de terminale", "logé.e","situation professionnelle du père ou responsable légal","situation professionnelle(mère ou responsable)" )],
      options = list(
        pageLength = 10,
        autoWidth = TRUE,
        dom = 'Bfrtip', # Ajoute les boutons
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
        class = 'stripe hover cell-border order-column'
      ),
      extensions = c('Buttons'),
      rownames = FALSE
    )
  })
  
  # ========================================2025========================================
  # ====================================================================================

    # Valeur box 1
  output$vbox_2025_1 <- renderbs4ValueBox({
    bs4ValueBox(
      value = HTML("<span style='font-size: 2rem;'>61%</span>"),
      subtitle = "De filles",
      icon = icon("venus", style = "font-size: 70px;"),
      elevation = 2,
      width = 3,
      color = "lightblue"
    )
  })
  
  # Valeur box 2
  output$vbox_2025_2 <- renderbs4ValueBox({
    bs4ValueBox(
      value = HTML("<span style='font-size: 2rem;'>68%</span>"),
      subtitle = HTML("Hors internat"),
      icon = icon("house-user", style = "font-size: 70px;"),
      elevation = 2,
      width = 3,
      color = "maroon"
    )
  })
  
  # Valeur box 3
  output$vbox_2025_3 <- renderbs4ValueBox({
    bs4ValueBox(
      value = HTML("<span style='font-size: 2rem;'>70%</span>"),
      subtitle = "En terminale générale",
      icon = icon("chalkboard-teacher", style = "font-size: 70px;"),
      elevation = 2,
      width = 3,
      color = "purple"
    )
  })
  
  # Valeur box 4
  output$vbox_2025_4 <- renderbs4ValueBox({
    bs4ValueBox(
      value = HTML("<span style='font-size: 2rem;'>69%</span>"),
      subtitle = HTML("Non boursier·es"),
      icon = icon("money-bill-wave", style = "font-size: 70px;"),
      elevation = 2,
      width = 3,
      color = "maroon"
    )
  })
  
  
  # DESCRIPTIF 2025 caracteristiques des eleves
  output$descriptive_2025 <- renderPlotly({
    var <- donnees_2025[[input$varName_2025]]
    if (is.null(var)) return(NULL)
    
    freq <- prop.table(table(var)) * 100
    freq <- round(freq, 1)
    freq <- sort(freq, decreasing = F)  # Tri croissant
    freq_labels <- paste(freq, "%", sep = "")
    
    plot_ly(
      x = names(freq),
      y = as.numeric(freq),
      type = "bar",
      text = freq_labels,
      hoverinfo = "text",
      textposition = "none",
      marker = list(color = 'blue')
    ) %>%
      layout(
        title = paste(input$varName_2025),
        xaxis = list(
          title = input$varName_2025,
          categoryorder = "array",
          categoryarray = names(freq),  # Respecter l’ordre trié
          showgrid = FALSE
        ),
        yaxis = list(title = "Pourcentage", showgrid = FALSE),
        dragmode = FALSE  # Supprime le zoom
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  
  # STATISTIQUES
  output$stats_2025 <- renderTable({
    var <- donnees_2025[[input$varName_2025]]
    if (is.numeric(var)) {
      stats <- data.frame(
        Moyenne = round(mean(var), 2),
        Médiane = median(var),
        Min = min(var),
        Max = round(max(var), 2),
        Écart_type = round(sd(var), 2)
      )
    } else {
      stats <- donnees_2025 %>%
        group_by(.data[[input$varName_2025]]) %>%
        summarise(
          Effectif = n(),
          Pourcentage = round(100 * n() / nrow(donnees_2025), 1),
          .groups = "drop"
        )
    }
    stats
  })
  
  
  # TABLEAU CROISÉ 2025
 
  output$tableau_croise_2025 <- renderTable({
    req(input$var1_2025, input$var2_2025)
    tab <- table(donnees_2025[[input$var1_2025]], donnees_2025[[input$var2_2025]])
    total <- sum(tab)
    formatted <- matrix(paste0(tab),
                        nrow = nrow(tab), ncol = ncol(tab),
                        dimnames = dimnames(tab))
    
    col_totals <- colSums(tab)
    formatted <- rbind(formatted, Total = paste0(col_totals))
    row_totals <- rowSums(tab)
    formatted <- cbind(formatted, Total = c(paste0(row_totals), paste0(total)))
    
    as.data.frame.matrix(formatted)
  }, rownames = TRUE)
  
  khi2_test_2025 <- reactive({
    req(input$var1_2025, input$var2_2025)
    tab <- table(donnees_2025[[input$var1_2025]], donnees_2025[[input$var2_2025]])
    chisq.test(tab)
  })
  output$khi2_result_2025 <- renderPrint({
    khi2_test_2025()
  })
  
  
  # GRAPHIQUE CROISÉ
  output$graphique_2025 <- renderPlotly({
    req(input$var1_2025, input$var2_2025)
    
    test <- khi2_test_2025()
    is_significant <- test$p.value <= 0.05
    
    df <- donnees_2025 %>%
      filter(!is.na(.data[[input$var1_2025]]), !is.na(.data[[input$var2_2025]])) %>%
      count(.data[[input$var1_2025]], .data[[input$var2_2025]]) %>%
      group_by(.data[[input$var1_2025]]) %>%
      mutate(pct = round(100 * n / sum(n), 1)) %>%
      ungroup()
    
    df$hover_text <- paste0(input$var1_2025, ": ", df[[input$var1_2025]],
                            "<br>", input$var2_2025, ": ", df[[input$var2_2025]],
                            "<br>", df$pct, "%")
    
    colors <- if (is_significant) c("blue", "#fc8d62") else c("grey50", "grey80")
    
    plot_ly(
      data = df,
      x = ~.data[[input$var1_2025]],
      y = ~pct,
      color = ~.data[[input$var2_2025]],
      colors = colors,
      type = "bar",
      text = ~hover_text,
      hoverinfo = "text",
      textposition = "none",
      barmode = "group",
      marker = list(line = list(width = 0))
    ) %>%
      layout(
        title = paste("Répartition de : ", input$var1_2025, "selon", input$var2_2025),
        xaxis = list(title = input$var1_2025, showgrid = FALSE, tickangle = 30),
        yaxis = list(title = "Pourcentage", showgrid = FALSE),
        dragmode = FALSE
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  
  
  #FILTRE caracteristique des eleves 2025
  
  filtered_data_2025 <- reactive({
    df <- donnees_2025
    
    if (input$sexes != "Tous") {
      df <- df %>% filter(sexe == input$sexes)
    }
    if (input$Terminales != "Tous") {
      df <- df %>% filter(`Dans quel type de terminale êtes-vous inscrit.e ?` == input$Terminales)
    }
    if (input$loges != "Tous") {
      df <- df %>% filter(`Etes-vous en internat au lycée cette année scolaire ?` == input$loges)
    }
    if (input$bourses != "Tous") {
      df <- df %>% filter(`Avez‐vous bénéficié d’une bourse au cours de votre scolarité au lycée ?` == input$bourses)
    }
    return(df)
  })
  
  output$iobox_count_2025 <- renderInfoBox({
    count_2025 <- nrow(filtered_data_2025())  #Appelle la reactive()
    
    infoBox(
      title = "",
      value = paste(count_2025, "  élève(s)"),
      icon = icon("users"),
      color = "purple",
      fill = TRUE
    )
  })
  
  
  output$table_2025 <- renderDT({
    datatable(
      filtered_data_2025()[, c("sexe", "Dans quel type de terminale êtes-vous inscrit.e ?", "Etes-vous en internat au lycée cette année scolaire ?","Avez‐vous bénéficié d’une bourse au cours de votre scolarité au lycée ?")],
      options = list(
        pageLength = 10,
        autoWidth = TRUE,
        dom = 'Bfrtip', # Ajoute les boutons
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
        class = 'stripe hover cell-border order-column'
      ),
      extensions = c('Buttons'),
      rownames = FALSE
    )
  })
  
  
  
  # DESCRIPTIVE 2
  output$descriptive22 <- renderPlotly({
    var <- donnees_2025[[input$var_Famille_2025]]
    if (is.null(var)) return(NULL)
    
    freq <- prop.table(table(var)) * 100
    freq <- round(freq, 1)
    
    plot_ly(
      x = names(freq),
      y = freq,
      type = "bar",
      text = paste(freq, "%"),
      hoverinfo = "text",
      textposition = "none",
      marker = list(color = 'blue')
    ) %>%
      layout(
        title = paste(" ", input$var_Famille_2025),
        xaxis = list(title = input$var_Famille_2025, showgrid = FALSE),
        yaxis = list(title = "Pourcentage", showgrid = FALSE),
        dragmode = FALSE
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # STATISTIQUES 2
  output$stats_22 <- renderTable({
    
    stats_22 <- donnees_2025 %>%
      group_by(.data[[input$var_Famille_2025]]) %>%
      summarise(
        Effectif = n(),
        Pourcentage = round(100 * n() / nrow(donnees_2025), 1),
        .groups = "drop"
      )
    
    stats_22
  })
  
  
  #filtre  entourage familial  2025
  
  filtered_data_2025 <- reactive({
    df <- donnees_2025
    
    if (input$sexes != "Tous") {
      df <- df %>% filter(sexe == input$sexes)
    }
    if (input$Terminales != "Tous") {
      df <- df %>% filter(`Dans quel type de terminale êtes-vous inscrit.e ?` == input$Terminales)
    }
    if (input$loges != "Tous") {
      df <- df %>% filter(`Etes-vous en internat au lycée cette année scolaire ?` == input$loges)
    }
    if (input$bourses != "Tous") {
      df <- df %>% filter(`Avez‐vous bénéficié d’une bourse au cours de votre scolarité au lycée ?` == input$bourses)
    }
    
    
    if (input$`statut d’activité du père / référent légal` != "Tous") {
      df <- df %>% filter(`Quel est la situation professionnelle du père ou responsable légal` == input$`statut d’activité du père / référent légal`)
    }
    
    
    if (input$`Statut d’activité de la mère / représentante légale` != "Tous") {
      df <- df %>% filter(`Quelle est la situation professionnelle de votre mère ou responsable légale?` == input$`Statut d’activité de la mère / représentante légale`)
    }
    return(df)
  })
  
  output$iobox_count_familial_2025  <- renderInfoBox({
    count <- nrow(filtered_data_2025())  # Appelle la reactive()
    
    infoBox(
      title = "",
      value = paste(count, "  élève(s)"),
      icon = icon("users"),
      color = "purple",
      fill = TRUE
    )
  })
  
  
  output$table_familial_2025  <- renderDT({
    datatable(
      filtered_data_2025()[, c("sexe", "Dans quel type de terminale êtes-vous inscrit.e ?", "Etes-vous en internat au lycée cette année scolaire ?","Avez‐vous bénéficié d’une bourse au cours de votre scolarité au lycée ?","Quel est la situation professionnelle du père ou responsable légal","Quelle est la situation professionnelle de votre mère ou responsable légale?")],
      options = list(
        pageLength = 10,
        autoWidth = TRUE,
        dom = 'Bfrtip', # Ajoute les boutons
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
        class = 'stripe hover cell-border order-column'
      ),
      extensions = c('Buttons'),
      rownames = FALSE
    )
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  

  # CARTE 2025
  centroides_data_2025 <- centroides_data_2025 %>%
    mutate(pct = round(100 * frequence / sum(frequence), 1))  # pourcentage arrondi à 1 décimale
  # Charger les pays du monde
  world <- ne_countries(scale = "medium", returnclass = "sf")
  
  # Garder uniquement la France
  france <- world[world$name_fr == "France", ]
  

  # Carte avec surbrillance France
  
  output$carte_2025 <- renderLeaflet({
    leaflet(options = leafletOptions(
      preferCanvas = TRUE,
      dragging = TRUE,
      zoomControl = F,
      scrollWheelZoom = T,
      doubleClickZoom = F,
      boxZoom = F,
      keyboard = TRUE
    )) %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = 2.5, lat = 47, zoom = 6.2) %>%
      setMaxBounds(lng1 = -5, lat1 = 41, lng2 = 10, lat2 = 52) %>%
      
      addPolygons(
        data = france,
        fillColor = "#99ccff",   
        fillOpacity = 0.5,
        color = "#99ccff",        
        weight = 1
      ) %>%
      
      addMarkers(
        data = embrun_marker,
        lng = ~st_coordinates(geometry)[,1],
        lat = ~st_coordinates(geometry)[,2],
        popup = "Commune d'Embrun"
      ) %>%
      
      addCircles(
        data = centroides_data_2025,
        lng = ~st_coordinates(geometry)[,1],
        lat = ~st_coordinates(geometry)[,2],
        radius = ~pct * 1500,
        weight = 0.5,
        color = "#666666",
        fillColor = "red",
        fillOpacity = 0.5,
        popup = ~paste0("<strong>", S_ville, "</strong><br>", pct, " %", "<br>" ,frequence, " élève(s)")
      ) %>%
      
      addLegend(
        position = "bottomright",
        title = "",
        colors = "red",
        labels = "Proportion d'eleve en % ",
        opacity = 0.5
      )
  })
  
  
  # DESCRIPTIVE 2025
  output$descriptive4_2025 <- renderPlotly({
    var <- donnees_2025[[input$var_Orientation_2025]]
    if (is.null(var)) return(NULL)
    
    freq <- prop.table(table(var)) * 100
    freq <- round(freq, 1)
    freq <- sort(freq, decreasing = F)  # Tri croissant
    
    plot_ly(
      x = names(freq),
      y = freq,
      type = "bar",
      text = paste(freq, "%"),
      hoverinfo = "text",
      textposition = "none",
      marker = list(color = 'blue')
    ) %>%
      layout(
        title = paste(input$var_Orientation_2025),
        xaxis = list(
          title = input$var_Orientation_2025,
          categoryorder = "array",
          categoryarray = names(freq),  # Fixer l'ordre
          showgrid = FALSE
        ),
        yaxis = list(title = "Pourcentage", showgrid = FALSE),
        dragmode = FALSE
      ) %>%
      config(displayModeBar = FALSE)
  })
  

  
  # STATISTIQUES
  output$stats_2025_orientation <- renderTable({
    var <- donnees_2025[[input$var_Orientation_2025]]
    if (is.numeric(var)) {
      stats <- data.frame(
        Moyenne = round(mean(var), 2),
        Médiane = median(var),
        Min = min(var),
        Max = round(max(var), 2),
        Écart_type = round(sd(var), 2)
      )
    } else {
      stats <- donnees_2025 %>%
        group_by(.data[[input$var_Orientation_2025]]) %>%
        summarise(
          Effectif = n(),
          Pourcentage = round(100 * n() / nrow(donnees_2025), 1),
          .groups = "drop"
        )
    }
    stats
  })
  
  

  
  observeEvent(input$go2024, {
    updateTabItems(session, "sidebar", selected = "generation")        # aller à la page generation
    updateTabsetPanel(session, "main_tabs", selected = "cohorte_2024") # sélectionner le sous-onglet
  })
  
  observeEvent(input$go2025, {
    updateTabItems(session, "sidebar", selected = "generation")        # aller à la page generation
    updateTabsetPanel(session, "main_tabs", selected = "cohorte_2025") # sélectionner le sous-onglet
  })
  
  # info box sur longlet enquete
  output$ibox1 <- renderbs4InfoBox({
    bs4InfoBox(
      title = "Cohorte 2024 : 61 répondants parmi les 130 élèves de
terminale",
      value = ,
      icon = icon("check-circle"),
      color = "primary",
      fill = TRUE
    )
  })
  
  output$ibox2 <- renderbs4InfoBox({
    bs4InfoBox(
      title = "Cohorte 2025 : 118 répondants ",
      value = ,
      icon = icon("check-circle"),
      color = "info",
      fill = TRUE
    )
  })
  
  
}


