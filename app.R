library(reshape2)         # transformation format long, format large
library(sp)               # objets spatiaux
library(rgdal)            # fonctions de la bibliothèque GDAL
library(ggplot2)          # fonctions graphiques
library(ggthemes)  
library(gstat)             # thèmes pour ggplot
library(grid)             # fonction arrow
library(cartography)      # cartographie thématique
library(RColorBrewer)     # palettes de couleurs de C. Brewer
library(dplyr)            # manipulation de tableaux
library(shiny)
library(sf)
library(classInt)
library(leaflet)
library(shinyjs)
library(shinycssloaders)
library(shinydashboard)
library(shinythemes)
library(emojifont)
library(raster)
library(SpatialPosition)



commData <- merge(comm,tabflow6, by.x="insee", by.y = "ORI")

coordCom <- coordcom



ui<- bootstrapPage(
  # element d'affichage de la page
  theme = shinytheme("superhero"),
  useShinyjs(),
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  
  tags$style(HTML("
                  #mapIndic {
                  position: absolute;
                  }
                  #mapvis {
                  position: absolute;
                  }
                  #mapsyn {
                  position: absolute;
                  }
                  #mapflu {
                  position: absolute;
                  }
                  #mappot {
                  position: absolute;
                  }
                  ")),
  
  ##############################
  #######            Map           #######
  ############################## 
  
  
  conditionalPanel(
    condition = "input.tabs=='Mobilité'",
    leafletOutput("mapIndic", width="100%", height = "100%")
  ),
  conditionalPanel(
    condition = "input.tabs=='Desserte'",
    leafletOutput("mapvis", width="100%", height = "100%")
  ),
  conditionalPanel(
    condition = "input.tabs=='Vitesse'",
    leafletOutput("mapsyn", width="100%", height = "100%")
  ),
  conditionalPanel(
    condition = "input.tabs=='Flux'",
    leafletOutput("mapflu", width="100%", height = "100%")
  ),
  conditionalPanel(
    condition = "input.tabs=='Bassin'",
    leafletOutput("mappot", width="100%", height = "100%")
  ),
  
  ##############################
  #######   Panneau des Scénarios  #######
  ############################## 
  
  absolutePanel(top = "2%", 
                right = "40%",
                div(class="btn-group",
                    actionButton("Scenario1", "Scenario1"),
                    actionButton("Scenario2", "Scenario2"),
                    actionButton("Scenario3", "Scenario3"),
                    actionButton("Scenario4", "Scenario4"),
                    actionButton("Scenario5", "Scenario5")
                )
  ),
  
  
  ##############################
  #######          Titre           #######
  ############################## 
  
  
  ##############################
  ####### Panneau des indicateurs #######
  ############################## 
  
  absolutePanel( class = "panel panel-default",
                 style = "padding : 10px",
                 top = "10%", 
                 left = "2%",
                 tabsetPanel(id = "tabs", 
                             
                             ##############################
                             ####### Panneau Mobilité #####
                             ##############################  
                             
                             tabPanel("Mobilité", class="btn-group-vertical",
                                      
                                      actionButton("Gravitation", "Gravitation"),
                                      actionButton("Contention", "Auto-Contention"),
                                      actionButton("Suffisance", "Auto-Suffisance"),
                                      actionButton("Dependance", "Dépendance"),
                                      actionButton("Mobilite", "Mobilité")
                             ),
                             
                             ##############################
                             ####### Panneau Desserte #####
                             ##############################
                             
                             tabPanel("Desserte",
                                      selectInput("viscom", 
                                                  label = "Choisir une commune",
                                                  choices = sort(coordCom$LIBGEO),
                                                  selected = ""),
                                      radioButtons("visref", label = "Origine ou destination", choices = c("Origine" = "ORI", "Destination" = "DES"), selected = "ORI"),
                                      radioButtons("vismod", label = "Mode de transport", choices = c("Transport en commun" = "TC", "Voiture (matin)" = "VPM", "Voiture (soir)" = "VPS"), selected = "TC"),
                                      sliderInput("visthr", label = "Seuil temporel", min = 15, max = 120, step = 15, value = 60),
                                      tags$br(),
                                      actionButton("vis1_descr", "Description"),
                                      tags$br(),
                                      actionButton("vis1_exemp", "Clés de lecture"),
                                      tags$br(),
                                      actionButton("vis1_donne", "Détails techniques")
                             ),
                             
                             ##############################
                             ####### Panneau Vitesse  #####
                             ##############################
                             
                             tabPanel("Vitesse",
                                      selectInput("viscom", 
                                                  label = "Choisir une commune",
                                                  choices = sort(coordCom$LIBGEO),
                                                  selected = ""),
                                      radioButtons("visref", label = "Origine ou destination", choices = c("Origine" = "ORI", "Destination" = "DES"), selected = "ORI"),
                                      radioButtons("vismod", label = "Mode de transport", choices = c("Transport en commun" = "TC", "Voiture (matin)" = "VPM", "Voiture (soir)" = "VPS"), selected = "TC"),
                                      sliderInput("visthr", label = "Seuil temporel", min = 15, max = 120, step = 15, value = 60),
                                      tags$br(),
                                      actionButton("vis1_descr", "Description"),
                                      tags$br(),
                                      actionButton("vis1_exemp", "Clés de lecture"),
                                      tags$br(),
                                      actionButton("vis1_donne", "Détails techniques")
                                      ,
                                      plotOutput("grapoma", width = "100%")
                             ),
                             
                             ##############################
                             ####### Panneau Flux     #####
                             ##############################
                             
                             tabPanel("Flux",
                                      selectInput("flucom", 
                                                  label = "Choisir une commune",
                                                  choices = sort(coordCom$LIBGEO),
                                                  selected = ""),
                                      radioButtons("fluref", label = "Origine ou destination", choices = c("Origine" = "ORI", "Destination" = "DES"), selected = "ORI"),
                                      radioButtons("flumod", label = "Mode de transport", choices = c("Tous modes" = "TOUT", "Transport en commun" = "TC", "Voiture" = "VP"), selected = "TOUT"),
                                      radioButtons("fluvar", label = "Quantité", choices = c("Nombre d'individus" = "FLOW", "Cumul de distance" = "DISTTOT"), selected = "FLOW"),
                                      sliderInput("fluthr", label = "Top", min = 2, max = 100, step = 1, value = 3),
                                      tags$br(),
                                      actionButton("vis3_descr", "Description"),
                                      tags$br(),
                                      actionButton("vis3_exemp", "Clés de lecture"),
                                      tags$br(),
                                      actionButton("vis3_donne", "Détails techniques")
                             ),
                             
                             ##############################
                             ####### Panneau Bassin   #####
                             ##############################
                             
                             tabPanel("Bassin",
                                      radioButtons("pottyp", 
                                                   label = "Type de potentiel", 
                                                   choices = c("Origine" = "ori", "Destination" = "des", "Différentiel" = "dif"), 
                                                   selected = "ori"),
                                      radioButtons("potcat",
                                                   label = "Catégorie de population", 
                                                   choices = c("Tout" = "tout", 
                                                               "Femme" = "femm", 
                                                               "Homme" = "homm",
                                                               "Agriculteur" = "agri", 
                                                               "Artisan-commerçant" = "arti", 
                                                               "Prof. supérieure" = "cadr",
                                                               "Prof. intermédiaire" = "inte",
                                                               "Employé" = "empl",
                                                               "Ouvrier" = "ouvr",
                                                               "Automobiliste" = "vp", 
                                                               "Transporté collectivement" = "tc"),
                                                   selected = "tout"),
                                      tags$br(),
                                      actionButton("vis4_descr", "Description"),
                                      tags$br(),
                                      actionButton("vis4_exemp", "Clés de lecture"),
                                      tags$br(),
                                      actionButton("vis4_donne", "Détails techniques"))
                 )
  )
  )

server <- function(input, output, session) {
  
  v <- reactiveValues(data = commData$Gravitation)
  addClass("Gravitation", "btn-warning active")
  
  observeEvent(input$Gravitation, {
    removeClass("Contention", "btn-warning active")
    removeClass("Suffisance", "btn-warning active")
    removeClass("Dependance", "btn-warning active")
    removeClass("Mobilite", "btn-warning active")
    addClass("Gravitation", "btn-warning active")
    v$data <- commData$Gravitation
  })
  observeEvent(input$Contention, {
    removeClass("Gravitation", "btn-warning active")
    removeClass("Suffisance", "btn-warning active")
    removeClass("Dependance", "btn-warning active")
    removeClass("Mobilite", "btn-warning active")
    addClass("Contention", "btn-warning active")
    v$data <- commData$AutoContention
  })
  observeEvent(input$Suffisance, {
    removeClass("Gravitation", "btn-warning active")
    removeClass("Dependance", "btn-warning active")
    removeClass("Mobilite", "btn-warning active")
    removeClass("Contention", "btn-warning active")
    addClass("Suffisance", "btn-warning active")
    v$data <- commData$AutoSuffisance
  }) 
  observeEvent(input$Dependance, {
    removeClass("Gravitation", "btn-warning active")
    removeClass("Mobilite", "btn-warning active")
    removeClass("Contention", "btn-warning active")
    removeClass("Suffisance", "btn-warning active")
    addClass("Dependance", "btn-warning active")
    v$data <- commData$Dependance
  })
  observeEvent(input$Mobilite, {
    removeClass("Gravitation", "btn-warning active")
    removeClass("Contention", "btn-warning active")
    removeClass("Suffisance", "btn-warning active")
    removeClass("Dependance", "btn-warning active")
    addClass("Mobilite", "btn-warning active")
    v$data <- commData$Mobilite
  })  
  
  ##############################
  #####  Map indicateurs   #####
  ##############################
  
  output$mapIndic <- renderLeaflet({
    leaflet(commData, options = leafletOptions(minZoom = 9, zoomControl = FALSE)) %>%  
      addProviderTiles(
        providers$"CartoDB.DarkMatter") %>% 
      addPolygons(
        fillColor = ~colorBin(palette = "YlOrRd", 
                              bins = getBreaks(v$data, 
                                               nclass = 6, 
                                               method = "fisher-jenks"),
                              domain = v$data
        )(v$data),
        weight = 1,
        opacity = 0.3,
        color = "white",
        fillOpacity = 0.3,
        highlight = highlightOptions(
          weight = 3,
          color = "white",
          opacity = 1,
          fillOpacity = 0.5,
          bringToFront = TRUE),
        label = sprintf(
          "<strong>%s</strong><br/> valeur : %g",
          commData$nomcom, 
          v$data
        )%>% lapply(htmltools::HTML),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", 
                       padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")) %>%
      addLegend(
        pal = colorBin(palette = "YlOrRd", 
                       bins = getBreaks(v$data, 
                                        nclass = 6, 
                                        method = "fisher-jenks"),
                       domain = v$data, 
                       pretty = TRUE), 
        values = ~v$data, 
        opacity = 0.7, 
        title = NULL,
        position = "bottomright") %>%
      setMaxBounds(lng1 = 1.44,
                   lat1 = 48.12,
                   lng2 = 3.55,
                   lat2 = 49.23)
  })
  
  ##############################
  #####    Map Desserte    #####
  ##############################
  
  output$mapvis <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles(provider = "CartoDB.DarkMatter") %>% 
      fitBounds(lng1 = 1.44, lat1 = 48.12, lng2 = 3.55, lat2 = 49.24)
  })
  
  observe({
    codCom <- coordCom$CODGEO[coordCom$LIBGEO == input$viscom]
    oneCom <- st_centroid(pomaCom[pomaCom$CODGEO == codCom, ])
    leafletProxy("mapvis") %>%
      clearShapes() %>% clearMarkers() %>% 
      addPolygons(data = DrawVis(), color = "grey", weight = 1, fill = TRUE, fillColor = c("black", "white"), fillOpacity = 0.3) %>% 
      addCircleMarkers(data = oneCom, stroke = FALSE, fill = TRUE, radius = 8, fillOpacity = 0.8, fillColor = "firebrick")
  })
  
  ##############################
  #####    Map Vitesse     #####
  ##############################
  
  ##Graph Vitesse
  output$grapoma <- renderPlot({
    PomaPlot(tabsum = pomaTable, mod = input$synmod)
  })
  
  # output$mapsyn <- renderLeaflet({
  #   leaflet() %>%
  #     addProviderTiles(provider = "CartoDB.DarkMatter") %>%
  #     fitBounds(lng1 = 1.44, lat1 = 48.12, lng2 = 3.55, lat2 = 49.24)
  # })
  # 
  # observe({
  #   varCarto <- paste0("CLASS", input$synmod)
  #   colPal <- c("tan3", "chartreuse4", "firebrick", "goldenrod")
  #   FctPal <- colorFactor(palette = colPal, levels = c("0_0", "0_1", "1_0", "1_1"), na.color = "transparent")
  #   leafletProxy("mapsyn") %>%
  #     clearShapes() %>%
  #     addPolygons(data = pomaCom, stroke = TRUE, weight = 0.5, color = "grey", fill = TRUE, fillColor = ~FctPal(eval(parse(text = varCarto))), fillOpacity = 0.4, label = pomaCom$LIBGEO)
  # })
  
  ##############################
  #####      Map Flux      #####
  ##############################
  
  output$mapflu <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(provider = "CartoDB.DarkMatter") %>%
      fitBounds(lng1 = 1.44, lat1 = 48.12, lng2 = 3.55, lat2 = 49.24)
  })

  observe({
    topDes <- GetTopLinks()
    leafletProxy("mapflu") %>%
      clearShapes() %>%
      addPolygons(data = topDes$POLYG, label = topDes$POLYG$LIBGEO, stroke = TRUE, weight = 1, color = "grey35", fill = TRUE, fillColor = "ghostwhite", fillOpacity = 0.3) %>%
      addPolylines(data = topDes$LINES, color = "firebrick", opacity = 0.8, weight = 1.5, stroke = TRUE)
  })
  
  ##############################
  #####    Map Bassin      #####
  ##############################
  
  output$mappot <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(provider = "CartoDB.DarkMatter") %>%
      fitBounds(lng1 = 1.44, lat1 = 48.12, lng2 = 3.55, lat2 = 49.24)
  })

  observe({
    if(input$pottyp == "dif"){
      leafletProxy("mappot") %>%
        clearImages() %>% clearShapes() %>% clearControls() %>%
        addRasterImage(x = SelecPotential(), colors = PotentialPalette(SelecPotential()), opacity = 0.4) %>%
        addLegend(position = "topright",
                  colors = c("#B22222", "#E5E5E5", "#000080"),
                  labels = c("Surplus d'emplois (déficit d'actifs)",
                             "Équilibre actifs-emplois",
                             "Surplus d'actifs (déficit d'emplois)"))
    } else if(input$pottyp == "ori"){
      leafletProxy("mappot") %>%
        clearImages() %>% clearShapes() %>% clearControls() %>%
        addRasterImage(x = sqrt(SelecPotential()), colors = PotentialPalette(sqrt(SelecPotential())), opacity = 0.4) %>%
        addPolygons(data = DrawContour(), stroke = TRUE, fill = FALSE, color = "#a9a9a9", weight = 1,
                    label = paste(as.character(round(DrawContour()$center^2)), "actifs")) %>%
        addLegend(position = "topright",
                  colors = c("#B22222", "#E5E5E5"),
                  labels = c("Forte densité d'actifs",
                             "Faible densité d'actifs"))
    } else {
      leafletProxy("mappot") %>%
        clearImages() %>% clearShapes() %>% clearControls() %>%
        addRasterImage(x = sqrt(SelecPotential()), colors = PotentialPalette(sqrt(SelecPotential())), opacity = 0.4) %>%
        addPolygons(data = DrawContour(), stroke = TRUE, fill = FALSE, color = "#a9a9a9", weight = 1,
                    label = paste(as.character(round(DrawContour()$center^2)), "emplois")) %>%
        addLegend(position = "topright",
                  colors = c("#B22222", "#E5E5E5"),
                  labels = c("Forte densité d'emplois (destination)",
                             "Faible densité d'emplois (destination)"))
    }
  })
  
  ##############################
  #####      Function      #####
  ##############################
  
  SelecPotential <- reactive({
    req(input$pottyp, input$potcat)
    if(input$potcat %in% c("femm", "homm")){
      keyRas <- paste(input$pottyp, input$potcat, "tout", "tout", sep = "_")
    } else if (input$potcat %in% c("agri", "arti", "cadr", "inte", "empl", "ouvr")){
      keyRas <- paste(input$pottyp, "tout", input$potcat, "tout", sep = "_")
    } else if (input$potcat %in% c("vp", "tc")){
      keyRas <- paste(input$pottyp, "tout", "tout", input$potcat, sep = "_")
    } else {
      keyRas <- paste(input$pottyp, "tout", "tout", "tout", sep = "_")
    }
    oneRaster <- listPotentials[[keyRas]]
    return(oneRaster)
  })
  
  DrawContour <- reactive({
    potCont <- PotentialContour(ras = sqrt(SelecPotential()))
    return(potCont)
  })
  
  GetOneCoord <- reactive({
    req(input$viscom)
    oneCoord <- GetTime(tabcoords = coordCom, 
                        tabtime = listTimes[[input$vismod]], 
                        ref = input$visref, 
                        oneunit = coordCom$CODGEO[coordCom$LIBGEO == input$viscom])
    return(oneCoord)
  })
  
  GetTopLinks <- reactive({
    req(input$fluref, input$fluvar, input$flucom, input$fluthr)
    topLinks <- GetLinks(tabnav = tabFlows, spcom = pomaCom, ref = input$fluref, mod = input$flumod, varsort = input$fluvar, oneunit = input$flucom, thres = input$fluthr)
    return(topLinks)
  })
  
  DrawVis <- reactive({
    req(input$visthr)
    contVis <- DrawVisibleZone(ras = listPotentials[[1]], onetime = GetOneCoord(), thres = input$visthr)
    return(contVis)
  })
  
  DrawPoma <- reactive({
    req(input$synmod)
    varCarto <- paste0("CLASS", input$synmod)
    selPoma <- pomaCom
    return(contVis)
  })
  
  ##############################
  #####      Global        #####
  ##############################
  
  # Create color palette for potentials ----
  
  PotentialPalette <- function(ras) {
    valRas <- c(as.matrix(ras))
    valRasMin <- min(valRas, na.rm = TRUE)
    valRasMax <- max(valRas, na.rm = TRUE)
    valRange <- c(valRasMin, valRasMax)
    if(valRasMin >= 0 & valRasMax > 0){
      palCol <- colorRampPalette(c("grey90", "firebrick"))(100)
    } else if (valRasMax - valRasMin < 40) {
      palCol <- "grey90"
    } else {
      seqVal <- seq(valRasMin, valRasMax, 20)
      getZero <- findInterval(0, seqVal)
      palBlue <- colorRampPalette(c("navyblue", "grey90"))(getZero)
      palRed <- colorRampPalette(c("grey90", "firebrick"))(length(seqVal)-getZero)
      palCol <- c(palBlue, palRed)
    }
    return(palCol)
  }
  
  
  
  # Create color palette for potentials ----
  
  PotentialContour <- function(ras) {
    potCont <- rasterToContourPoly(r = ras, nclass = 15)
    potContGeo <- st_as_sf(spTransform(potCont, CRSobj = CRS("+init=epsg:4326")))
    return(potContGeo)
  }
  
  
  # Select spatial unit and join time values ----
  
  GetTime <- function(tabcoords, tabtime, ref, oneunit){
    oriDes <- c("ORI", "DES")
    invRef <- oriDes[oriDes != ref]
    oneTime <- tabtime[tabtime[[ref]] == oneunit, ]
    oneTimeCoords <- tabcoords %>% left_join(oneTime, by = c("CODGEO" = invRef)) %>% filter(!is.na(VAL))
    return(oneTimeCoords)
  }
  
  
  # Draw visible zone with time threshold ----
  
  DrawVisibleZone <- function(ras, onetime, thres){
    timeInterpol <- gstat(id = "VAL", formula = VAL ~ 1, locations = ~ X1 + X2, data = onetime, nmax = 10)
    rasTime <- interpolate(ras, timeInterpol, xyOnly = TRUE, xyNames = c("X1", "X2"))
    rasTime <- mask(rasTime, ras)
    valRas <- c(as.matrix(rasTime))
    valRasMax <- max(valRas, na.rm = TRUE)
    contThres <- rasterToContourPoly(r = rasTime, breaks = c(0, thres, ceiling(valRasMax)))
    contThres <- st_as_sf(spTransform(contThres, CRSobj = CRS("+init=epsg:4326")))
    return(contThres)
  }
  
  
  # create POMA plot ----
  
  PomaPlot <- function(tabsum, mod){
    varx <- paste0("AVG", mod)
    vary <- paste0("PMI", mod)
    varclass <- paste0("CLASS", mod)
    tabsum <- tabsum[tabsum[[varclass]] %in% c("0_0", "0_1", "1_0", "1_1"), ]
    medx <- median(tabsum[[varx]], na.rm = T)
    medy <- median(tabsum[[vary]], na.rm = T)
    pomaPlot <- ggplot(tabsum) + 
      geom_point(aes_string(x = varx, y = vary, color = varclass), size = 1) + 
      geom_vline(xintercept = medx, color = "grey80") +
      geom_hline(yintercept = medy, color = "grey80") +
      # scale_color_manual(values = c("chartreuse4", "tan3", "goldenrod", "firebrick")) +
      scale_color_manual(values = c("tan3", "chartreuse4", "firebrick", "goldenrod")) +
      xlab(label = paste0("Temps moyen d'accès vers tout (", mod, ", mn)")) + 
      ylab(label = paste0("Vitesse moyenne d'accès vers tout (", mod, ", km/h)")) + 
      theme_darkhc
    return(pomaPlot)
  }
  
  
  # get top links ----
  
  GetLinks <- function(tabnav, spcom, ref, mod, varsort, oneunit, thres){
    refLib <- paste0(ref, "LIB")
    oriDes <- paste0(c("ORI", "DES"), "LIB")
    invRef <- oriDes[oriDes != refLib]
    print(mod)
    if(mod == "TOUT"){
      tabSel <- tabnav %>% 
        group_by(ORI, DES) %>% 
        summarise(FLOW = sum(FLOW), DIST = first(DIST), DISTTOT = sum(DISTTOT), ORILIB = first(ORILIB), DESLIB = first(DESLIB)) %>% 
        as.data.frame(stringsAsFactors = FALSE)
      tabSel <- tabSel[tabSel[[refLib]] == oneunit, ]
      tabSel <- tabSel[order(tabSel[[varsort]], decreasing = TRUE), ]
    } else {
      tabSel <- tabnav[tabnav[[refLib]] == oneunit, ] 
      tabSel <- tabSel[tabSel$MODE %in% mod, ] 
      tabSel <- tabSel[order(tabSel[[varsort]], decreasing = TRUE), ]
    }
    
    nbRows <- ifelse(thres > nrow(tabSel), nrow(tabSel), thres)
    spLinks <- getLinkLayer(x = pomaCom, df = tabSel[1:nbRows, c("ORI", "DES")])
    print(spLinks)
    spPol <- spcom[spcom$CODGEO %in% spLinks$DES, ]
    topDes <- list(POLYG = spPol, LINES = spLinks)
    return(topDes)
  }
  
  
  
  # ggplot dark theme ----
  
  # without lines
  theme_darkhc <- theme_bw() +
    theme(plot.background = element_rect(fill = "#272B30"),
          axis.line = element_line(color = "grey80"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_rect(fill = "#272B30"),
          axis.title = element_text(family = "sans-serif", color = "grey80"),
          axis.text = element_text(family = "sans-serif", color = "grey80"),
          axis.ticks = element_blank(),
          legend.position = "none",
          legend.background = element_rect(fill = "#272B30"))
  
  # with lines
  theme_darklinehc <- theme_bw() +
    theme(plot.background = element_rect(fill = "#272B30"),
          axis.line = element_line(color = "grey80"),
          # panel.grid.major = element_line(color = "grey80", size = 0.1),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_rect(fill = "#272B30"),
          axis.title = element_text(family = "sans-serif", color = "grey80"),
          axis.text = element_text(family = "sans-serif", color = "grey80"),
          axis.ticks = element_blank(),
          legend.position = "bottom",
          legend.title = element_blank(),
          legend.key =  element_blank(),
          legend.text = element_text(family = "sans-serif", color = "grey80"),
          legend.background = element_rect(fill = "#272B30"))
  
}

shinyApp(ui = ui, server = server)
