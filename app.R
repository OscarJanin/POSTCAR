library(reshape2)         # transformation format long, format large
library(sp)               # objets spatiaux
library(rgdal)            # fonctions de la bibliothèque GDAL
library(ggplot2)          # fonctions graphiques
library(ggthemes)  
library(gstat)            # thèmes pour ggplot
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

data_PCR <- readRDS("C:/Users/Bureau des MCFs/Desktop/Postcar/Dev/Tuto/PremiereAppliShiny/data/data_PCR.Rds")
pomaTable <- readRDS("C:/Users/Bureau des MCFs/Desktop/Postcar/Dev/Tuto/PremiereAppliShiny/data/pomatable.Rds")
coordcom <- readRDS("C:/Users/Bureau des MCFs/Desktop/Postcar/Dev/Tuto/PremiereAppliShiny/data/coordcom.Rds")
listtimes <- readRDS("C:/Users/Bureau des MCFs/Desktop/Postcar/Dev/Tuto/PremiereAppliShiny/data/listtimes.Rds")
pomaCom <- readRDS(file = "C:/Users/Bureau des MCFs/Desktop/Postcar/Dev/Tuto/PremiereAppliShiny/data/pomacom.Rds")
listTimes <- readRDS(file = "C:/Users/Bureau des MCFs/Desktop/Postcar/Dev/Tuto/PremiereAppliShiny/data/listtimes.Rds")
listPotentials <- readRDS(file = "C:/Users/Bureau des MCFs/Desktop/Postcar/Dev/Tuto/PremiereAppliShiny/data/listpotentials.Rds")
pomaTable <- readRDS(file = "C:/Users/Bureau des MCFs/Desktop/Postcar/Dev/Tuto/PremiereAppliShiny/data/pomatable.Rds")
tabFlows <- readRDS(file = "C:/Users/Bureau des MCFs/Desktop/Postcar/Dev/Tuto/PremiereAppliShiny/data/tabflows.Rds")
listCondor <- readRDS(file = "C:/Users/Bureau des MCFs/Desktop/Postcar/Dev/Tuto/PremiereAppliShiny/data/condorcet.Rds")


comm <- read_sf(dsn = "C:/Users/Bureau des MCFs/Desktop/Postcar/Dev/Tuto/PremiereAppliShiny/data/les-communes-generalisees-dile-de-france.shp")
vferre <- read_sf(dsn = "C:/Users/Bureau des MCFs/Desktop/Postcar/Dev/Tuto/PremiereAppliShiny/data/traces-du-reseau-ferre-idf.shp")
routier <- read_sf(dsn = "C:/Users/Bureau des MCFs/Desktop/Postcar/Dev/Tuto/PremiereAppliShiny/data/Réseau_routier_magistral_existant_de_la_Région_ÎledeFrance_inscrit_sur_la_CDGT_du_Sdrif_approuvé_par_décret_le_27_décembre_2013.shp")

commData <- merge(comm,data_PCR, by.x="insee", by.y = "ORI")
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
                  #mapflu {
                  position: absolute;
                  }
                  #mappot {
                  position: absolute;
                  }
                  #mapfluDom {
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
    condition = "input.tabs=='Flux'",
    leafletOutput("mapflu", width="100%", height = "100%")
  ),
  conditionalPanel(
    condition = "input.tabs=='Bassin'",
    leafletOutput("mappot", width="100%", height = "100%")
  ),
  conditionalPanel(
    condition = "input.tabs=='FluxDom'",
    leafletOutput("mapfluDom", width="100%", height = "100%")
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
                             
                             tabPanel("Mobilité", 
                                      radioButtons("radioMobi", label = NULL,
                                                   choices = list("Gravitation" = "Gravitation",
                                                                  "Auto-Contention" = "Contention",
                                                                  "Auto-Suffisance" = "Suffisance",
                                                                  "Dépendance" = "Dependance",
                                                                  "Mobilité" = "Mobilite"))
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
                                      actionButton("vis4_donne", "Détails techniques")
                              ),
                             
                             ##############################
                             ####### Panneau FluxDom  #####
                             ##############################
                             
                             tabPanel("FluxDom",
                                      radioButtons("radioFlu", label = NULL,
                                                   choices = list("Emploi" = "iEmploi",
                                                                  "Population" = "iPopulation",
                                                                  "Emploi et Population" = "iEmpPop"))
                                      )
                 )
  )
  )

server <- function(input, output, session) {
  
  v <- reactiveValues(data = commData$Gravitation)
  
  observeEvent(input$radioMobi,{
    if(input$radioMobi=="Gravitation"){
      v$data <- commData$Gravitation}
    if(input$radioMobi=="Contention"){
      v$data <- commData$AutoContention}
    if(input$radioMobi=="Suffisance"){
      v$data <- commData$AutoSuffisance}
    if(input$radioMobi=="Dependance"){
      v$data <- commData$Dependance}
    if(input$radioMobi=="Mobilite"){
      v$data <- commData$Mobilite}
  })
  
  f <- reactiveValues(dataflu = spLinksEmploi)
  r <- reactiveValues(rayon = rayonEmploi)
  c <- reactiveValues(col = totDes$col)
  vc <- reactiveValues(valCercle = totDes$EMPLOI)
  
  observeEvent(input$radioFlu,{
    if(input$radioFlu=="iEmploi"){
      f$dataflu <- spLinksEmploi
      r$rayon <- rayonEmploi
      c$col <- totDes$col
      vc$valCercle <- totDes$EMPLOI}
    if(input$radioFlu=="iPopulation"){
      f$dataflu <- spLinksPop
      r$rayon <- rayonPopulation
      c$col <- totOri$col
      vc$valCercle <- totOri$POPULATION}
    if(input$radioFlu=="iEmpPop"){
      f$dataflu <- spLinksEmploiPop
      r$rayon <- rayonEmploiPop
      c$col <- totOriDes$col
      vc$valCercle <- totOriDes$POPetEMPLOI}
  })
  
  ##############################
  #####  Map indicateurs   #####
  ##############################
  
  output$mapIndic <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(provider = "CartoDB.DarkMatter") %>%
      fitBounds(lng1 = 1.44, lat1 = 48.12, lng2 = 3.55, lat2 = 49.24)
  })
  
  observe({
    leafletProxy("mapIndic", data =commData) %>%
      clearShapes() %>%
      addPolygons(
        fillColor = ~colorBin(palette = "Purples",bins = getBreaks(v$data, 
        nclass = 6,method = "fisher-jenks"),domain = v$data)(v$data),
        weight = 1, opacity = 0.3,color = "white",fillOpacity = 0.5,
        highlight = highlightOptions(
          weight = 3,
          color = "white",
          opacity = 1,
          fillOpacity = 0.7,
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
      setMaxBounds(lng1 = 1.44,
                   lat1 = 48.12,
                   lng2 = 3.55,
                   lat2 = 49.23)
  })
  
  observe({
    proxy <- leafletProxy("mapIndic", data =commData)
    
    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearControls()
      proxy %>%  addLegend(pal = colorBin(palette = "Purples",
                bins = getBreaks(v$data,nclass = 6,method = "fisher-jenks"),
                domain = v$data,pretty = TRUE),values = ~v$data, opacity = 0.5,
        title = NULL, position = "bottomright")
  })
 
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
  #####     Map Bassin     #####
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
  #####    Map FluDom      #####
  ##############################
  
  output$mapfluDom <- renderLeaflet({
    leaflet() %>%
      addMapPane("background_map", zIndex = 410) %>%    # Level 1
      addMapPane("communes", zIndex = 420) %>%          # Level 2
      addMapPane("réseau_routier", zIndex = 430) %>%    # Level 3
      addMapPane("voie_ferré", zIndex = 440) %>%        # Level 4
      addMapPane("flux", zIndex = 450) %>%              # Level 5
      addMapPane("cercles", zIndex = 460) %>%           # Level 6
      
      addProviderTiles(provider = "CartoDB.DarkMatter",
                       options = pathOptions(pane = "background_map")) %>%
      
      addLayersControl(
        overlayGroups = c("Communes", "Réseau routier principal", "Réseau ferré"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>% 
      fitBounds(lng1 = 1.44, lat1 = 48.12, lng2 = 3.55, lat2 = 49.24)%>% 
      hideGroup("Réseau routier principal") %>% 
      hideGroup("Réseau ferré") %>% 
      hideGroup("Communes")
  })
  
  observe({
    leafletProxy(mapId = "mapfluDom") %>%
      clearShapes() %>%
      
      addPolygons(data = st_transform(commsf, crs = 4326), stroke = TRUE, weight = 0.5, opacity = 0.5, color = "grey", fill = TRUE,
                  fillColor = "ghostwhite", fillOpacity = 0, group = "Communes",
                  options = pathOptions(pane = "communes")) %>% 
      addPolylines(data = st_transform(routier, crs = 4326), color = "grey", opacity = 0.6, weight = 1.3 ,
                   stroke = TRUE, group = "Réseau routier principal",
                   options = pathOptions(pane = "réseau_routier")) %>% 
      addPolylines(data = st_transform(vferre, crs = 4326), color = "grey", opacity = 0.6, weight = 1.3 ,
                   stroke = TRUE, group = "Réseau ferré",  dashArray = 2,
                   options = pathOptions(pane = "voie_ferré")) %>% 
      addPolylines(data = st_transform(f$dataflu, crs = 4326), color = "white", opacity = 0.6, weight = f$dataflu$weight ,
                   stroke = TRUE,
                   options = pathOptions(pane = "flux")) %>% 
      addCircles(lng = totDes$lon, lat = totDes$lat, radius = r$rayon, color = c$col, stroke = F,
                 highlight = highlightOptions(
                   weight = 3,
                   color = "white",
                   opacity = 1,
                   fillOpacity = 0.7,
                   bringToFront = F),
                 label = sprintf(
                   "<strong>%s</strong><br/> Valeur : %.0f", 
                   totDes$nomcom,
                   vc$valCercle
                 )%>% lapply(htmltools::HTML),
                 labelOptions = labelOptions(
                   style = list("font-weight" = "normal", 
                                padding = "3px 8px"),
                   textsize = "15px",
                   direction = "auto"),
                 options = pathOptions(pane = "cercles")
      ) 
  })
  
  # observe({
  #   proxy <- leafletProxy(mapId = , data = )
  #   
  #   # Remove any existing legend, and only if the legend is
  #   # enabled, create a new one.
  #   proxy %>% clearControls()
  #   proxy %>%  addLegend()
  # })

  
  
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