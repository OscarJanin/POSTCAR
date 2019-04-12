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
library(shinyBS)
library(shinyWidgets)
library(flows)
library(plotly)



commsf <- read_sf(dsn = "data/les-communes-generalisees-dile-de-france.shp")
vferre <- read_sf(dsn = "data/traces-du-reseau-ferre-idf.shp")
routier <- read_sf(dsn = "data/Réseau_routier_magistral_existant_de_la_Région_ÎledeFrance_inscrit_sur_la_CDGT_du_Sdrif_approuvé_par_décret_le_27_décembre_2013.shp")
coordCom <- readRDS(file = "data/coordcom.Rds")
pomaCom <- readRDS(file = "data/pomacom.Rds")
shape <- readOGR(dsn = "data/les-communes-generalisees-dile-de-france-parisagr2.shp")
listPotentials <- readRDS(file = "data/listpotentials.Rds")
tabFlows <- readRDS(file = "data/tabflows.Rds")

mat75056 <- readRDS(file = "data/mat75056")
mat <- readRDS(file = "data/mat")
id <- "insee"

commData <- mobIndic(matrix = mat, id = "insee", shapesf = commsf)

domFlowJob <- domFlow(mat = mat75056, shape = shape ,id = id, weight = "job")
domFlowPop <- domFlow(mat = mat75056, shape = shape ,id = id, weight = "population")
domFlowJP <- domFlow(mat = mat75056, shape = shape ,id = id, weight = "job&pop")


ui<- bootstrapPage(
  
  
  # element d'affichage de la page
  theme = shinytheme("superhero"),
  useShinyjs(),
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  
  tags$style(HTML("
                  #loading {
                  position: relative;
                  z-index : 1
                  }
                  #Scénarii{
                  margin: auto.
                  }
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
                  #graphPanel{
                  display: none
                  }
                  .panel-title {
                  text-align: center
                  }
                  #graphPanelButton {
                  text-align: center
                  }
                  ")),
  
  ## Loading wheel
  absolutePanel(top = "50%", 
                right = "50%",
                class = "panel panel-default",
                shinyjs::hidden(div(id = 'loading', addSpinner(div(), spin = "cube-grid", color = "#6495ED")))
                ),
  
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
  
  absolutePanel(id ="Scénarii",
                top = "2%", 
                right = "25%",
                left = "25%",
                class = "panel panel-default",
                style = "padding : 10px",
                bsCollapse(id = "collapseExample", open = "Panel 2",
                           bsCollapsePanel("Scénarios",
                                           # radioButtons(inputId = "reloc", 
                                           #              label = "Relocaliser les populations et les activités", 
                                           #              choices = c("Configuration actuelle" = "ACT", 
                                           #                          "Finger plan" = "FIN", 
                                           #                          "Transport-oriented development" = "TOD", 
                                           #                          "Polycentrisation" = "POL", 
                                           #                          "CBDsation" = "CBD"),
                                           #              selected = "ACT",
                                           #              inline = T, width = "100%"),
                                           # radioButtons(inputId = "equip", 
                                           #              label = "Relocaliser les équipements", 
                                           #              choices = c("Configuration actuelle" = "ACT", 
                                           #                          "Près des résidents" = "ORI",
                                           #                          "Près des emplois" = "DES", 
                                           #                          "Equilibre résidents-emplois" = "EQU"),
                                           #              selected = "ACT",
                                           #              inline = T, width = "100%"),
                                           # radioButtons(inputId = "excess", 
                                           #              label = "Agir sur les mobilités résidentielle et professionnelle", 
                                           #              choices = c("Configuration actuelle" = "ACT", 
                                           #                          "Échange d'emploi" = "CS1",
                                           #                          "Échange de logement" = "TYPL",
                                           #                          "Échange sans contrainte" = "GLO"),
                                           #              selected = "ACT",
                                           #              inline = T, width = "100%"),
                                           # radioButtons(inputId = "modetrans", 
                                           #              label = "Agir sur le mode de transport", 
                                           #              choices = c("Configuration actuelle" = "ACT", 
                                           #                          "Zéro voiture" = "ZVP", 
                                           #                          "Tout voiture" = "TVP",
                                           #                          "Zéro transport collectf" = "ZTC",
                                           #                          "Tout transport collectf" = "TTC",
                                           #                          "Zéro modes doux" = "ZNM",
                                           #                          "Tout modes doux" = "TNM"),
                                           #              selected = "ACT",
                                           #              inline = T, width = "100%")
                                           radioButtons(inputId = "reloc", 
                                                        label = "Relocaliser les populations et les activités",
                                                        choices = c("Configuration actuelle" = "ACT",
                                                                    "Finger plan" = "FIN"),
                                                        selected = "ACT",
                                                        inline = T, width = "100%"
                                                        ),
                                           radioButtons(inputId = "equip", 
                                                        label = "Relocaliser les équipements",
                                                        choices = c("Configuration actuelle" = "ACT",
                                                                    "Près des résidents" = "ORI"),
                                                        selected = "ACT",
                                                        inline = T, width = "100%"
                                           ),
                                           radioButtons(inputId = "modetrans", 
                                                        label = "Agir sur le mode de transport",
                                                        choices = c("Configuration actuelle" = "ACT",
                                                                    "Zéro voiture" = "ZVP"),
                                                        selected = "ACT",
                                                        inline = T, width = "100%"
                                           )
                           )
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
                 top = "2%", 
                 left = "2%",
                 right = "78%",
                 tabsetPanel(id = "tabs", 
                             
                             ##############################
                             ####### Panneau Mobilité #####
                             ##############################  
                             
                             tabPanel("Mobilité", 
                                      radioButtons("radioMobi", label = NULL,
                                                   choices = list("Gravitation" = "Gravitation",
                                                                  "Auto-Contention" = "Contention",
                                                                  "Auto-Suffisance" = "Suffisance",
                                                                  "Dépendance" = "Dependance"
                                                                  ))
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
  ,

  absolutePanel( id = "graphPanelButton",
                 class = "panel panel-default",
                 style = "padding : 10px",
                 top = "2%",
                 left = "78%",
                 right = "2%",
                 actionButton("button", "Graphiques")          
  ),
  absolutePanel( id = "graphPanel",
                 class = "panel panel-default",
                 style = "padding : 10px",
                 top = "12%",
                 left = "78%",
                 right = "2%",
                 draggable = F,
                 width = "20%",
                 fixed = T,
                 plotlyOutput("plot1")
  )
  )

server <- function(input, output, session) {
  
  # output$plot1 <- ggplot(data = tabflow, aes(x = tabflow$RelBal, y = tabflow$AutoSuff)) + geom_col()
  
  output$plot1 <- renderPlotly({
    plot_ly(as.data.frame(commData), x = ~RelBal, y = ~AutoSuff)
  })
  
  observeEvent(input$button, {
    toggle("graphPanel")
  })
  
  v <- reactiveValues(data = commData$RelBal)
  n <- reactiveValues(nom = "Gravitation")
  
  observeEvent(input$radioMobi,{
    if(input$radioMobi=="Gravitation"){
      v$data <- commData$RelBal
      n$nom <- "Gravitation : "}
    if(input$radioMobi=="Contention"){
      v$data <- commData$Mobility
      n$nom <- "Auto-Contention : "}
    if(input$radioMobi=="Suffisance"){
      v$data <- commData$AutoSuff
      n$nom <- "Auto-Suffisance : "}
    if(input$radioMobi=="Dependance"){
      v$data <- commData$Dependency
      n$nom <- "Dépendance : "}
  })
  

  
  f <- reactiveValues(dataflu = domFlowJob[[2]])
  r <- reactiveValues(rayon = (sqrt(domFlowJob[[1]][["JOB"]])/pi)*20)
  c <- reactiveValues(cercle = domFlowJob[[1]])
  vc <- reactiveValues(valCercle = domFlowJob[[1]][["JOB"]])
  nf <- reactiveValues(nom = "Emploi : ")
  nc <- reactiveValues(comm = domFlowJob[[1]][["nomcom"]])

  
  observeEvent(input$radioFlu,{
    if(input$radioFlu=="iEmploi"){
      
      f$dataflu <- domFlowJob[[2]]
      r$rayon <- (sqrt(domFlowJob[[1]][["JOB"]])/pi)*20
      c$cercle <- domFlowJob[[1]]
      vc$valCercle <- domFlowJob[[1]][["JOB"]]
      nf$nom <- "Emploi : "
      nc$comm <- domFlowJob[[1]][["nomcom"]]}
    if(input$radioFlu=="iPopulation"){
      f$dataflu <- domFlowPop[[2]]
      r$rayon <- (sqrt(domFlowPop[[1]][["POPULATION"]])/pi)*20
      c$cercle <- domFlowPop[[1]]
      vc$valCercle <- domFlowPop[[1]][["POPULATION"]]
      nf$nom <- "Population : "
      nc$comm <- domFlowPop[[1]][["nomcom"]]}
    if(input$radioFlu=="iEmpPop"){
      f$dataflu <- domFlowJP[[2]]
      r$rayon <- (sqrt(domFlowJP[[1]][["POPJOB"]])/pi)*20
      c$cercle <- domFlowJP[[1]]
      vc$valCercle <- domFlowJP[[1]][["POPJOB"]]
      nf$nom <- "Emploi et Population : "
      nc$comm <- domFlowJP[[1]][["nomcom"]]}
  })
  
  ##############################
  #####  Map indicateurs   #####
  ##############################
  
  output$mapIndic <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
      addMapPane("background_map", zIndex = 410) %>%  # Level 1
      addMapPane("réseau_routier", zIndex = 420) %>%  # Level 2
      addMapPane("voie_ferré", zIndex = 430) %>%      # Level 3
      addProviderTiles(provider = "Esri.WorldGrayCanvas") %>%
      addLayersControl(
        position = "bottomleft",
        overlayGroups = c("Réseau routier principal", "Réseau ferré"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>% 
      fitBounds(lng1 = 1.44, lat1 = 48.12, lng2 = 3.55, lat2 = 49.24)%>% 
      hideGroup("Réseau routier principal") %>% 
      hideGroup("Réseau ferré")
  })
  
  observe({
    
    shinyjs::showElement(id = 'loading')
    
    leafletProxy("mapIndic", data =commData) %>%
      clearShapes() %>%
      addPolylines(data = st_transform(routier, crs = 4326), color = "grey", opacity = 0.6, weight = 1.3 ,
                   stroke = TRUE, group = "Réseau routier principal",
                   options = pathOptions(pane = "réseau_routier")) %>% 
      addPolylines(data = st_transform(vferre, crs = 4326), color = "grey", opacity = 1, weight = 1 ,
                   stroke = TRUE, group = "Réseau ferré",  dashArray = 2,
                   options = pathOptions(pane = "voie_ferré")) %>%
      addPolygons(
        fillColor = ~colorBin(palette = "Purples",bins = getBreaks(v$data, 
        nclass = 6,method = "fisher-jenks"),domain = v$data)(v$data),
        weight = 0.7, opacity = 0.5,color = "grey",fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 2,
          color = "white",
          opacity = 0.7,
          fillOpacity = 1,
          bringToFront = TRUE),
        label = sprintf(
          "<strong>%s</strong><br/> %s %.2f",
          commData$nomcom, 
          n$nom,
          v$data
        )%>% lapply(htmltools::HTML),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", 
                       padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"),
        options = pathOptions(pane = "réseau_routier"))
    
    shinyjs::hideElement(id = 'loading')
  })
  
  observe({
    proxy <- leafletProxy("mapIndic", data =commData)
    
    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearControls()
      proxy %>%  addLegend(pal = colorBin(palette = "Purples", 
                bins = getBreaks(v$data,nclass = 6,method = "fisher-jenks"),
                domain = v$data,pretty = TRUE),values = ~v$data, opacity = 0.7,
        title = NULL, position = "bottomright")
      
  })
 
  ##############################
  #####      Map Flux      #####
  ##############################
  
  output$mapflu <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
      addMapPane("background_map", zIndex = 410) %>%    # Level 1
      addMapPane("communes", zIndex = 420) %>%          # Level 2
      addMapPane("réseau_routier", zIndex = 430) %>%    # Level 3
      addMapPane("voie_ferré", zIndex = 440) %>%        # Level 4
      addMapPane("comm", zIndex = 450) %>%              # Level 5
      addMapPane("lignes", zIndex = 460) %>%            # Level 6
      addProviderTiles(provider = "Esri.WorldGrayCanvas") %>%
      addLayersControl(
        position = "bottomleft",
        overlayGroups = c("Communes", "Réseau routier principal", "Réseau ferré"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>% 
      fitBounds(lng1 = 1.44, lat1 = 48.12, lng2 = 3.55, lat2 = 49.24)%>% 
      hideGroup("Réseau routier principal") %>% 
      hideGroup("Réseau ferré") %>% 
      hideGroup("Communes")
  })
  
  observe({
    shinyjs::showElement(id = 'loading')
    
    topDes <- GetTopLinks()
    leafletProxy("mapflu") %>%
      clearShapes() %>%
      addPolygons(data = st_transform(commsf, crs = 4326), stroke = TRUE, weight = 0.5, opacity = 0.5, color = "grey", fill = TRUE,
                  fillColor = "grey", fillOpacity = 0, group = "Communes",
                  options = pathOptions(pane = "communes")) %>% 
      addPolylines(data = st_transform(routier, crs = 4326), color = "grey", opacity = 0.6, weight = 1.3 ,
                   stroke = TRUE, group = "Réseau routier principal",
                   options = pathOptions(pane = "réseau_routier")) %>% 
      addPolylines(data = st_transform(vferre, crs = 4326), color = "grey", opacity = 0.6, weight = 1.3 ,
                   stroke = TRUE, group = "Réseau ferré",  dashArray = 2,
                   options = pathOptions(pane = "voie_ferré")) %>%
      addPolygons(data = topDes$POLYG, label = topDes$POLYG$LIBGEO, stroke = TRUE, weight = 1, color = "grey35", 
                  fill = TRUE, fillColor = "ghostwhite", fillOpacity = 0.3,options = pathOptions(pane = "comm")) %>%
      addPolylines(data = topDes$LINES, color = "Purple", opacity = 0.8, weight = 1.5, stroke = TRUE, options = pathOptions(pane = "lignes"))
    
    shinyjs::hideElement(id = 'loading')
  })
  
  ##############################
  #####     Map Bassin     #####
  ##############################
  
  output$mappot <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
      addMapPane("background_map", zIndex = 410) %>%    # Level 1
      addMapPane("communes", zIndex = 420) %>%          # Level 2
      addMapPane("réseau_routier", zIndex = 430) %>%    # Level 3
      addMapPane("voie_ferré", zIndex = 440) %>%    # Level 3
      addProviderTiles(provider = "Esri.WorldGrayCanvas") %>%
      addLayersControl(
        position = "bottomleft",
        overlayGroups = c("Communes", "Réseau routier principal", "Réseau ferré"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>% 
      fitBounds(lng1 = 1.44, lat1 = 48.12, lng2 = 3.55, lat2 = 49.24)%>% 
      hideGroup("Réseau routier principal") %>% 
      hideGroup("Réseau ferré") %>% 
      hideGroup("Communes")
  })
  
  observe({
    shinyjs::showElement(id = 'loading')
    
    if(input$pottyp == "dif"){
      leafletProxy("mappot") %>%
        clearShapes() %>%
        clearImages() %>% clearShapes() %>% clearControls() %>%
        addPolygons(data = st_transform(commsf, crs = 4326), stroke = TRUE, weight = 0.5, opacity = 0.5, color = "grey", fill = TRUE,
                    fillColor = "grey", fillOpacity = 0, group = "Communes",
                    options = pathOptions(pane = "communes")) %>% 
        addPolylines(data = st_transform(routier, crs = 4326), color = "grey", opacity = 0.6, weight = 1.3 ,
                     stroke = TRUE, group = "Réseau routier principal",
                     options = pathOptions(pane = "réseau_routier")) %>% 
        addPolylines(data = st_transform(vferre, crs = 4326), color = "grey", opacity = 0.6, weight = 1.3 ,
                     stroke = TRUE, group = "Réseau ferré",  dashArray = 2,
                     options = pathOptions(pane = "voie_ferré")) %>%
        addRasterImage(x = SelecPotential(), colors = PotentialPalette(SelecPotential()), opacity = 0.4) %>%
        addLegend(position = "topright",
                  colors = c("#B22222", "#E5E5E5", "#000080"),
                  labels = c("Surplus d'emplois (déficit d'actifs)",
                             "Équilibre actifs-emplois",
                             "Surplus d'actifs (déficit d'emplois)"))
    } else if(input$pottyp == "ori"){
      leafletProxy("mappot") %>%
        clearShapes() %>%
        clearImages() %>% clearShapes() %>% clearControls() %>%
        addPolygons(data = st_transform(commsf, crs = 4326), stroke = TRUE, weight = 0.5, opacity = 0.5, color = "grey", fill = TRUE,
                    fillColor = "grey", fillOpacity = 0, group = "Communes",
                    options = pathOptions(pane = "communes")) %>% 
        addPolylines(data = st_transform(routier, crs = 4326), color = "grey", opacity = 0.6, weight = 1.3 ,
                     stroke = TRUE, group = "Réseau routier principal",
                     options = pathOptions(pane = "réseau_routier")) %>% 
        addPolylines(data = st_transform(vferre, crs = 4326), color = "grey", opacity = 0.6, weight = 1.3 ,
                     stroke = TRUE, group = "Réseau ferré",  dashArray = 2,
                     options = pathOptions(pane = "voie_ferré")) %>%
        addRasterImage(x = sqrt(SelecPotential()), colors = PotentialPalette(sqrt(SelecPotential())), opacity = 0.4) %>%
        addPolygons(data = DrawContour(), stroke = TRUE, fill = FALSE, color = "#a9a9a9", weight = 2,
                    label = paste(as.character(round(DrawContour()$center^2)), "actifs")) %>%
        addLegend(position = "topright",
                  colors = c("#B22222", "#E5E5E5"),
                  labels = c("Forte densité d'actifs",
                             "Faible densité d'actifs"))
    } else {
      leafletProxy("mappot") %>%
        clearShapes() %>%
        clearImages() %>% clearShapes() %>% clearControls() %>%
        addPolygons(data = st_transform(commsf, crs = 4326), stroke = TRUE, weight = 0.5, opacity = 0.5, color = "grey", fill = TRUE,
                    fillColor = "grey", fillOpacity = 0, group = "Communes",
                    options = pathOptions(pane = "communes")) %>% 
        addPolylines(data = st_transform(routier, crs = 4326), color = "grey", opacity = 0.6, weight = 1.3 ,
                     stroke = TRUE, group = "Réseau routier principal",
                     options = pathOptions(pane = "réseau_routier")) %>% 
        addPolylines(data = st_transform(vferre, crs = 4326), color = "grey", opacity = 0.6, weight = 1.3 ,
                     stroke = TRUE, group = "Réseau ferré",  dashArray = 2,
                     options = pathOptions(pane = "voie_ferré")) %>%
        addRasterImage(x = sqrt(SelecPotential()), colors = PotentialPalette(sqrt(SelecPotential())), opacity = 0.4) %>%
        addPolygons(data = DrawContour(), stroke = TRUE, fill = FALSE, color = "#a9a9a9", weight = 2,
                    label = paste(as.character(round(DrawContour()$center^2)), "emplois")) %>%
        addLegend(position = "topright",
                  colors = c("#B22222", "#E5E5E5"),
                  labels = c("Forte densité d'emplois (destination)",
                             "Faible densité d'emplois (destination)"))
    }
    
    shinyjs::hideElement(id = 'loading')
  })
  
  ##############################
  #####    Map FluDom      #####
  ##############################
  
  output$mapfluDom <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = FALSE, incl.data=TRUE)) %>%
      addMapPane("background_map", zIndex = 410) %>%    # Level 1
      addMapPane("communes", zIndex = 420) %>%          # Level 2
      addMapPane("réseau_routier", zIndex = 430) %>%    # Level 3
      addMapPane("voie_ferré", zIndex = 440) %>%        # Level 4
      addMapPane("cercles", zIndex = 460) %>%           # Level 5
      addMapPane("flux", zIndex = 450) %>%              # Level 6
      
      addProviderTiles(provider = "Esri.WorldGrayCanvas",
                       options = pathOptions(pane = "background_map")) %>%
      
      addLayersControl(
        position = "bottomleft",
        overlayGroups = c("Communes", "Réseau routier principal", "Réseau ferré"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>% 
      fitBounds(lng1 = 1.44, lat1 = 48.12, lng2 = 3.55, lat2 = 49.24)%>% 
      hideGroup("Réseau routier principal") %>% 
      hideGroup("Réseau ferré") %>% 
      hideGroup("Communes")
  })
  
  observe({
    shinyjs::showElement(id = 'loading')
    
    leafletProxy(mapId = "mapfluDom", data = c(f$dataflu,r$rayon,c$col)) %>% 
      clearShapes() %>%
      
      addPolygons(data = st_transform(commsf, crs = 4326), stroke = TRUE, weight = 0.5, opacity = 0.5, color = "grey", fill = TRUE,
                  fillColor = "grey", fillOpacity = 0, group = "Communes",
                  options = pathOptions(pane = "communes")) %>% 
      addPolylines(data = st_transform(routier, crs = 4326), color = "grey", opacity = 0.6, weight = 1.3 ,
                   stroke = TRUE, group = "Réseau routier principal",
                   options = pathOptions(pane = "réseau_routier")) %>% 
      addPolylines(data = st_transform(vferre, crs = 4326), color = "grey", opacity = 0.6, weight = 1.3 ,
                   stroke = TRUE, group = "Réseau ferré",  dashArray = 2,
                   options = pathOptions(pane = "voie_ferré")) %>% 
      addPolylines(data = st_transform(f$dataflu, crs = 4326), color = "royalblue", opacity = 0.1, weight = f$dataflu[["linweight"]] ,
                   stroke = TRUE, 
                   # highlight = highlightOptions(
                   #   weight = 2,
                   #   color = "royalblue",
                   #   opacity = 1,
                   #   fillOpacity = 0.6,
                   #   bringToFront = F),
                   options = pathOptions(pane = "flux")) %>% 
      addCircles(lng = c$cercle[["lon"]], 
                 lat = c$cercle[["lat"]], 
                 radius = r$rayon, 
                 color = c$cercle[["col"]], 
                 stroke = F,
                 fillOpacity = 0.3,
                 highlight = highlightOptions(
                   weight = 3,
                   color = "white",
                   opacity = 1,
                   fillOpacity = 0.8,
                   bringToFront = F),
                 label = sprintf(
                   "<strong>%s</strong><br/> %s %.0f", 
                   nc$comm,
                   nf$nom,
                   vc$valCercle
                 )%>% lapply(htmltools::HTML),
                 labelOptions = labelOptions(
                   style = list("font-weight" = "normal", 
                                padding = "3px 8px"),
                   textsize = "15px",
                   direction = "auto"),
                 options = pathOptions(pane = "cercles")
      )
    
    shinyjs::hideElement(id = 'loading')
  })

  
  
  ##############################
  #####      Function      #####
  ##############################
  
  
  ###Fonction pour chopper tableau dans la liste
  GetAggregates <- reactive({
    keyCom <- paste(input$modetrans, input$reloc, input$excess, sep = "_")
    keyOther <- paste(input$modetrans, input$equip, input$reloc, input$excess, sep = "_")
    comConf <- listCommuteAggregates[[keyCom]]
    comRef <- listCommuteAggregates[["ACT_ACT_ACT"]]
    otherConf <- listOtherAggregates[[keyOther]]
    otherRef <- listOtherAggregates[["ACT_ACT_ACT_ACT"]]
    setOfPlots <- PlotAggrDist(comconf = comConf, otherconf = otherConf, comref = comRef, otherref = otherRef)
    return(setOfPlots)
  })
  
  
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