library(reshape2)         # transformation format long, format large
library(sp)               # objets spatiaux
library(rgdal)            # fonctions de la bibliothèque GDAL
library(ggplot2)          # fonctions graphiques
library(ggthemes)         # thèmes pour ggplot
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

commData <- merge(comm,tabflow6, by.x="insee", by.y = "ORI")

coordCom <- coordcom



ui<- bootstrapPage(
  # element d'affichage de la page
  useShinyjs(),
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  
       ##############################
  #######            Map           #######
       ############################## 
  

  leafletOutput("mymap", width="100%", height = "100%"),
  
  
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
                 top = "10%", 
                 left = "2%",
    tabsetPanel(id = "tabset1", 
                       
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
  #Indiquez ici le chemin de tout les attributs a cartographier
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
  
  

  output$mymap <- renderLeaflet({
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
}

# labels <- sprintf(
#   "<strong>%s</strong><br/> valeur : %g",
#   commData$nomcom, commData$Mobilite
# ) %>% lapply(htmltools::HTML)

shinyApp(ui = ui, server = server)

