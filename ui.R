shinyUI(bootstrapPage(
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
  
  # Loading wheel
  absolutePanel(top = "50%", 
                right = "50%",
                class = "panel panel-default",
                shinyjs::hidden(div(id = 'loading', addSpinner(div(), spin = "cube-grid", color = "#6495ED")))
  ),
  
  #Display maps through the panel selection
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
  
  #Scenarios panel
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
  
  #indicators panel
  absolutePanel( class = "panel panel-default",
                 style = "padding : 10px",
                 top = "2%", 
                 left = "2%",
                 right = "78%",
                 tabsetPanel(id = "tabs", 
                             
                             ####### Panneau Mobilité ##### 
                             tabPanel("Mobilité", 
                                      radioButtons("radioMobi", label = NULL,
                                                   choices = list("Solde Relatif" = "soldeRel",
                                                                  "Auto-Contention" = "Contention",
                                                                  "Auto-Suffisance" = "Suffisance",
                                                                  "Mobilité" = "Mobility"
                                                   ))
                             ),
                             
                             ####### Panneau Flux     #####
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
                             
                             ####### Panneau Bassin   #####
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
                             
                             ####### Panneau FluxDom  #####
                             tabPanel("FluxDom",
                                      radioButtons("radioFlu", label = NULL,
                                                   choices = list("Emploi" = "iEmploi",
                                                                  "Population" = "iPopulation",
                                                                  "Emploi et Population" = "iEmpPop"))
                             )
                 )
  ),
  
  #Graphic panel button display
  absolutePanel( id = "graphPanelButton",
                 class = "panel panel-default",
                 style = "padding : 10px",
                 top = "2%",
                 left = "78%",
                 right = "2%",
                 actionButton("button", "Graphiques")          
  ),
  
  #Graphic panel 
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
  )))