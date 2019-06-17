shinyUI(bootstrapPage(
  theme = shinytheme("superhero"),
  useShinyjs(),
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  
  tags$style(HTML("
                  body {
                    font-family:'Helvetica';
                  }
                  #loading {
                  position: relative;
                  z-index : 1;
                  }
                  #Scénarii{
                  margin: auto.;
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
                  .panel-title {
                  text-align: center;
                  }
                  #tabPanel {
                  max-height: 90%;
                  overflow: auto;
                  }
                  .panel-group {
                    margin-bottom: 0px;
                  }
                  .leaflet .legend i{
                    border-radius: 50%;
                    width: 10px;
                    height: 10px;
                    margin-bottom: 10px;
                  }
                  #tabPanel{
                    font-weight: bold;
                  }
                  .panel-title{
                    font-weight: bold;
                  }
                  .nav-tabs > li:hover {
                   text-decoration: underline;
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
    condition = "input.tabs=='Indices'",
    leafletOutput("mapIndic", width="100%", height = "100%") 
  ),
  conditionalPanel(
    condition = "input.tabs=='Micro Flux'",
    leafletOutput("mapflu", width="100%", height = "100%") 
  ),
  conditionalPanel(
    condition = "input.tabs=='Structure'",
    leafletOutput("mappot", width="100%", height = "100%") 
  ),
  conditionalPanel(
    condition = "input.tabs=='Macro Flux'",
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
  absolutePanel( id = "tabPanel",
                 class = "panel panel-default",
                 style = "padding : 10px",
                 top = "2%", 
                 left = "2%",
                 right = "78%",
                 tabsetPanel(id = "tabs", 
                             
                             ####### Panneau Indice ##### 
                             tabPanel("Indices", 
                                      radioButtons("radioMobi", label = NULL,
                                                   choices = list("Emploi" = "emploi",
                                                                  "Population active" = "popact",
                                                                  "Solde Relatif" = "soldeRel",
                                                                  "Auto-Contention" = "Contention",
                                                                  "Auto-Suffisance" = "Suffisance",
                                                                  "Distance moyenne à l'origine" = "meanDistOri",
                                                                  "Distance moyenne à destination" = "meanDistDes",
                                                                  "Mobilité" = "Mobility",
                                                                  "Part des flux à l'origine" = "perOri",
                                                                  "Part des flux à la destination" = "perDes"
                                                   )),
                                      actionButton("index_descr", "Description")
                             ),
                             
                             ####### Panneau Micro Flux     #####
                             tabPanel("Micro Flux",
                                      selectInput("flucom", 
                                                  label = "Choisir une commune",
                                                  choices = sort(coordCom$LIBGEO),
                                                  selected = ""),
                                      radioButtons("fluref", label = "Origine ou destination", choices = c("Origine" = "ORI", "Destination" = "DES"), selected = "ORI"),
                                      radioButtons("flumod", label = "Mode de transport", choices = c("Tous modes" = "TOUT", "Transport en commun" = "TC", "Voiture" = "VP"), selected = "TOUT"),
                                      radioButtons("fluvar", label = "Quantité", choices = c("Nombre d'individus" = "FLOW", "Cumul de distance" = "DISTTOT"), selected = "FLOW"),
                                      sliderInput("fluthr", label = "Top", min = 2, max = 100, step = 1, value = 3),
                                      actionButton("flux_descr", "Description")
                             ),
                             
                             ####### Panneau Bassin   #####
                             tabPanel("Structure",
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
                                      actionButton("pool_descr", "Description")
                             ),
                             
                             ####### Panneau FluxDom  #####
                             tabPanel("Macro Flux",
                                      radioButtons("radioFlu", label = NULL,
                                                   choices = list("Emploi" = "iEmploi",
                                                                  "Population" = "iPopulation",
                                                                  "Flux intra-communaux" = "iEmpPop",
                                                                  "Flux intégrés" = "integrated",
                                                                  "Flux convergent" = "convergent",
                                                                  "Flux divergent" = "divergent"
                                                   )),
                                      actionButton("fludom_descr", "Description")
                             )
                             
                 )
                             ####### 
  )
  
  # ,
  # 
  # 
  # 
  #Graphic panel button display
  # absolutePanel( class = "panel panel-default",
  #                style = "padding : 10px",
  #                top = "2%",
  #                left = "78%",
  #                right = "2%",
  #                bsCollapse(id = "collapseExample", open = "Panel 2",
  #                           bsCollapsePanel("Graphiques",
  #                                           plotlyOutput("plot1")
  #                                           )
  #                           )
  # )

  ))