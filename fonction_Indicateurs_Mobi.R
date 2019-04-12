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

matrix <- mat

mobIndic <- function (matrix, shapesf, id){
  
  #Matrix trandformation into long format
  longMatrix <- melt(data = matrix, varnames = c("ORI", "DES"), value.name = "FLOW")
  
  #Store Origins to Origins Flow Value into a df name "tabflowOriOri"
  tabflowOriOri <- longMatrix %>% filter_( "ORI == DES")
  colnames(tabflowOriOri) <- c("ORI", "DES","OriOriFlow")
  
  #Store Origins Flow Value into a df name "tabflowOri"
  tabflowOri <-  longMatrix %>% filter_( "ORI != DES") %>% group_by(ORI) %>% summarise(OriFlow = sum(FLOW))
  
  #Store Destination Flow Value into a df name "tabflowDes"
  tabflowDes <-  longMatrix %>% filter_( "ORI != DES") %>% group_by(DES) %>% summarise(DesFlow = sum(FLOW))
  tabflow <- left_join(x = tabflowOriOri, y = tabflowOri, by = c("ORI","ORI"))
  tabflow <- left_join(x = tabflow, y = tabflowDes, by = c("DES","DES"))
  tabflow$DES <- NULL
  colnames(tabflow) <- c("idflow", "OriOriFlow","OriFlow", "DesFlow")
  
  #Building indicators
  #AutoContention 
  tabflow$Dependency <- tabflow$OriOriFlow / (tabflow$OriFlow + tabflow$OriOriFlow)
  #Auto sufficiency
  tabflow$AutoSuff <- tabflow$OriOriFlow / (tabflow$DesFlow + tabflow$OriOriFlow)
  #Mobility
  tabflow$Mobility <- (tabflow$DesFlow+tabflow$OriFlow) / (tabflow$OriFlow + tabflow$OriOriFlow)
  #Relative Balance
  tabflow$RelBal <- (tabflow$DesFlow-tabflow$OriFlow) / (tabflow$OriFlow + tabflow$DesFlow)
  
  #Join with pol
  shapesf$idshp <- shapesf[[id]]
  shapeflow <- merge(shapesf,tabflow, by.x="idshp", by.y = "idflow")
                
  return(shapeflow)
}



