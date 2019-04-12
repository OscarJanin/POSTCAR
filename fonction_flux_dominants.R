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



domFlow <- function(mat, shape, id, weight){
  # weight choices between "job", "population", "job&pop"
  
  if(weight=="job"){
    weight2 <- colSums(mat)
  } else if (weight=="population"){
    weight2 <- rowSums(mat)
  } else if (weight=="job&pop"){
    weight2 <- colSums(mat) + rowSums(mat)
  }
  
  shape$idshp <- shape[[id]]
  
  diag(mat) <- 0
  
  firstflows <- firstflows(mat = mat, method = "nfirst",k = 1)
  
  domflows <- domflows(mat = mat, w = weight2, k = 1)
  
  # Combine selections
  flowDom <- mat * firstflows * domflows
  
  ##################
  #   FlowDomJob
  ##################
  flowDomWide <- melt(data = flowDom, varnames = c("ORI", "DES"), value.name = "FLOW", as.is = TRUE) %>%
    filter(FLOW > 0)
  flowDomWide$KEY <- paste(flowDomWide$ORI, flowDomWide$DES, sep = "_")
  
  spLinks <- getLinkLayer(x = shape, xid = id, df = flowDomWide[, c("ORI", "DES")], dfid = c("ORI", "DES"))
  spLinks$KEY <- paste(spLinks$ORI, spLinks$DES, sep = "_")
  spLinks <- left_join(spLinks, flowDomWide[, c("KEY", "FLOW")], by = "KEY")
  
  ###############
  
  linksClass <- getBreaks(spLinks$FLOW, n=3, method = "fisher-jenks")
  
  ###### Line Weight
  spLinks$linweight<-  ifelse(spLinks$FLOW<linksClass[2],1,
                                 ifelse(spLinks$FLOW>=linksClass[2] & spLinks$FLOW<linksClass[3],10,20
                                 ))
  
  ###### Création des points ######
  
  #Convert shape in a sf object so we can extract centroid in the X, Y format
  shapesf <- st_as_sf(shape)
  shapesfCent <- st_centroid(shapesf)
  xy <- do.call(rbind, st_geometry(shapesfCent)) %>% setNames(c("lon","lat"))

  # Transformed data
  proj4string <- as.character(shape@proj4string)
  shapesfCent$lon <- project(xy=xy, proj4string, inv = TRUE)[,1]
  shapesfCent$lat <- project(xy=xy, proj4string, inv = TRUE)[,2]
  shapesfCent <- transform(shapesfCent, lon = as.numeric(lon))
  shapesfCent <- transform(shapesfCent, lat = as.numeric(lat))
  
  ###### total d'entrée et de sortie mergé avec le tableau
  
  longMatrix <- melt(data = mat)
  colnames(longMatrix) <- c("ORI", "DES","FLOW")
  
  OriFlow <- longMatrix %>% group_by(ORI) %>% summarise(POPULATION = sum(FLOW))
  OriFlow <- transform(OriFlow, ORI = as.numeric(ORI))
  
  DesFlow <- longMatrix %>% group_by(DES) %>% summarise(JOB = sum(FLOW))
  DesFlow <- transform(DesFlow, DES = as.numeric(DES))
  
  pointFlow <- left_join(OriFlow, shapesfCent, by = c("ORI"= id))
  pointFlow <- left_join(pointFlow, DesFlow, by = c("ORI"= "DES"))
  pointFlow$POPJOB <- pointFlow$POPULATION + pointFlow$JOB
  
  ##Création de données pour la couleur des cercles
  fdom1 <- melt(flowDom)
  names(fdom1) <- c("i", "j", "fij")
  fdom1 <- fdom1[fdom1$fij > 0, ]
  fdom1 <- left_join(fdom1, OriFlow, by = c("i"="ORI"))

  ###Création des couleurs pour pointFlow
  pointFlow$col <- ""
  pointFlow[pointFlow$ORI %in% fdom1$j & !pointFlow$ORI %in% fdom1$i, "col"] <- "brown"
  pointFlow[pointFlow$ORI %in% fdom1$j & pointFlow$ORI %in% fdom1$i, "col"] <- "mediumorchid"
  pointFlow[!pointFlow$ORI %in% fdom1$j & pointFlow$ORI %in% fdom1$i, "col"] <- "cornflowerblue"
  pointFlow <- pointFlow[pointFlow$col != "", ]
  
  dfs <- list(pointFlow, spLinks)
  return(dfs)
}


