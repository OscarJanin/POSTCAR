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
library(plotly)
library(igraph)
library(tidyverse)
library(toyspace)

############ LOAD DATA ############ 

pol <- readRDS("data/pol.Rds")
before <-  c(75101,75102,75103,75104,75105,75106,75107,75108,75109,75110,75111,75112,75113,75114,75115,75116,75117,75118,75119,75120)
after <- 75056
polAgr <- toyspace::pol_aggregate(before = before, after = after, pol = pol, idpol = "insee", namepol = "nomcom", nameAgr = "paris")

#Tableau avec les origines, destination, mode de transport et flux de naveteurs entre toute les communes 
tabFlows <- readRDS(file = "data/tabflows.Rds")
tabFlowsAgr <- toyspace::city_aggregate(before = before,after = after,tabflows = tabFlows,idori = "ORI",iddes = "DES")

tabFlowsAgrNoMode <- tabFlowsAgr %>% 
  group_by(ORIAGR, DESAGR) %>% 
  summarise(FLOW = sum(FLOW))

tabFlowsNoMode <- tabFlows %>% 
  group_by(ORI, DES) %>% 
  summarise(FLOW = sum(FLOW))

#Tableau avec pour chaque commune le total des flux sortant, le total des flux entrant, 
#et le total des flux intra
poptab <- toyspace::pop_tab(tabflows = tabFlows,idori = "ORI",iddes = "DES", idflow = "FLOW", iddist = "DIST")
poptabAgr <- toyspace::pop_tab(tabflows = tabFlowsAgr,idori = "ORIAGR",iddes = "DESAGR", idflow = "FLOW", iddist = "DIST")

#couches tiers (voies ferré, réseau routier, gares.)
vferre <- readRDS(file = "data/vferre.Rds")
routier <- readRDS(file = "data/routier.Rds")
station <- readRDS(file = "data/station.Rds")

#Coordonnées X Y des communes
coordCom <- toyspace::coord_com(pol = pol)
coordCom$LIBGEO <- paste(coordCom$insee, coordCom$nomcom, sep = ' - ')

#matrice des flux
matflow <- readRDS(file = "data/mat.Rds")


#listes des différentes combinaisons possible comportant chacune les données pour l'affichage du 3e onglet :
#Bassin d'habitation
listPotentials <- readRDS(file = "data/listpotentials.Rds")

############ LOAD FUNCTION ##########

commData <- toyspace::mob_indic(tabflows = tabFlows, idori = "ORI", iddes = "DES", idflow = "FLOW", pol = pol, idpol = "insee", iddist = "DIST")

domFlowJob <- toyspace::nystuen_dacey(tabflows = tabFlowsAgr, idori = "ORIAGR", iddes = "DESAGR", idflow = "FLOW",
                                      weight = "destination", threspct = 1, pol = polAgr, idpol = "IDAGR", iddist = "DIST")
domFlowPop <- toyspace::nystuen_dacey(tabflows = tabFlowsAgr, idori = "ORIAGR", iddes = "DESAGR", idflow = "FLOW",
                                      weight = "origin", threspct = 1, pol = polAgr, idpol = "IDAGR", iddist = "DIST")
domFlowJP <- toyspace::nystuen_dacey(tabflows = tabFlowsAgr, idori = "ORIAGR", iddes = "DESAGR", idflow = "FLOW",
                                     weight = "internal", threspct = 1, pol = polAgr, idpol = "IDAGR", iddist = "DIST")

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

# Create contour for potentials ----
PotentialContour <- function(ras) {
  potCont <- rasterToContourPoly(r = ras, nclass = 15)
  potContGeo <- st_as_sf(spTransform(potCont, CRSobj = CRS("+init=epsg:4326")))
  return(potContGeo)
}

# get top links ----
GetLinks <- function(tabnav, ref, mod, varsort, oneunit, thres){
  refLib <- paste0(ref, "LIB")
  oriDes <- paste0(c("ORI", "DES"), "LIB")
  invRef <- oriDes[oriDes != refLib]
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
  spLinks <- getLinkLayer(x = pol, df = tabSel[1:nbRows, c("ORI", "DES")])
  topDes <- spLinks
  return(topDes)
}
pol$insee
# get city value ----
city_Value <- function(matflow,spcom,od,city){
  if(od == "ORI"){
    tabCityFlow <- matflow[city,]
  }else{
    tabCityFlow <- matflow[,city]
  }  
  tabCityFlow <- as.data.frame(tabCityFlow)
  tabCityFlow$ID <- rownames(tabCityFlow)
  spcom <- dplyr::left_join(spcom,tabCityFlow, by = c("insee" ="ID")) 
  return(spcom)
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