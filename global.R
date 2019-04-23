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

############ LOAD DATA ############

shape <- readRDS(file = "data/communes.Rds")
shapeAgr <- readRDS(file = "data/communesAggrege.Rds")
poptab <- readRDS(file = "data/poptab.Rds")
poptabAgr <- readRDS(file = "data/poptabAgr.Rds")
tabFlows <- readRDS(file = "data/tabflows.Rds")
tabFlowsAgr <- readRDS(file = "data/tabflowsAgr.Rds")

tabFlowsAgrNoMode <- readRDS("data/tabflowsAgr.Rds") %>% 
  group_by(ORIAGR, DESAGR) %>% 
  summarise(FLOW = sum(FLOW))

tabFlowsNoMode <- readRDS("data/tabflows.Rds") %>% 
  group_by(ORI, DES) %>% 
  summarise(FLOW = sum(FLOW))

#les shapes doivent être de la forme S4 avec en premiere colonne, l'identifiant
shapeSf <- st_as_sf(shape)
shapeSf <- shapeSf[,c(3,2,1,4,5,6)]
commsf <- st_as_sf(shape)
commsf <- commsf[,c(3,2,1,4,5,6)]

vferre <- readRDS(file = "data/vferre.Rds")
routier <- readRDS(file = "data/routier.Rds")
coordCom <- readRDS(file = "data/coordcom.Rds")
station <- readRDS(file = "data/station.Rds")

listPotentials <- readRDS(file = "data/listpotentials.Rds")

mat75056 <- readRDS(file = "data/mat75056.Rds")
mat <- readRDS(file = "data/mat.Rds")
id <- "insee"


############ LOAD FUNCTION ##########

mobIndic <- function (matrix, shapesf, id){
  
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
  tabflow$Dependency <- tabflow$OriOriFlow / (tabflow$OriFlow + tabflow$OriOriFlow)
  tabflow$AutoSuff <- tabflow$OriOriFlow / (tabflow$DesFlow + tabflow$OriOriFlow)
  tabflow$Mobility <- (tabflow$DesFlow+tabflow$OriFlow) / (tabflow$OriFlow + tabflow$OriOriFlow)
  tabflow$RelBal <- (tabflow$DesFlow-tabflow$OriFlow) / (tabflow$OriFlow + tabflow$DesFlow)
  
  shapesf$idshp <- shapesf[[id]]
  shapeflow <- merge(x = shapesf,y = tabflow, by.x="idshp", by.y = "idflow")
  
  return(shapeflow)
}


nystuen_dacey <- function(
  tabflows,   # data.frame with commuting flows, long format (origin, destination, flow)
  poptab,     # table with population, flows summary (total at ori, des, intra) and core id
  idfield,    # CHAR, name of the id field in the population table (poptab)
  targetfield,# CHAR, name of the variable used for weighting
  threspct,    # threshold for defining max flow
  shape,
  shapeId
  )
{
  
  #prepare data
  colnames(tabflows) <- c("ORI", "DES", "FLOW")
  tabflows <- tabflows %>%    #on élimine les flux intra
    filter(ORI != DES)
  
  poptab <- poptab %>%  #on crée un tableau propre ne comportant que l'origine, destination et la variable choisie pour pondérer (WGT)
    transmute(ORI = poptab[, idfield],
              DES = poptab[, idfield],
              WGT = poptab[, targetfield])
  
  tabFlowsSum <- tabflows %>% #Tableau des sommes de flux à l'origine
    group_by(ORI) %>% 
    summarise(SUMFLOW = sum(FLOW, na.rm = TRUE))
  
  tabFlowsMax <- tabflows %>% #tableau des flux maximums à l'origine et du pourcentage que représente ce flux par rapport au flux total de la commune
    group_by(ORI) %>% 
    arrange(desc(FLOW)) %>% 
    slice(1) %>% 
    left_join(y = tabFlowsSum, by = "ORI") %>% 
    mutate(PCTMAX = FLOW / SUMFLOW) %>% 
    filter(PCTMAX > threspct)  #ne conserve que les communes dont le flux est superieur au seuil rentré au préalable
  
  tabFlowsAggr <- tabFlowsMax %>%   #Jointure des tableaux des flux maximum et de la variable de pondération (à l'origine et à la destination)
    left_join(x = ., y = poptab[, c("ORI", "WGT")], by = "ORI") %>% 
    left_join(x = ., y = poptab[, c("DES", "WGT")], by = "DES") 
  
  colnames(tabFlowsAggr)[6:7] <- c("WGTORI", "WGTDES")
  
  tabFlowsAggr <- tabFlowsAggr %>% filter(WGTORI < WGTDES) # ne garder que les flux dont la valeur de pondération à la destination est plus grande qu'a l'origine
  
  graphFlows <- graph.data.frame(d = tabFlowsAggr[, c("ORI", "DES")], directed = TRUE) #Pour chaque commune on représente le lien du flux le plus élevé
  V(graphFlows)$DEGIN <- degree(graphFlows, mode = "in")  #nombre de flux entrant par communes
  graphTab <- get.data.frame(x = graphFlows, what = "vertices")

  degSorted <- sort(V(graphFlows)$DEGIN, decreasing = TRUE) #on trie du plus grand au plus petit
  degSecond <- degSorted[2] + 1 #on dégage la seconde valeur la plus grande

  tabflows <- tabflows %>% # Obtenir le statut des flux d'une commune à l'autre
    left_join(y = graphTab, by = c("ORI" = "name")) %>%
    left_join(y = graphTab, by = c("DES" = "name")) %>%
    mutate(STATUSORI = ifelse(is.na(DEGIN.x) | DEGIN.x == 0, 0, ifelse(DEGIN.x == 1 | DEGIN.x == 2, 1, ifelse(DEGIN.x < degSecond, 2, 3))),
           STATUSDES = ifelse(is.na(DEGIN.y) | DEGIN.y == 0, 0, ifelse(DEGIN.y == 1 | DEGIN.y == 2, 1, ifelse(DEGIN.y < degSecond, 2, 3))),
           STATUS = paste(STATUSORI, STATUSDES, sep = "_")) %>%
    select(ORI, DES, STATUS)

  graphTab <- graphTab %>% #Obtenir le statut de pole d'emploi des communes (petit 1, moyen 2, grand 3)
    mutate(STATUS = ifelse(is.na(DEGIN) | DEGIN == 0, 0,
                           ifelse(DEGIN == 1 | DEGIN == 2, 1,
                                  ifelse(DEGIN < degSecond, 2, 3))))
  
  #Get geometry for tabflows
  spLinks <- getLinkLayer(x = shape, xid = shapeId, df = tabFlowsAggr[, c("ORI", "DES")], dfid = c("ORI", "DES"))
  spLinks$KEY <- paste(spLinks$ORI, spLinks$DES, sep = "_")
  tabflows$KEY <- paste(tabflows$ORI, tabflows$DES, sep = "_")
  tabflows <- left_join(spLinks, tabflows[, c("KEY", "STATUS")], by = "KEY")
  # tabflows <- left_join(spLinks, tabflows[,"KEY"], by = "KEY")
  tabflows$KEY <- NULL
  
  #Get geometry for graphTab
  shapeSf <- st_as_sf(shape)
  shapesfCent <- st_centroid(shapeSf)
  proj4string <- as.character(shape@proj4string)
  xy <- do.call(rbind, st_geometry(shapesfCent))
  shapesfCent$lon <- project(xy=xy, proj4string, inv = TRUE)[,1]
  shapesfCent$lat <- project(xy=xy, proj4string, inv = TRUE)[,2]
  graphTab <- transform(graphTab, name = as.numeric(name))
  graphTab <- left_join(graphTab, shapesfCent, by = c("name"= shapeId))
  graphTab <- transform(graphTab, name = as.character(name))
  graphTab <- left_join(graphTab, poptab, by = c("name"= "ORI"))
  
  return(list( PTS = graphTab, FLOWS = tabflows))
}

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
  spLinks <- getLinkLayer(x = commsf, df = tabSel[1:nbRows, c("ORI", "DES")])
  print(spLinks)
  spPol <- spcom[spcom$insee %in% spLinks$DES, ]
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