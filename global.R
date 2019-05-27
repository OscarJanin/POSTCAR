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

#shape de la forme S4

shape <- readRDS(file = "data/communes.Rds")
shape$nomcom <- as.character(shape$nomcom)
Encoding(shape$nomcom) <- "UTF-8"

shapeAgr <- readRDS(file = "data/communesAggrege.Rds")

#les shapes doivent être de la forme S4 avec en premiere colonne, l'identifiant
shapeSf <- st_as_sf(shape)
shapeSf <- shapeSf[,c(3,2,1,4,5,6)]

#Tableau avec pour chaque commune le total des flux sortant, le total des flux entrant, 
#et le total des flux intra
poptab <- readRDS(file = "data/poptab.Rds")
poptabAgr <- readRDS(file = "data/poptabAgr.Rds")

#Tableau avec les origines, destination, mode de transport et flux de naveteurs entre toute les communes 
tabFlows <- readRDS(file = "data/tabflows.Rds")
tabFlowsAgr <- readRDS(file = "data/tabflowsAgr.Rds")

tabFlowsAgrNoMode <- readRDS("data/tabflowsAgr.Rds") %>% 
  group_by(ORIAGR, DESAGR) %>% 
  summarise(FLOW = sum(FLOW))

tabFlowsNoMode <- readRDS("data/tabflows.Rds") %>% 
  group_by(ORI, DES) %>% 
  summarise(FLOW = sum(FLOW))

id <- "insee"

#couches tiers (voies ferré, réseau routier, gares.)
vferre <- readRDS(file = "data/vferre.Rds")
routier <- readRDS(file = "data/routier.Rds")
station <- readRDS(file = "data/station.Rds")

#Coordonnées X Y des communes
coordCom <- readRDS(file = "data/coordcom.Rds")
coordCom$LIBGEO <- paste(coordCom$CODGEO, coordCom$LIBGEO, sep = ' - ')

#matrice des flux
matflow <- readRDS(file = "data/mat.Rds")


#listes des différentes combinaisons possible comportant chacune les données pour l'affichage du 3e onglet :
#Bassin d'habitation
listPotentials <- readRDS(file = "data/listpotentials.Rds")

### Making Data for the server
#Creation of variables specifically
commData <- mobIndic(tabFlows = tabFlows, id = "insee", shapeSf = shapeSf)

domFlowJob <- nystuen_dacey(tabFlowsAgrNoMode, poptabAgr, idfield = "insee", targetfield  = "TOTDES", threspct = 0, shapeAgr, shapeId = "insee")
domFlowPop <- nystuen_dacey(tabFlowsAgrNoMode, poptabAgr, idfield = "insee", targetfield = "TOTORI", threspct = 0, shapeAgr, shapeId = "insee")
domFlowJP <- nystuen_dacey(tabFlowsAgrNoMode, poptabAgr, idfield = "insee", targetfield = "TOTINTRA", threspct = 0, shapeAgr, shapeId = "insee")

############ LOAD FUNCTION ##########

mobIndic <- function (tabFlows, shapeSf, id){
  ###FLOW #### 
  #Store Origins to Origins Flow Value into a df name "tabflowOriOri"
  tabflowOriOri <- tabFlows %>% filter_( "ORI == DES") %>% group_by(ORI,DES) %>%summarise(OriOriFlow = sum(FLOW))
  
  #Store Origins Flow Value into a df name "tabflowOri"
  tabflowOri <-  tabFlows %>% filter_( "ORI != DES") %>% group_by(ORI) %>% summarise(OriFlow = sum(FLOW))
  
  #Store Destination Flow Value into a df name "tabflowDes"
  tabflowDes <-  tabFlows %>% filter_( "ORI != DES") %>% group_by(DES) %>% summarise(DesFlow = sum(FLOW))
  tabflow <- left_join(x = tabflowOriOri, y = tabflowOri, by = c("ORI","ORI"))
  tabflow <- left_join(x = tabflow, y = tabflowDes, by = c("DES","DES"))
  tabflow$DES <- NULL
  colnames(tabflow) <- c("idflow", "OriOriFlow","OriFlow", "DesFlow")
  
  ### DIST#### 
  #Store Origins to Origins Flow Value into a df name "tabflowOriOri"
  tabdistOriOri <- tabFlows %>% filter_( "ORI == DES") %>% group_by(ORI,DES) %>%summarise(OriOriFlow = sum(DISTTOT))
  
  #Store Origins Flow Value into a df name "tabflowOri"
  tabdistOri <-  tabFlows %>% filter_( "ORI != DES") %>% group_by(ORI) %>% summarise(OriFlow = sum(DISTTOT))
  
  #Store Destination Flow Value into a df name "tabflowDes"
  tabdistDes <-  tabFlows %>% filter_( "ORI != DES") %>% group_by(DES) %>% summarise(DesFlow = sum(DISTTOT))
  tabdist <- left_join(x = tabdistOriOri, y = tabdistOri, by = c("ORI","ORI"))
  tabdist <- left_join(x = tabdist, y = tabdistDes, by = c("DES","DES"))
  tabdist$DES <- NULL
  colnames(tabdist) <- c("idflow", "OriOriDist","OriDist", "DesDist")
  
  tabflow <- left_join(x = tabdist, y = tabflow, by = c("idflow","idflow"))
  
  #Building indicators
  tabflow[is.na(tabflow)] <- 0
  tabflow$Dependency <- tabflow$OriOriFlow / (tabflow$OriFlow + tabflow$OriOriFlow)
  tabflow$AutoSuff <- tabflow$OriOriFlow / (tabflow$DesFlow + tabflow$OriOriFlow)
  tabflow$Mobility <- (tabflow$DesFlow+tabflow$OriFlow) / (tabflow$OriFlow + tabflow$OriOriFlow)
  tabflow$RelBal <- (tabflow$DesFlow-tabflow$OriFlow) / (tabflow$OriFlow + tabflow$DesFlow)
  tabflow$meanDist <- tabflow$OriDist/tabflow$OriFlow
  tabflow$perOri <- (tabflow$OriFlow*100)/sum(tabflow$OriFlow)
  tabflow$perDes <- (tabflow$DesFlow*100)/sum(tabflow$DesFlow)
  tabflow$perIntra <- (tabflow$OriOriFlow*100)/sum(tabflow$OriOriFlow)
  tabflow[is.na(tabflow)] <- 0
  
  shapeSf$idshp <- shapeSf[[id]]
  shapeflow <- merge(x = shapeSf,y = tabflow, by.x="idshp", by.y = "idflow")
  
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
  shapeSfCent <- st_centroid(shapeSf)
  proj4string <- as.character(shape@proj4string)
  xy <- do.call(rbind, st_geometry(shapeSfCent))
  shapeSfCent$lon <- project(xy=xy, proj4string, inv = TRUE)[,1]
  shapeSfCent$lat <- project(xy=xy, proj4string, inv = TRUE)[,2]
  graphTab <- transform(graphTab, name = as.numeric(name))
  graphTab <- left_join(graphTab, shapeSfCent, by = c("name"= shapeId))
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

# Create contour for potentials ----
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
  spLinks <- getLinkLayer(x = shapeSf, df = tabSel[1:nbRows, c("ORI", "DES")])
  print(spLinks)
  spPol <- spcom[spcom$insee %in% spLinks$DES, ]
  topDes <- list(POLYG = spPol, LINES = spLinks)
  return(topDes)
}

# get city value ----
city_Value <- function(matflow,spcom,od,city){
  if(od == "ORI"){
    tabCityFlow <- matflow[city,]
  }else{
    tabCityFlow <- matflow[,city]
  }  
  tabCityFlow <- as.data.frame(tabCityFlow)
  tabCityFlow$ID <- rownames(tabCityFlow)
  tabCityFlow$ID <- as.numeric(tabCityFlow$ID)
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