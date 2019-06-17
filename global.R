library(dplyr)
library(ggplot2)
library(shinythemes)
library(shinyjs)
library(shinyWidgets)
library(leaflet)
library(shinyBS)
library(plotly)
library(cartography)
library(raster)
library(SpatialPosition)
library(reshape2)

library(sf)
library(igraph)
library(dodgr)


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

# road <- read_sf("data/gis_osm_roads_free_1.shp")
# target <- c("motorway", "trunk", "primary", "secondary", "tertiary", "motorway_link", "trunk_link", "primary_link", "secondary_link", "tertiary_link")
# road <- road %>% filter(fclass %in% target)
# matDist <- toyspace::routing_machine(road = road, pol = pol, idpol = "insee")
# saveRDS(matDist,"data/matDist.Rds")
matDist <- readRDS(file = "data/matDist.Rds")


tabDist <- melt(matDist, value.name = "DIST", as.is = TRUE)
colnames(tabDist) <- c("ORI","DES","DIST")


#listes des différentes combinaisons possible comportant chacune les données pour l'affichage du 3e onglet :
#Bassin d'habitation
listPotentials <- readRDS(file = "data/listpotentials.Rds")

############ LOAD FUNCTION ############

commData <- toyspace::mob_indic(tabflows = tabFlows, idori = "ORI", iddes = "DES", idflow = "FLOW", pol = pol, idpol = "insee", iddist = "DIST")

# domFlowJob <- toyspace::nystuen_dacey(tabflows = tabFlowsAgr, idori = "ORIAGR", iddes = "DESAGR", idflow = "FLOW",
#                                       weight = "destination", threspct = 1, pol = polAgr, idpol = "IDAGR", iddist = "DIST")
domFlowJob <- readRDS("data/domFlowJob.Rds")
# domFlowPop <- toyspace::nystuen_dacey(tabflows = tabFlowsAgr, idori = "ORIAGR", iddes = "DESAGR", idflow = "FLOW",
#                                       weight = "origin", threspct = 1, pol = polAgr, idpol = "IDAGR", iddist = "DIST")
domFlowPop <- readRDS("data/domFlowPop.Rds")
# domFlowJP <- toyspace::nystuen_dacey(tabflows = tabFlowsAgr, idori = "ORIAGR", iddes = "DESAGR", idflow = "FLOW",
#                                      weight = "internal", threspct = 1, pol = polAgr, idpol = "IDAGR", iddist = "DIST")
domFlowJP <- readRDS("data/domFlowJP.Rds")
# icdr <- toyspace::label_icdr(tabflows = tabFlowsAgrNoMode, idori = "ORIAGR",iddes = "DESAGR", idflow = "FLOW", pol = polAgr, idpol = "IDAGR")
icdr <- readRDS(file = "data/icdr.Rds")

icdrI <- icdr %>% dplyr::filter(ICDR == "Integrated")
icdrC <- icdr %>% dplyr::filter(ICDR == "Convergent")
icdrD <- icdr %>% dplyr::filter(ICDR == "Divergent")

# domFlowJPCercle <- domFlowJP[[1]]

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

# get city value ----
city_Value <- function(tabflows,matDist,pol,idpol,var, od, mod, city){
  if(mod == "TC"){
    tabflows <- tabflows %>% filter(MODE == "TC")
  }else if (mod == "VP"){
    tabflows <- tabflows %>% filter(MODE == "VP")
  }else {}
  tabIndex <- expand.grid(ORI = pol[[idpol]],
                          DES = pol[[idpol]],
                          stringsAsFactors = FALSE)
  tabIndex <- left_join(x = tabIndex, y = tabflows, by = c("ORI", "DES"))
  if(od == "ORI"){
    matflow <- dcast(tabIndex, formula = DES ~ ORI, value.var = "FLOW", fun.aggregate = sum)
    matflow <- as.matrix(matflow[, -1])
    matflow[is.na(matflow)] <- 0
    tabCityDist <- as.data.frame(matDist[city,])
    tabCityFlow <- as.data.frame(matflow[,city])
  }else{
    matflow <- dcast(tabIndex, formula = ORI ~ DES, value.var = "FLOW", fun.aggregate = sum)
    matflow <- as.matrix(matflow[, -1])
    matflow[is.na(matflow)] <- 0
    tabCityDist <- as.data.frame(matDist[,city])
    tabCityFlow <- as.data.frame(matflow[,city])
  }
  colnames(tabCityFlow)<-"DATA"
  colnames(tabCityDist)<-"DIST"
  tabCityFlow$ID <- colnames(matflow)
  tabCityDist$ID <- rownames(tabCityDist)
  tabCityDist <- dplyr::left_join(tabCityFlow,tabCityDist, by = "ID")
  tabCityDist$DATA <- tabCityDist$DATA * tabCityDist$DIST
  if(var == "FLOW"){
    tabCity <- tabCityFlow
    index <- "actifs"
  }else{
    tabCity <- tabCityDist
    index <- "km"
  }
  spcom <- dplyr::left_join(pol,tabCity, by = c("insee" ="ID"))
  return(list( SPCOM = spcom, INDEX = index))
}

addLegendCustom <- function(map, colors, labels, sizes, opacity = 0.8){
  colorAdditions <- paste0(colors, "; width:", sizes, "px; height:", sizes, "px")
  labelAdditions <- paste0("<div style='display: inline-block;height: ", sizes, "px;margin-bottom: 10px;line-height: ", sizes, "px;'>", labels, "</div>")
  return(addLegend(map, colors = colorAdditions, labels = labelAdditions, opacity = opacity, position = "bottomright"))
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