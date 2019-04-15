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


############ LOAD DATA ############
commsf <- readRDS(file = "data/communes.Rds")
vferre <- readRDS("data/vferre.Rds")
routier <- readRDS("data/routier.Rds")
coordCom <- readRDS(file = "data/coordcom.Rds")
shape <- readRDS(file = "data/communesAggrege.Rds")
listPotentials <- readRDS(file = "data/listpotentials.Rds")
tabFlows <- readRDS(file = "data/tabflows.Rds")

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

# weight choices between "job", "population", "job&pop"
domFlow <- function(mat, shape, id, weight){
  
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
  
  # Links Creation and merging with the flow matrix
  flowDomWide <- melt(data = flowDom, varnames = c("ORI", "DES"), value.name = "FLOW", as.is = TRUE) %>% filter(FLOW > 0)
  flowDomWide$KEY <- paste(flowDomWide$ORI, flowDomWide$DES, sep = "_")
  spLinks <- getLinkLayer(x = shape, xid = id, df = flowDomWide[, c("ORI", "DES")], dfid = c("ORI", "DES"))
  spLinks$KEY <- paste(spLinks$ORI, spLinks$DES, sep = "_")
  spLinks <- left_join(spLinks, flowDomWide[, c("KEY", "FLOW")], by = "KEY")
  
  #Create class for links break
  linksClass <- getBreaks(spLinks$FLOW, n=3, method = "fisher-jenks")
  
  # Line Weight
  spLinks$linweight<-  ifelse(spLinks$FLOW<linksClass[2],1,ifelse(spLinks$FLOW>=linksClass[2] & spLinks$FLOW<linksClass[3],10,20))
  
  
  ##Create Points##
  #Convert shape in a sf object so we can extract centroid in the X, Y format
  shapesf <- st_as_sf(shape)
  shapesfCent <- st_centroid(shapesf)
  xy <- do.call(rbind, st_geometry(shapesfCent)) %>% setNames(c("lon","lat"))
  
  proj4string <- as.character(shape@proj4string)
  shapesfCent$lon <- project(xy=xy, proj4string, inv = TRUE)[,1]
  shapesfCent$lat <- project(xy=xy, proj4string, inv = TRUE)[,2]
  shapesfCent <- transform(shapesfCent, lon = as.numeric(lon))
  shapesfCent <- transform(shapesfCent, lat = as.numeric(lat))
  
  #Create income and outcome values
  longMatrix <- melt(data = mat)
  colnames(longMatrix) <- c("ORI", "DES","FLOW")
  
  OriFlow <- longMatrix %>% group_by(ORI) %>% summarise(POPULATION = sum(FLOW))
  OriFlow <- transform(OriFlow, ORI = as.numeric(ORI))
  
  DesFlow <- longMatrix %>% group_by(DES) %>% summarise(JOB = sum(FLOW))
  DesFlow <- transform(DesFlow, DES = as.numeric(DES))
  
  pointFlow <- left_join(OriFlow, shapesfCent, by = c("ORI"= id))
  pointFlow <- left_join(pointFlow, DesFlow, by = c("ORI"= "DES"))
  pointFlow$POPJOB <- pointFlow$POPULATION + pointFlow$JOB
  
  #Create Circles colour
  fdom1 <- melt(flowDom)
  names(fdom1) <- c("i", "j", "fij")
  fdom1 <- fdom1[fdom1$fij > 0, ]
  fdom1 <- left_join(fdom1, OriFlow, by = c("i"="ORI"))
  pointFlow$col <- ""
  pointFlow[pointFlow$ORI %in% fdom1$j & !pointFlow$ORI %in% fdom1$i, "col"] <- "brown"
  pointFlow[pointFlow$ORI %in% fdom1$j & pointFlow$ORI %in% fdom1$i, "col"] <- "mediumorchid"
  pointFlow[!pointFlow$ORI %in% fdom1$j & pointFlow$ORI %in% fdom1$i, "col"] <- "cornflowerblue"
  pointFlow <- pointFlow[pointFlow$col != "", ]
  
  dfs <- list(pointFlow, spLinks)
  return(dfs)
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
