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
library(flows)

library(sf)
library(igraph)
library(dodgr)


############ LOAD DATA ############ 

before <-  c(75101,75102,75103,75104,75105,75106,75107,75108,75109,75110,75111,75112,75113,75114,75115,75116,75117,75118,75119,75120)
after <- 75056

###prepare tabflows

# tabIndiv <- tabIndiv %>%
#   filter(substr(COMMUNE, 1, 2) %in% c("75", "77", "78", "91", "92", "93", "94", "95") & substr(DCLT, 1, 2) %in% c("75", "77", "78", "91", "92", "93", "94", "95")) %>%
#   mutate(COMCORR = ifelse(COMMUNE == "75056", ARM, COMMUNE))
# tabIndiv$MODE <- ifelse(tabIndiv[["TRANS"]] == 1, "DOMICILE", ifelse(tabIndiv[["TRANS"]] == 2 | tabIndiv[["TRANS"]] == 3, "NM", ifelse(tabIndiv[["TRANS"]] == 4, "VP", "TC")))
# saveRDS(tabIndiv, "data/tabIndiv.Rds")
# tabIndiv <- readRDS("data/tabIndiv.Rds")

pol <- readRDS("data/pol.Rds")

# tabFlows <- toyspace::create_tabflows(tabindiv = tabIndiv, idori = "COMCORR", iddes = "DCLT", idmode = "MODE", varwgt = "IPONDI",idcsp = "CS1")

# road <- read_sf("data/gis_osm_roads_free_1.shp")
# target <- c("motorway", "trunk", "primary", "secondary", "tertiary", "motorway_link", "trunk_link", "primary_link", "secondary_link", "tertiary_link")
# road <- road %>% filter(fclass %in% target)
# matDist <- toyspace::routing_machine(road = road, pol = pol, idpol = "insee")
# saveRDS(matDist,"data/matDist.Rds")
matDist <- readRDS(file = "data/matDist.Rds")

tabDist <- melt(matDist, value.name = "DIST", as.is = TRUE)
colnames(tabDist) <- c("ORI","DES","DIST")

#join tabDist & tabFlow 
# tabDist$KEY <- paste(tabDist$ORI, tabDist$DES, sep = '_')
# tabFlows$KEY <- paste(tabFlows$ORI, tabFlows$DES, sep = '_')
# tabFlows <- left_join(tabFlows, tabDist[,c("KEY","DIST")], "KEY")
# tabFlows$KEY <- NULL
# tabFlows$DISTTOT <- tabFlows$DIST*tabFlows$FLOW
# 
# #get nomcom for tabFlows
# tabFlows <- left_join(tabFlows, pol[,c("insee","nomcom")], c("ORI"="insee"))
# tabFlows <- left_join(tabFlows, pol[,c("insee","nomcom")], c("DES"="insee"))
# tabFlows$geometry.x <- NULL
# tabFlows$geometry.y <- NULL
# colnames(tabFlows) <- c("ORI","DES","MODE","CSP","FLOW","DIST","DISTTOT","ORILIB","DESLIB")
# saveRDS(tabFlows,"data/tabflows.Rds")

#Tableau avec les origines, destination, mode de transport et flux de naveteurs entre toute les communes 
tabFlows <- readRDS(file = "data/tabflows.Rds")
tabFlowsAgr <- toyspace::city_aggregate(before = before,after = after,tabflows = tabFlows,idori = "ORI",iddes = "DES")

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

polAgr <- toyspace::pol_aggregate(before = before, after = after, pol = pol, idpol = "insee", namepol = "nomcom", nameAgr = "paris")

#Coordonnées X Y des communes
coordCom <- toyspace::coord_com(pol = pol)
coordCom$LIBGEO <- paste(coordCom$insee, coordCom$nomcom, sep = ' - ')


############ LOAD FUNCTION ############

commData <- toyspace::mob_indic(tabflows = tabFlows, idori = "ORI", iddes = "DES", idflow = "FLOW", pol = pol, idpol = "insee", iddist = "DIST")

# domFlowJob <- toyspace::nystuen_dacey(tabflows = tabFlowsAgr, idori = "ORIAGR", iddes = "DESAGR", idflow = "FLOW",
#                                       weight = "destination", threspct = 1, pol = polAgr, idpol = "IDAGR", iddist = "DIST")
domFlowJob <- readRDS("data/domFlowJob.Rds")
# domFlowPop <- toyspace::nystuen_dacey(tabflows = tabFlowsAgr, idori = "ORIAGR", iddes = "DESAGR", idflow = "FLOW",
# weight = "origin", threspct = 1, pol = polAgr, idpol = "IDAGR", iddist = "DIST")
domFlowPop <- readRDS("data/domFlowPop.Rds")
# domFlowJP <- toyspace::nystuen_dacey(tabflows = tabFlowsAgr, idori = "ORIAGR", iddes = "DESAGR", idflow = "FLOW",
# weight = "sum", threspct = 1, pol = polAgr, idpol = "IDAGR", iddist = "DIST")
domFlowJP <- readRDS("data/domFlowJP.Rds")
# icdr <- toyspace::label_icdr(tabflows = tabFlowsAgrNoMode, idori = "ORIAGR",iddes = "DESAGR", idflow = "FLOW", pol = polAgr, idpol = "IDAGR")
# icdr <- readRDS(file = "data/icdr.Rds")
# 
# icdrI <- icdr[[2]] %>% dplyr::filter(ICDR == "Integrated")
# icdrC <- icdr[[2]] %>% dplyr::filter(ICDR == "Convergent")
# icdrD <- icdr[[2]] %>% dplyr::filter(ICDR == "Divergent")
# hotspot <- icdr[[1]]

##############################
#####      Global        #####
##############################

Filter_flux <- function(tabflows, variable, label){
  if(is.null(variable) == TRUE | is.null(label) == TRUE){
    tabflows <- tabflows
  }else{
    tabflows <- tabflows[tabflows[[variable]]==label,]
  }
  shinyjs::hideElement(id = 'loading')
  return(tabflows)
}

# get index----
Index <- function(mobi,commdata){
  if(mobi=="emploi"){
    data <- commdata$TOTDES
    nom <- ""
    unit <- "emplois"
    color <- "PuOr"
    breaks <- replace(sort(append(0,getBreaks(commdata$RelBal,nclass = 6,method = "fisher-jenks"))), 6, max(commdata$RelBal)+0.1)
    layer <- "taux"
    polygons <- ""}
  if(mobi=="popact"){
    data <- commdata$TOTORI
    nom <- ""
    unit <- "actifs"
    color <- "PuOr"
    breaks <- replace(sort(append(0,getBreaks(commdata$RelBal,nclass = 6,method = "fisher-jenks"))), 6, max(commdata$RelBal)+0.1)
    layer <- "taux"
    polygons <- ""}
  if(mobi=="soldeRel"){
    data <- commdata$RelBal
    nom <- "Solde relatif : "
    unit <- ""
    color <- "PuOr"
    breaks <- sort(round(replace(append(0,getBreaks(commdata$RelBal,nclass = 6,method = "fisher-jenks")), 8, max(commdata$RelBal)+0.1), digit = 2))
    layer <- "stock"
    polygons <- "communes"}
  if(mobi=="Contention"){
    data <- commdata$Contention
    nom <- "Auto-Contention : "
    unit <- "%"
    color <- "Purples"
    breaks <- sort(replace(round(getBreaks(commdata$Contention,nclass = 6,method = "fisher-jenks"), digit = 2), 7, max(commdata$Contention)+0.1))
    layer <- "stock"
    polygons <- "communes"}
  if(mobi=="Suffisance"){
    data <- commdata$AutoSuff
    nom <- "Auto-Suffisance : "
    unit <- "%"
    color <- "Purples"
    breaks <- sort(replace(round(getBreaks(commdata$AutoSuff,nclass = 6,method = "fisher-jenks"), digit = 2), 7, max(commdata$AutoSuff)+0.1))
    layer <- "stock"
    polygons <- "communes"}
  if(mobi=="meanDistOri"){
    data <- commdata$MEANDISTORI
    nom <- "Distance moyenne à l'origine : "
    unit <- "km"
    color <- "Purples"
    breaks <- sort(replace(round(getBreaks(commdata$MEANDISTORI,nclass = 6,method = "fisher-jenks"), digit = 2), 7, max(commdata$MEANDISTORI)+0.1))
    layer <- "stock"
    polygons <- "communes"}
  if(mobi=="meanDistDes"){
    data <- commdata$MEANDISTDES
    nom <- "Distance moyenne à destination : "
    unit <- "km"
    color <- "Purples"
    breaks <- sort(replace(round(getBreaks(commdata$MEANDISTDES,nclass = 6,method = "fisher-jenks"), digit = 2), 7, max(commdata$MEANDISTDES)+0.1))
    layer <- "stock"
    polygons <- "communes"}
  if(mobi=="perOri"){
    data <- commdata$perOri
    nom <- "Part des flux à l'origine : "
    unit <- "%"
    color <- "Purples"
    breaks <- sort(replace(round(getBreaks(commdata$perOri,nclass = 6,method = "fisher-jenks"), digit = 2), 7, max(commdata$perOri)+0.1))
    layer <- "stock"
    polygons <- "communes"}
  if(mobi=="perDes"){
    data <- commdata$perDes
    nom <- "Part des flux à la destination : "
    unit <- "%"
    color <- "Purples"
    breaks <- sort(replace(round(getBreaks(commdata$perDes,nclass = 6,method = "fisher-jenks"), digit = 2), 7, max(commdata$perDes)+0.1))
    layer <- "stock"
    polygons <- "communes"}
  return(list(data = data,nom = nom,unit = unit,color = color,breaks = breaks,layer = layer,polygons = polygons, commdata = commdata))
}

Filter_indice <- function(tabflows, idori, iddes, idflow, iddist, pol, idpol,variable, label){
  shinyjs::showElement(id = 'loading')
  if(is.null(variable) == TRUE | is.null(label) == TRUE){
    tabflows <- tabflows
  }else{
    tabflows <- tabflows[tabflows[[variable]]==label,]
  }
  commdata <- toyspace::mob_indic(tabflows = tabflows, idori = idori, iddes = iddes, idflow = idflow, pol = pol, idpol = idpol, iddist = iddist)
  shinyjs::hideElement(id = 'loading')
  return(commdata)
}

# get structure----
Structure <- function(Flu,domFlowJob,domFlowPop,domFlowJP){
  if(Flu=="iEmploi"){
    dataflu <- domFlowJob[[2]]
    rayon <- (sqrt(domFlowJob[[1]][["TOTDES"]])/pi)*20
    cercle <- domFlowJob[[1]]
    valCercle <- domFlowJob[[1]][["TOTDES"]]
    nom <- "Emploi : "
    comm <- toupper(domFlowJob[[1]][["nomcom"]])}
  if(Flu=="iPopulation"){
    dataflu <- domFlowPop[[2]]
    rayon <- (sqrt(domFlowPop[[1]][["TOTORI"]])/pi)*20
    cercle <- domFlowPop[[1]]
    valCercle <- domFlowPop[[1]][["TOTORI"]]
    nom <- "Population : "
    comm <- toupper(domFlowPop[[1]][["nomcom"]])}
  if(Flu=="iEmpPop"){
    dataflu <- domFlowJP[[2]]
    rayon <- (sqrt(domFlowJP[[1]][["TOTORIDES"]])/pi)*20
    cercle <- domFlowJP[[1]]
    valCercle <- domFlowJP[[1]][["TOTORIDES"]]
    nom <- "Flux intra-communaux : "
    comm <- toupper(domFlowJP[[1]][["nomcom"]])}
  return(list(dataflu = dataflu, rayon = rayon, cercle = cercle, valCercle = valCercle, nom = nom, comm = comm, dataflu = dataflu))
}

Filter_structure <- function(tabflows, idflow, before, after, pol, idpol, namepol, nameAgr, variable, label){
  shinyjs::showElement(id = 'loading')
  if(is.null(variable) == TRUE | is.null(label) == TRUE){
    tabflows <- tabflows
  }else{
    tabflows <- tabflows[tabflows[[variable]]==label,]
  }
  tabFlowsAgr <- toyspace::city_aggregate(before = before,after = after,tabflows = tabflows,idori = "ORI",iddes = "DES")
  polAgr <- toyspace::pol_aggregate(before = before, after = after, pol = pol, idpol = idpol, namepol = namepol, nameAgr = nameAgr)
  domFlowJob <- toyspace::nystuen_dacey(tabflows = tabFlowsAgr, idori = "ORIAGR", iddes = "DESAGR", idflow = idflow,
                                        weight = "destination", threspct = 1, pol = polAgr, idpol = "IDAGR", iddist = "DIST")
  domFlowPop <- toyspace::nystuen_dacey(tabflows = tabFlowsAgr, idori = "ORIAGR", iddes = "DESAGR", idflow = idflow,
                                        weight = "origin", threspct = 1, pol = polAgr, idpol = "IDAGR", iddist = "DIST")
  domFlowJP <- toyspace::nystuen_dacey(tabflows = tabFlowsAgr, idori = "ORIAGR", iddes = "DESAGR", idflow = idflow,
                                       weight = "sum", threspct = 1, pol = polAgr, idpol = "IDAGR", iddist = "DIST")
  shinyjs::hideElement(id = 'loading')
  return(list(domFlowJob = domFlowJob, domFlowPop = domFlowPop, domFlowJP = domFlowJP))
}

# get top links ----

GetLinks <- function(tabnav, ref, varsort, oneunit, thres){
  refLib <- paste0(ref, "LIB")
  oriDes <- paste0(c("ORI", "DES"), "LIB")
  invRef <- oriDes[oriDes != refLib]
  tabSel <- tabnav %>% 
    group_by(ORI, DES) %>% 
    summarise(FLOW = sum(FLOW), DIST = first(DIST), DISTTOT = sum(DISTTOT), ORILIB = first(ORILIB), DESLIB = first(DESLIB)) %>% 
    as.data.frame(stringsAsFactors = FALSE)
  tabSel <- tabSel[tabSel[[refLib]] == oneunit, ]
  tabSel <- tabSel[order(tabSel[[varsort]], decreasing = TRUE), ]
  nbRows <- ifelse(thres > nrow(tabSel), nrow(tabSel), thres)
  if (dim(tabSel)[1] == 0) {
    #get a "false" link with the same departure and arrival so it seems unexisting
    spLinks <- getLinkLayer(x = pol, df = tabFlows[1:0, c("ORI", "DES")])
  }else{
    spLinks <- getLinkLayer(x = pol, df = tabSel[1:nbRows, c("ORI", "DES")])
  }
  topDes <- spLinks
  return(list(topDes = topDes,layer = layer))
}

# get city value ----
city_Value <- function(tabflows,matDist,pol,idpol,var, od, city){
  
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
  if(sum(tabCityFlow$DATA) == 0 |sum(tabCityFlow$DATA) == 0){
    breaks <- c(0,1)
  }else{
    breaks <- round(replace(getBreaks(spcom$DATA, nclass = 6,method = "fisher-jenks"),7,max(spcom$DATA)+1))
  }
  return(list( SPCOM = spcom, INDEX = index, BREAKS = breaks))
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

