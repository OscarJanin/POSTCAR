library(flows)
library(cartography)
library(sf)
library(dplyr)



shape <- readOGR(dsn = "data/les-communes-generalisees-dile-de-france-parisagr2.shp")

id <- "insee"

############################################################################################################################
###############################################   Préparation de données ###################################################
############################################################################################################################

dicoAgrParis <- tibble(OLDCODE = c("75101", "75102", "75103","75104", "75105", 
                                   "75106","75107", "75108", "75109","75110", 
                                   "75111", "75112", "75113", "75114","75115", 
                                   "75116", "75117","75118", "75119", "75120"), NEWCODE = "75056")

tabflowdom$ORIAGR <- plyr::mapvalues(x = tabflowdom$ORI, from = dicoAgrParis$OLDCODE, to = dicoAgrParis$NEWCODE)
tabflowdom$DESAGR <- plyr::mapvalues(x = tabflowdom$DES, from = dicoAgrParis$OLDCODE, to = dicoAgrParis$NEWCODE)


mat <- prepflows(mat = tabflowdom, i = "ORIAGR", j = "DESAGR", fij = "FLOW")

############################################################################################################################
############################################################################################################################
############################################################################################################################


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
  # latlon <- data.frame(lat=pj$y, lon=pj$x)
  # print(latlon)
  
  ###### total d'entrée et de sortie mergé avec le tableau
  
  longMatrix <- melt(data = mat)
  colnames(longMatrix) <- c("ORI", "DES","FLOW")
  
  OriFlow <- longMatrix %>% group_by(ORI) %>% summarise(POPULATION = sum(FLOW))
  OriFlow <- transform(OriFlow, ORI = as.numeric(ORI))
  # OriFlow <- left_join(OriFlow, shapesfCent, by = c("ORI"= id))
  
  DesFlow <- longMatrix %>% group_by(DES) %>% summarise(JOB = sum(FLOW))
  DesFlow <- transform(DesFlow, DES = as.numeric(DES))
  # DesFlow <- left_join(DesFlow, shapesfCent, by = c("DES"= id))
  
  # OriDesFlow <- OriFlow
  # OriDesFlow$POPJOB <- OriFlow$POPULATION + DesFlow$JOB
  
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

domFloow <- domFlow(mat = mat, shape = shape ,id = id, weight = "job")

domFloow[1]
domFloow[2]

