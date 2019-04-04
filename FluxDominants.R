library(rgeos)

# Import data
tabflowdom <- readRDS("data/tabflows.Rds")


dicoAgrParis <- tibble(OLDCODE = c("75101", "75102", "75103","75104", "75105", 
                                "75106","75107", "75108", "75109","75110", 
                                "75111", "75112", "75113", "75114","75115", 
                                "75116", "75117","75118", "75119", "75120"), NEWCODE = "75056")

tabflowdom$ORIAGR <- plyr::mapvalues(x = tabflowdom$ORI, from = dicoAgrParis$OLDCODE, to = dicoAgrParis$NEWCODE)
tabflowdom$DESAGR <- plyr::mapvalues(x = tabflowdom$DES, from = dicoAgrParis$OLDCODE, to = dicoAgrParis$NEWCODE)


matflowdom <- prepflows(mat = tabflowdom, i = "ORIAGR", j = "DESAGR", fij = "FLOW")


# Remove the matrix diagonal
diag(matflowdom) <- 0

# Select flows that represent at least 20% of the sum of outgoing flows for 
# each urban area.
firstflows <- firstflows(mat = matflowdom, method = "nfirst",k = 1)


# Select the dominant flows (incoming flows criterion)
domflowsEmploi <- domflows(mat = matflowdom, w = colSums(matflowdom), k = 1)
domflowsPop <- domflows(mat = matflowdom, w = rowSums(matflowdom), k = 1)
domflowsEmploiPop <- domflows(mat = matflowdom, w = colSums(matflowdom) + rowSums(matflowdom), k = 1)

# Combine selections
flowDomEmploi <- matflowdom * firstflows * domflowsEmploi
flowDomPop <- matflowdom * firstflows * domflowsPop
flowDomEmploiPop <- matflowdom * firstflows * domflowsEmploiPop

# Node weights
inflows <- data.frame(id = colnames(matflowdom), w = colSums(matflowdom))

commflu <- readOGR(dsn = "data/les-communes-generalisees-dile-de-france-parisagr.shp")


##################
#FlowDomEmploi
##################
flowDomLongEmploi <- melt(data = flowDomEmploi, varnames = c("ORI", "DES"), value.name = "FLOW", as.is = TRUE) %>%
  filter(FLOW > 0)
flowDomLongEmploi$KEY <- paste(flowDomLongEmploi$ORI, flowDomLongEmploi$DES, sep = "_")

spLinksEmploi <- getLinkLayer(x = commflu, xid = "insee", df = flowDomLongEmploi[, c("ORI", "DES")], dfid = c("ORI", "DES"))
spLinksEmploi$KEY <- paste(spLinksEmploi$ORI, spLinksEmploi$DES, sep = "_")
spLinksEmploi <- left_join(spLinksEmploi, flowDomLongEmploi[, c("KEY", "FLOW")], by = "KEY")

##################
#FlowDomPop
##################
flowDomLongPop <- melt(data = flowDomPop, varnames = c("ORI", "DES"), value.name = "FLOW", as.is = TRUE) %>%
  filter(FLOW > 0)
flowDomLongPop$KEY <- paste(flowDomLongPop$ORI, flowDomLongPop$DES, sep = "_")

spLinksPop <- getLinkLayer(x = commflu, xid = "insee", df = flowDomLongPop[, c("ORI", "DES")], dfid = c("ORI", "DES"))
spLinksPop$KEY <- paste(spLinksPop$ORI, spLinksPop$DES, sep = "_")
spLinksPop <- left_join(spLinksPop, flowDomLongPop[, c("KEY", "FLOW")], by = "KEY")

##################
#FlowDomEmploiPop
##################
flowDomLongEmploiPop <- melt(data = flowDomEmploiPop, varnames = c("ORI", "DES"), value.name = "FLOW", as.is = TRUE) %>%
  filter(FLOW > 0)
flowDomLongEmploiPop$KEY <- paste(flowDomLongEmploiPop$ORI, flowDomLongEmploiPop$DES, sep = "_")

spLinksEmploiPop <- getLinkLayer(x = commflu, xid = "insee", df = flowDomLongEmploiPop[, c("ORI", "DES")], dfid = c("ORI", "DES"))
spLinksEmploiPop$KEY <- paste(spLinksEmploiPop$ORI, spLinksEmploiPop$DES, sep = "_")
spLinksEmploiPop <- left_join(spLinksEmploiPop, flowDomLongEmploiPop[, c("KEY", "FLOW")], by = "KEY")


###### Poid des lignes
spLinksEmploi$weight<-  ifelse(spLinksEmploi$FLOW<1741.04,1.3,
                               ifelse(spLinksEmploi$FLOW>=1741.04 & spLinksEmploi$FLOW<6632.37,4,8
                                      ))

spLinksPop$weight<-  ifelse(spLinksPop$FLOW<1741.04,1.3,
                               ifelse(spLinksPop$FLOW>=1741.04 & spLinksPop$FLOW<6632.37,4,8
                                      ))

spLinksEmploiPop$weight<-  ifelse(spLinksEmploiPop$FLOW<1741.04,1.3,
                               ifelse(spLinksEmploiPop$FLOW>=1741.04 & spLinksEmploiPop$FLOW<6632.37,4,8
                                      ))


#Convertir commflu en objet SF et extraire les centroides au format X, Y
commflusf <- st_as_sf(commflu)
commflusfCent <- st_centroid(commflusf)
xy <- do.call(rbind, st_geometry(commflusfCent)) %>% setNames(c("lon","lat"))


# Transformed data
proj4string <-"+proj=lcc +lat_1=49 +lat_2=44 +lat_0=46.5 +lon_0=3 +x_0=700000 +y_0=6600000 +ellps=GRS80 +units=m +no_defs"
commflusfCent$lon <- project(xy=xy, proj4string, inv = TRUE)[,1]
commflusfCent$lat <- project(xy, proj4string, inv = TRUE)[,2]
latlon <- data.frame(lat=pj$y, lon=pj$x)
print(latlon)

###### total d'entrée et de sortie mergé avec le tableau

totOri <- tabflowdom %>% group_by(ORIAGR) %>% summarise(POPULATION = sum(FLOW))
totOri <- transform(totOri, ORIAGR = as.numeric(ORIAGR))
totOri <- left_join(totOri, commflusfCent, by = c("ORIAGR"="insee"))

totDes <- tabflowdom %>% group_by(DESAGR) %>% summarise(EMPLOI = sum(FLOW))
totDes <- transform(totDes, DESAGR = as.numeric(DESAGR))
totDes <- left_join(totDes, commflusfCent, by = c("DESAGR"="insee"))

totOriDes <- totOri
totOriDes$POPetEMPLOI <- totOri$POPULATION + totDes$EMPLOI


##Création de données pour les rayons

bbbox <- sp::bbox(commflu)
x1 <- bbbox[1]
y1 <- bbbox[2]
x2 <- bbbox[3]
y2 <- bbbox[4]
sfdc <- (x2 - x1) * (y2 - y1)

rayonEmploi <- sqrt(totDes$EMPLOI)*7
rayonPopulation <- sqrt(totOri$POPULATION)*7
rayonEmploiPop <- sqrt(totOriDes$POPetEMPLOI)*7

##Création de donées pour la taille des traits et la couleur des cercles
fdom1 <- reshape2::melt(flowDomEmploi)
names(fdom1) <- c("i", "j", "fij")
fdom1 <- fdom1[fdom1$fij > 0, ]
fdom1 <- merge(fdom1, totDes, by.x = "i", by.y = "DESAGR", all.x = T, 
              suffixes = c("i", "j"))
fdom1 <- merge(fdom1, totDes, by.x = "j", by.y = "DESAGR", all.x = T, 
              suffixes = c("i", "j"))
fdom1$width <- (fdom1$fij * 8/(max(fdom1$fij) - min(fdom1$fij))) + 
  2

###Création des couleurs pour totDes
totDes$col <- ""
totDes[totDes$DESAGR %in% fdom1$j & !totDes$DESAGR %in% fdom1$i, "col"] <- "brown"
totDes[totDes$DESAGR %in% fdom1$j & totDes$DESAGR %in% fdom1$i, "col"] <- "mediumorchid"
totDes[!totDes$DESAGR %in% fdom1$j & totDes$DESAGR %in% fdom1$i, "col"] <- "cornflowerblue"
totDes <- totDes[totDes$col != "", ]

###Création des couleurs pour totOri
totOri$col <- ""
totOri[totOri$ORIAGR %in% fdom1$j & !totOri$ORIAGR %in% fdom1$i, "col"] <- "brown"
totOri[totOri$ORIAGR %in% fdom1$j & totOri$ORIAGR %in% fdom1$i, "col"] <- "mediumorchid"
totOri[!totOri$ORIAGR %in% fdom1$j & totOri$ORIAGR %in% fdom1$i, "col"] <- "cornflowerblue"
totOri <- totOri[totOri$col != "", ]

###Création des couleurs pour totOriDes
totOriDes$col <- ""
totOriDes[totOriDes$ORIAGR %in% fdom1$j & !totOriDes$ORIAGR %in% fdom1$i, "col"] <- "brown"
totOriDes[totOriDes$ORIAGR %in% fdom1$j & totOriDes$ORIAGR %in% fdom1$i, "col"] <- "mediumorchid"
totOriDes[!totOriDes$ORIAGR %in% fdom1$j & totOriDes$ORIAGR %in% fdom1$i, "col"] <- "cornflowerblue"
totOriDes <- totOriDes[totOriDes$col != "", ]



