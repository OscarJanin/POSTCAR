library(reshape2)         # transformation format long, format large
library(sp)               # objets spatiaux
library(rgdal)            # fonctions de la bibliothèque GDAL
library(ggplot2)          # fonctions graphiques
library(ggthemes)         # thèmes pour ggplot
library(grid)             # fonction arrow
library(cartography)      # cartographie thématique
library(RColorBrewer)     # palettes de couleurs de C. Brewer
library(dplyr)            # manipulation de tableaux
library(shiny)
library(sf)
library(classInt)
library(leaflet)
library(flows)
library(proj4)



#Chargement des données
tabflows <- readRDS("C:/Users/Bureau des MCFs/Desktop/Postcar/Dev/aperitif-master/data/tabflows.Rds")
comm <- read_sf(dsn = "C:/Users/Bureau des MCFs/Desktop/Postcar/Dev/Tuto/PremiereAppliShiny/data/les-communes-generalisees-dile-de-france.shp")


pomaTable <- readRDS("C:/Users/Bureau des MCFs/Desktop/Postcar/Dev/aperitif-master/data/pomatable.Rds")
coordcom <- readRDS("C:/Users/Bureau des MCFs/Desktop/Postcar/Dev/aperitif-master/data/coordcom.Rds")
listtimes <- readRDS("C:/Users/Bureau des MCFs/Desktop/Postcar/Dev/aperitif-master/data/listtimes.Rds")
pomaCom <- readRDS(file = "C:/Users/Bureau des MCFs/Desktop/Postcar/Dev/Tuto/PremiereAppliShiny/data/pomacom.Rds")
listPotentials <- readRDS(file = "C:/Users/Bureau des MCFs/Desktop/Postcar/Dev/Tuto/PremiereAppliShiny/data/listpotentials.Rds")
pomaTable <- readRDS(file = "C:/Users/Bureau des MCFs/Desktop/Postcar/Dev/Tuto/PremiereAppliShiny/data/pomatable.Rds")
tabFlows <- readRDS(file = "C:/Users/Bureau des MCFs/Desktop/Postcar/Dev/Tuto/PremiereAppliShiny/data/tabflows.Rds")
listCondor <- readRDS(file = "C:/Users/Bureau des MCFs/Desktop/Postcar/Dev/Tuto/PremiereAppliShiny/data/condorcet.Rds")




#transformation de tabflow en format long
tabflowscast <- dcast(tabflows, ORI + DES ~ MODE, value.var = "FLOW")

#remplacement des valeurs NA par 0
tabflowscast[is.na(tabflowscast)] <- 0


############# ############# ############# ############# ############# 
#############     DF    FLUX    (ORIORI ORI DES)     ################
############# ############# ############# ############# ############# 

#dégager les valeurs d'origine à origine et les stocker dans un tableau 
tabflowOriOri <- tabflowscast %>% filter_( "ORI == DES")
tabflowOriOri$DomOriOri <- tabflowOriOri$DOMICILE
tabflowOriOri$NmOriOri <- tabflowOriOri$NM
tabflowOriOri$TcOriOri <- tabflowOriOri$TC
tabflowOriOri$VpOriOri <- tabflowOriOri$VP
tabflowOriOri$DOMICILE <- NULL
tabflowOriOri$NM <- NULL
tabflowOriOri$TC <- NULL
tabflowOriOri$VP <- NULL


#créer nouvelle colonne somme des déplacement intra (OriOri)
tabflowOriOri$OriOriTotal <- tabflowOriOri$DomOriOri + tabflowOriOri$NmOriOri + tabflowOriOri$TcOriOri + tabflowOriOri$VpOriOri 


#suppression des flux intra, puis regrouppement des valeurs aux mêmes origines en les aditionnant
tabflowOri <-  tabflowscast %>% filter_( "ORI != DES") %>% group_by(ORI) %>% summarise(DomOri = sum(DOMICILE),
                                                                                       NmOri = sum(NM),
                                                                                       TcOri = sum(TC),
                                                                                       VpOri = sum(VP))


tabflowOri$OriTotal <- tabflowOri$DomOri + tabflowOri$NmOri + tabflowOri$TcOri + tabflowOri$VpOri 


#suppression des flux intra, puis regrouppement des valeurs aux mêmes destinations en les aditionnant
tabflowDes <-  tabflowscast %>% filter_( "ORI != DES") %>% group_by(DES) %>% summarise(DomDes = sum(DOMICILE),
                                                                                       NmDes = sum(NM),
                                                                                       TcDes = sum(TC),
                                                                                       VpDes = sum(VP))

tabflowDes$DesTotal <- tabflowDes$DomDes + tabflowDes$NmDes + tabflowDes$TcDes + tabflowDes$VpDes

#Merge les 3 df
tabflow2 <- merge(tabflowOri, tabflowDes, by.x = "ORI", by.y = "DES")
tabflow3 <- merge(tabflow2, tabflowOriOri, by.x = "ORI", by.y = "ORI")


########################################################
######         Créations des indicateurs         #######
########################################################

# pct utilisation des transports en communs
tabflow3$PctTC <- ((tabflow3$TcOri + tabflow3$TcOriOri)*100)/(tabflow3$TcOri + tabflow3$TcOriOri + tabflow3$VpOri + tabflow3$VpOriOri + tabflow3$NmOri + tabflow3$NmOriOri)

#AutoContention 
tabflow3$AutoContention <- tabflow3$OriOriTotal / tabflow3$OriTotal

#AutoSuffisance
tabflow3$AutoSuffisance <- tabflow3$OriOriTotal / (tabflow3$DesTotal + tabflow3$OriOriTotal)

#Dépendance
tabflow3$Dependance <- tabflow3$OriOriTotal / (tabflow3$OriTotal + tabflow3$OriOriTotal)

#Mobilité
tabflow3$Mobilite <- (tabflow3$DesTotal+tabflow3$OriTotal) / (tabflow3$OriTotal + tabflow3$OriOriTotal)

#Gravitation / Solde relatif
tabflow3$Gravitation <- (tabflow3$DesTotal-tabflow3$OriTotal) / (tabflow3$OriTotal + tabflow3$OriOriTotal)



saveRDS(tabflow6, file = "C:/Users/Bureau des MCFs/Desktop/Postcar/Dev/Tuto/PremiereAppliShiny/data/data_PCR.rds")
