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
listTimes <- readRDS(file = "C:/Users/Bureau des MCFs/Desktop/Postcar/Dev/Tuto/PremiereAppliShiny/data/listtimes.Rds")
listPotentials <- readRDS(file = "C:/Users/Bureau des MCFs/Desktop/Postcar/Dev/Tuto/PremiereAppliShiny/data/listpotentials.Rds")
pomaTable <- readRDS(file = "C:/Users/Bureau des MCFs/Desktop/Postcar/Dev/Tuto/PremiereAppliShiny/data/pomatable.Rds")
tabFlows <- readRDS(file = "C:/Users/Bureau des MCFs/Desktop/Postcar/Dev/Tuto/PremiereAppliShiny/data/tabflows.Rds")
listCondor <- readRDS(file = "C:/Users/Bureau des MCFs/Desktop/Postcar/Dev/Tuto/PremiereAppliShiny/data/condorcet.Rds")


#extraction des df de temps
timesTC <- listtimes[[1]]
timesVPM <- listtimes[[2]]
timesVPS <- listtimes[[3]]
timesDIST <- listtimes[[4]]

timesTC$TTC <- timesTC$VAL
timesTC$TVPM <- timesVPM$VAL
timesTC$TVPS <- timesVPS$VAL
timesTC$TDIST <- timesDIST$VAL
timesTC$ORIDES <- paste(timesTC$ORI,timesTC$DES, sep ="")


#transformation de tabflow en format long
tabflowscast <- dcast(tabflows, ORI + DES ~ MODE, value.var = "FLOW")

#remplacement des valeurs NA par 0
tabflowscast[is.na(tabflowscast)] <- 0

#Création valriable jointure pour df temps
tabflowscast$ORIDES <- paste(tabflowscast$ORI,tabflowscast$DES, sep ="")

#jointure des deux df
tabflowscast <- merge(tabflowscast, timesTC, by.x = "ORIDES", by.y = "ORIDES")

#Supression des colonnes inutiles
tabflowscast$ORIDES <- NULL
tabflowscast$ORI.y <- NULL
tabflowscast$DES.y <- NULL
tabflowscast$VAL <- NULL

tabflowscast$ORI <- tabflowscast$ORI.x
tabflowscast$DES <- tabflowscast$DES.x

tabflowscast$ORI.x <- NULL
tabflowscast$DES.x <- NULL


############# ############# ############# ############# ############# 
#############   DF TEMPS DE TRANSPORT (ORIORI ORI DES)  #############
############# ############# ############# ############# ############# 

#créer un df de temps avec le temps median d'acces a chaque commune ayant un flux y compris d'ori a ori
#dégager les valeurs d'origine à origine et les stocker dans un tableau 
timesOriOri <- tabflowscast %>% filter_( "ORI == DES")
timesOriOri$TTCOriOri <- timesOriOri$TTC
timesOriOri$TVPMOriOri <- timesOriOri$TVPM
timesOriOri$TVPSOriOri <- timesOriOri$TVPS
timesOriOri$TTC <- NULL
timesOriOri$TVPM <- NULL
timesOriOri$TVPS <- NULL
timesOriOri$DIST <- NULL

#suppression des flux intra, puis regrouppement des valeurs aux mêmes origines en les aditionnant
timesOri <-  tabflowscast %>% filter_( "ORI != DES") %>% group_by(ORI) %>% summarise(TTCOri = median(TTC),
                                                                                TVPMOri = median(TVPM),
                                                                                TVPSOri = median(TVPS),
                                                                                DISTOri = median(TDIST))

#suppression des flux intra, puis regrouppement des valeurs aux mêmes destinations en les aditionnant
timesDes <-  tabflowscast %>% filter_( "ORI != DES") %>% group_by(DES) %>% summarise(TTCDes = median(TTC),
                                                                                TVPMDes = median(TVPM),
                                                                                TVPSDes = median(TVPS),
                                                                                DISTDes = median(TDIST))


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
tabflow4 <- merge(tabflow3, timesOri, by.x = "ORI", by.y = "ORI")
tabflow5 <- merge(tabflow4, timesOriOri, by.x = "ORI", by.y = "ORI")
tabflow6 <- merge(tabflow5, timesDes, by.x = "ORI", by.y = "DES")

tabflow6$TTC <- NULL
tabflow6$TVPM <- NULL
tabflow6$TVPS <- NULL
tabflow6$DES.x <- NULL
tabflow6$TDIST.y <- NULL
tabflow6$DES.y <- NULL

########################################################
######         Créations des indicateurs         #######
########################################################

# pct utilisation des transports en communs
tabflow6$PctTC <- ((tabflow6$TcOri + tabflow6$TcOriOri)*100)/(tabflow6$TcOri + tabflow6$TcOriOri + tabflow6$VpOri + tabflow6$VpOriOri + tabflow6$NmOri + tabflow6$NmOriOri)

#AutoContention 
tabflow6$AutoContention <- tabflow6$OriOriTotal / tabflow6$OriTotal

#AutoSuffisance
tabflow6$AutoSuffisance <- tabflow6$OriOriTotal / (tabflow6$DesTotal + tabflow6$OriOriTotal)

#Dépendance
tabflow6$Dependance <- tabflow6$OriOriTotal / (tabflow6$OriTotal + tabflow6$OriOriTotal)

#Mobilité
tabflow6$Mobilite <- (tabflow6$DesTotal+tabflow6$OriTotal) / (tabflow6$OriTotal + tabflow6$OriOriTotal)

#Gravitation / Solde relatif
tabflow6$Gravitation <- (tabflow6$DesTotal-tabflow6$OriTotal) / (tabflow6$OriTotal + tabflow6$OriOriTotal)

#Pourcentage Flux Entrant
tabflow6$PctFluxEntrant <- (tabflow6$DesTotal*100) / (tabflow6$OriTotal + tabflow6$OriOriTotal)

#Pourcentage Flux Sortant
tabflow6$PctFluxSortant <- (tabflow6$OriTotal*100) / (tabflow6$OriTotal + tabflow6$OriOriTotal)

#Différence de temps de trajet voiture matin et transport en commun pour les communes d'origine
tabflow6$TvpmMTtcOri <- tabflow6$TVPMOri - tabflow6$TTCOri

#Différence de temps de trajet voiture soir et transport en commun pour les communes d'origine
tabflow6$TvpsMTtcOri <- tabflow6$TVPSOri - tabflow6$TTCOri

#Différence de temps de trajet voiture matin et transport en commun pour les communes de destination
tabflow6$TvpmMTtcDes <- tabflow6$TVPMDes - tabflow6$TTCDes

#Différence de temps de trajet voiture soir et transport en commun pour les communes de destination
tabflow6$TvpsMTtcDes <- tabflow6$TVPSDes - tabflow6$TTCDes



saveRDS(tabflow6, file = "C:/Users/Bureau des MCFs/Desktop/Postcar/Dev/Tuto/PremiereAppliShiny/data/data_PCR.rds")

########################################################
######     Créations des des flux dominants      #######
########################################################

myflows <- prepflows(mat = nav, i = "i", j = "j", fij = "fij")
diag(myflows) <- 0
flowSel1 <- domflows(mat = myflows, w = colSums(myflows), k = 1)
flowSel2 <- firstflows(mat = myflows, method = "nfirst", ties.method = "first", k = 1)

myflowDom <- myflows * flowSel1 * flowSel2
myflowDomLong <- melt(data = myflowDom, varnames = c("ORI", "DES"), value.name = "FLOW", as.is = TRUE) %>%
  filter(FLOW > 0)
myflowDomLong$KEY <- paste(myflowDomLong$ORI, myflowDomLong$DES, sep = "_")
myspLinks <- getLinkLayer(x = UA, df = myflowDomLong[, c("ORI", "DES")])
class(myspLinks)
myspLinks$KEY <- paste(myspLinks$ORI, myspLinks$DES, sep = "_")
myspLinks <- left_join(myspLinks, myflowDomLong[, c("KEY", "FLOW")], by = "KEY")

# firstFlow <- tabFlows %>%
#   filter(ORI != DES) %>%
#   group_by(ORI) %>%
#   top_n(n = 1, wt = FLOW)

UAsf <- st_as_sf(UA)

leaflet() %>%
  addTiles() %>%
  addPolygons(data = st_transform(UAsf, crs = 4326), stroke = TRUE, weight = 1, color = "grey35", fill = TRUE,
              fillColor = "ghostwhite", fillOpacity = 0.3) %>%
  addPolylines(data = st_transform(myspLinks, crs = 4326), color = "firebrick", opacity = 0.8, weight = 1.5,
               stroke = TRUE)

tabflows <- readRDS("C:/Users/Bureau des MCFs/Desktop/Postcar/Dev/aperitif-master/data/tabflows.Rds")

# tabflows$KEY <- paste(tabflows$ORI, tabflows$DES, sep="_")




tabflows <- tabflows %>% group_by(ORI, DES) %>% 
  summarise(FLOW = sum(FLOW)) %>% filter_( "ORI != DES") 

tabflows$key <- paste(tabflows$ORI,tabflows$DES, sep="_")

tabflows <- tabflows %>% group_by(ORI, DES) %>% top_n( n = 1, wt = FLOW)
  








ui<- fluidPage(
  #element d'affichage de la page
  
  # sliderInput(inputId = "num",
  #             label = "Choose a number",
  #             value = 5, min = 1, max = 10,
  #             ticks = TRUE),
  # plotOutput("hist")
  
)

server <- function(input, output) {
  
  # output$hist <- renderPlot({
  #     title <- "100 random normal values"
  #     hist(rnorm(input$num), main = title)
  # })
  
}

shinyApp(ui = ui, server = server)


########################################################
######                  Cartes                   #######
########################################################

      ###########################################
      ######        Cartes  Leaflet       #######
      ###########################################

commData <- merge(comm,tabflow6, by.x="insee", by.y = "ORI")

m <- leaflet(commData) %>%
  addProviderTiles("MapBox", options = providerTileOptions(
    id = "mapbox.light",
    accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')))
m %>% addPolygons()

bins <- c(0,3, 6, 9, 40)
pal <- colorBin("YlOrRd", domain = commData$Mobilite,  pretty = TRUE)

labels <- sprintf(
  "<strong>%s</strong><br/> Mobilité : %g",
  commData$nomcom, commData$Mobilite
) %>% lapply(htmltools::HTML)


m %>% addPolygons(
  fillColor = ~pal(Mobilite),
  weight = 2,
  opacity = 1,
  color = "white",
  dashArray = "3",
  fillOpacity = 0.7,
  highlight = highlightOptions(
    weight = 5,
    color = "#666",
    dashArray = "",
    fillOpacity = 0.7,
    bringToFront = TRUE),
  label = labels,
  labelOptions = labelOptions(
    style = list("font-weight" = "normal", padding = "3px 8px"),
    textsize = "15px",
    direction = "auto")) %>% addLegend(pal = pal, values = ~Mobilite, opacity = 0.7, title = NULL,
                position = "bottomright")

#carte pct trsprt en comm1
choroLayer(
  spdf = comm,
  df = tabflow6,
  spdfid = "insee",
  dfid = "ORI",
  var = "TvpmMTtcOri",
  nclass = 6,
  col = brewer.pal(n = 6, "Reds"),
  border = "white",
  method = "fisher-jenks",
  legend.values.rnd = 2
)

