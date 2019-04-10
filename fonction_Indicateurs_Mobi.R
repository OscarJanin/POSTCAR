library(reshape2)
library(dplyr)


#Chargement des données
tabFlows <- readRDS(file = "data/tabflows.Rds")
comm <- read_sf(dsn = "data/les-communes-generalisees-dile-de-france.shp")


#############################################################################################################################
####################################  Transformation des données en Matrice O/D #############################################
#############################################################################################################################
#tabflow en format long                                                                                                     ####                   
tabFlowscast <- dcast(tabFlows, ORI + DES ~ MODE, value.var = "FLOW")                                                       ####                                                          
tabFlowscast[is.na(tabFlowscast)] <- 0                                                                                      ####                            
                                                                                                                          
tabFlowscast$FLOW <- tabFlowscast$DOMICILE + tabFlowscast$NM + tabFlowscast$TC + tabFlowscast$VP                    
                                                                                                                          
tabFlowscast$DOMICILE <- NULL                                                                                               ####                  
tabFlowscast$NM <- NULL                                                                                                     ####            
tabFlowscast$TC <- NULL                                                                                                     ####            
tabFlowscast$VP <- NULL                                                                                                     ####            
                                                                                                                          
#Remplir avec des valeurs nulle les couples non représentés pour avoir une matrice²                                         
tabFlowscast$ORIFAC <- factor(tabFlowscast$ORI,                                                                             ####                                      
                           levels = sort(unique(comm$insee)),                                                               ####                                                  
                           labels = sort(unique(comm$insee)))                                                               ####                                                  
                                                                                                                          
                                                                                                                          
tabFlowscast$DESFAC <- factor(tabFlowscast$DES,                                                                             ####                                      
                           levels = sort(unique(comm$insee)),                                                               ####                                                  
                           labels = sort(unique(comm$insee)))                                                               ####                                                  
                                                                                                                          
infoFlowsWide <- dcast(tabFlowscast, formula = ORIFAC ~ DESFAC, value.var = "FLOW", fill = 0, drop = FALSE)                 ####   
                                                                                                                          
matFlows <- as.matrix(infoFlowsWide[, -1])                                                                                  ####                                
row.names(matFlows) <- infoFlowsWide$ORI                                                                                    ####                              
matFlows[is.na(matFlows)] <- 0                                                                                              ####                    
                                                                                                                          
#############################################################################################################################
#############################################################################################################################
#############################################################################################################################

mobIndic <- function (matrix, shapesf, id){
  
  #Matrix trandformation into long format
  longMatrix <- melt(data = matrix)
  colnames(longMatrix) <- c("ORI", "DES","FLOW")
  
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
  #AutoContention 
  tabflow$Dependency <- tabflow$OriOriFlow / (tabflow$OriFlow + tabflow$OriOriFlow)
  
  #Auto sufficiency
  tabflow$AutoSuff <- tabflow$OriOriFlow / (tabflow$DesFlow + tabflow$OriOriFlow)
  
  #Mobility
  tabflow$Mobility <- (tabflow$DesFlow+tabflow$OriFlow) / (tabflow$OriFlow + tabflow$OriOriFlow)
  
  #Relative Balance
  tabflow$RelBal <- (tabflow$DesFlow-tabflow$OriFlow) / (tabflow$OriFlow + tabflow$DesFlow)
  
  
  #Join with pol
  shapesf$idshp <- shapesf[[id]]
  shapeflow <- merge(shapesf,tabflow, by.x="idshp", by.y = "idflow")
                
  
  return(shapeflow)

}

commData <- mobIndic(matrix = matFlows, id = "insee", shapesf = comm)

