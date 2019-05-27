shinyServer(function(input, output, session) {
  ### Making Data for the server
  #Creation of variables specifically
  commData <- mobIndic(tabFlows = tabFlows, id = "insee", shapeSf = shapeSf)
  
  domFlowJob <- nystuen_dacey(tabFlowsAgrNoMode, poptabAgr, idfield = "insee", targetfield  = "TOTDES", threspct = 0, shapeAgr, shapeId = "insee")
  domFlowPop <- nystuen_dacey(tabFlowsAgrNoMode, poptabAgr, idfield = "insee", targetfield = "TOTORI", threspct = 0, shapeAgr, shapeId = "insee")
  domFlowJP <- nystuen_dacey(tabFlowsAgrNoMode, poptabAgr, idfield = "insee", targetfield = "TOTINTRA", threspct = 0, shapeAgr, shapeId = "insee")
  
  
  # Graphic Display  ####
  output$plot1 <- renderPlotly({
    plot_ly(as.data.frame(commData), x = ~RelBal, y = ~AutoSuff)
  })

  
  # Get reactive value based on radiobutton (for 1st panel "Indicators")  ####
  v <- reactiveValues(data = commData$RelBal)
  n <- reactiveValues(nom = "soldeRel")
  
  observeEvent(input$radioMobi,{
    if(input$radioMobi=="soldeRel"){
      v$data <- commData$RelBal
      n$nom <- "Solde relatif : "}
    if(input$radioMobi=="Dependance"){
      v$data <- commData$Dependency
      n$nom <- "Dépendance : "}
    if(input$radioMobi=="Suffisance"){
      v$data <- commData$AutoSuff
      n$nom <- "Auto-Suffisance : "}
    if(input$radioMobi=="Mobility"){
      v$data <- commData$Mobility
      n$nom <- "Mobilité : "}
    if(input$radioMobi=="meanDist"){
      v$data <- commData$meanDist
      n$nom <- "Distance moyenne : "}
    if(input$radioMobi=="perOri"){
      v$data <- commData$perOri
      n$nom <- "Part des flux à l'origine : "}
    if(input$radioMobi=="perDes"){
      v$data <- commData$perDes
      n$nom <- "Part des flux à la destination : "}
    if(input$radioMobi=="perIntra"){
      v$data <- commData$perIntra
      n$nom <- "Part des flux intra : "}
  })

  
  # Get reactive value based on radiobutton (for 4th panel "flow")  ####
  f <- reactiveValues(dataflu = domFlowJob[[2]])
  r <- reactiveValues(rayon = (sqrt(domFlowJob[[1]][["WGT"]])/pi)*20)
  c <- reactiveValues(cercle = domFlowJob[[1]])
  vc <- reactiveValues(valCercle = domFlowJob[[1]][["WGT"]])
  nf <- reactiveValues(nom = "Emploi : ")
  nc <- reactiveValues(comm = domFlowJob[[1]][["nomcom"]])
  
  observeEvent(input$radioFlu,{
    if(input$radioFlu=="iEmploi"){
      f$dataflu <- domFlowJob[[2]]
      r$rayon <- (sqrt(domFlowJob[[1]][["WGT"]])/pi)*20
      c$cercle <- domFlowJob[[1]]
      vc$valCercle <- domFlowJob[[1]][["WGT"]]
      nf$nom <- "Emploi : "
      nc$comm <- domFlowJob[[1]][["nomcom"]]}
    if(input$radioFlu=="iPopulation"){
      f$dataflu <- domFlowPop[[2]]
      r$rayon <- (sqrt(domFlowPop[[1]][["WGT"]])/pi)*20
      c$cercle <- domFlowPop[[1]]
      vc$valCercle <- domFlowPop[[1]][["WGT"]]
      nf$nom <- "Population : "
      nc$comm <- domFlowPop[[1]][["nomcom"]]}
    if(input$radioFlu=="iEmpPop"){
      f$dataflu <- domFlowJP[[2]]
      r$rayon <- (sqrt(domFlowJP[[1]][["WGT"]])/pi)*20
      c$cercle <- domFlowJP[[1]]
      vc$valCercle <- domFlowJP[[1]][["WGT"]]
      nf$nom <- "Flux intra-communaux : "
      nc$comm <- domFlowJP[[1]][["nomcom"]]}
  })
  
  # Indicators map Display  ####
  output$mapIndic <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
      addMapPane("background_map", zIndex = 410) %>%  # Level 1
      addMapPane("réseau_routier", zIndex = 420) %>%  # Level 2
      addMapPane("voie_ferré", zIndex = 430) %>%  # Level 3
      addMapPane("station", zIndex = 440) %>%  # Level 4
      addProviderTiles(provider = "Esri.WorldGrayCanvas",
                       options = providerTileOptions(minZoom = 8, maxZoom = 12)) %>%
      addLayersControl(
        position = "bottomright",
        overlayGroups = c("Réseau routier principal", "Réseau ferré","Stations ferroviaires"),
        options = layersControlOptions(collapsed = T)
      ) %>% 
      fitBounds(lng1 = 1.44, lat1 = 48.12, lng2 = 3.55, lat2 = 49.24)%>% 
      setMaxBounds(-0.05, 46.62, 5.05, 50.73) %>% 
      hideGroup("Réseau routier principal") %>% 
      hideGroup("Réseau ferré") %>% 
      hideGroup("Stations ferroviaires")
  })
  
  observe({
    shinyjs::showElement(id = 'loading')
    leafletProxy("mapIndic", data =commData) %>%
      clearShapes() %>%
      addPolylines(data = st_transform(routier, crs = 4326), 
                   color = "grey",
                   opacity = 0.6,
                   weight = 1.3 ,
                   stroke = TRUE, 
                   group = "Réseau routier principal",
                   options = pathOptions(pane = "réseau_routier")) %>% 
      addPolylines(data = st_transform(vferre, crs = 4326), 
                   color = "grey", 
                   opacity = 1, 
                   weight = 1 ,
                   stroke = TRUE, 
                   group = "Réseau ferré",  
                   dashArray = 2,
                   options = pathOptions(pane = "voie_ferré")) %>%
      addCircleMarkers(lng = station$longitude, 
                       lat = station$latitude, 
                       radius = 2,
                       stroke = F,
                       color = "grey",
                       fillOpacity = 0.8,
                       group = "Stations ferroviaires",
                       options = pathOptions(pane = "station")) %>% 
      
      addPolygons(
        fillColor = ~colorBin(palette = "Purples",
                              bins = getBreaks(v$data, nclass = 6,method = "fisher-jenks"),
                              domain = v$data)(v$data),
        weight = 0.7, 
        opacity = 0.5,
        color = "grey",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 2,
          color = "white",
          opacity = 0.7,
          fillOpacity = 1,
          bringToFront = TRUE),
        label = sprintf(
          "<strong>%s</strong><br/> %s %.2f",
          commData$nomcom, 
          n$nom,
          v$data
        )%>% lapply(htmltools::HTML),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"),
        options = pathOptions(pane = "réseau_routier")
      )
    shinyjs::hideElement(id = 'loading')
  })
  
  observe({
    proxy <- leafletProxy("mapIndic", data =commData)
    proxy %>% clearControls()
    proxy %>% addLegend(pal = colorBin(palette = "Purples", 
                                        bins = getBreaks(v$data,nclass = 6,method = "fisher-jenks"),
                                        domain = v$data,pretty = TRUE),
                         values = ~v$data, opacity = 0.7,
                         title = NULL, position = "bottomright"
    )
  })
  
  # Flow map Display ####
  output$mapflu <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
      addMapPane("background_map", zIndex = 410) %>%    # Level 1
      addMapPane("communes", zIndex = 420) %>%          # Level 2
      addMapPane("réseau_routier", zIndex = 430) %>%    # Level 3
      addMapPane("voie_ferré", zIndex = 440) %>%        # Level 4
      addMapPane("station", zIndex = 460) %>%            # Level 6
      addMapPane("lignes", zIndex = 470) %>%           # Level 7
      addProviderTiles(provider = "Esri.WorldGrayCanvas",
                       options = providerTileOptions(minZoom = 8, maxZoom = 12)) %>%
      addLayersControl(
        position = "bottomright",
        overlayGroups = c("Réseau routier principal", "Réseau ferré", "Stations ferroviaires"),
        options = layersControlOptions(collapsed = T)
      ) %>% 
      fitBounds(lng1 = 1.44, lat1 = 48.12, lng2 = 3.55, lat2 = 49.24)%>% 
      setMaxBounds(-0.05, 46.62, 5.05, 50.73) %>% 
      hideGroup("Réseau routier principal") %>% 
      hideGroup("Réseau ferré") %>% 
      hideGroup("Stations ferroviaires")
  })
  
  observe({
    shinyjs::showElement(id = 'loading')
    
    topDes <- GetTopLinks()
    cityVal <- Get_CityValue()
    leafletProxy("mapflu", data = cityVal) %>%
      clearShapes() %>%
      addPolylines(data = st_transform(routier, crs = 4326), color = "grey", opacity = 0.6, weight = 1.3 ,
                   stroke = TRUE, group = "Réseau routier principal",
                   options = pathOptions(pane = "réseau_routier")) %>% 
      addPolylines(data = st_transform(vferre, crs = 4326), color = "grey", opacity = 0.6, weight = 1.3 ,
                   stroke = TRUE, group = "Réseau ferré",  dashArray = 2,
                   options = pathOptions(pane = "voie_ferré")) %>%
      addCircleMarkers(lng = station$longitude, 
                       lat = station$latitude, 
                       radius = 2,
                       stroke = F,
                       color = "grey",
                       fillOpacity = 0.8,
                       group = "Stations ferroviaires",
                       options = pathOptions(pane = "station")) %>%
      addPolygons(data = topDes$POLYG, stroke = TRUE, weight = 2, color = "black", highlight = highlightOptions(  weight = 3, color = "grey",
        opacity = 0.7, bringToFront = TRUE), fill = F, options = pathOptions(pane = "comm")) %>%
      addPolylines(data = topDes$LINES, color = "Purple", opacity = 0.8, weight = 1.5, stroke = TRUE, options = pathOptions(pane = "lignes")) %>% 
      addPolygons(
        fillColor = ~colorBin(palette = "Purples",
                              bins = append(0.001,getBreaks(cityVal$tabCityFlow, nclass = 6,method = "fisher-jenks")),
                              domain = cityVal$tabCityFlow)(cityVal$tabCityFlow),
        weight = 0.7, 
        opacity = 0.5,
        color = "grey",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 2,
          color = "white",
          opacity = 0.7,
          fillOpacity = 1,
          bringToFront = TRUE),
        label = sprintf(
          "<strong>%s</strong><br/> %.2f",
          cityVal$nomcom,
          cityVal$tabCityFlow
        )%>% lapply(htmltools::HTML),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"),
        options = pathOptions(pane = "réseau_routier")
      )
    shinyjs::hideElement(id = 'loading')
  })
  
  observe({
    cityVal <- Get_CityValue()
    proxy <- leafletProxy("mapflu", data =cityVal)
    proxy %>% clearControls()
    proxy %>% addLegend(pal = colorBin(palette = "Purples", 
                                       bins = append(0.001,getBreaks(cityVal$tabCityFlow, nclass = 6,method = "fisher-jenks")),
                                       domain = cityVal$tabCityFlow,pretty = TRUE),
                        values = ~cityVal$tabCityFlow, opacity = 0.7,
                        title = NULL, position = "bottomright"
    )
  })


  
  # Pool map Display ####
  output$mappot <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
      addMapPane("background_map", zIndex = 410) %>%    # Level 1
      addMapPane("communes", zIndex = 420) %>%          # Level 2
      addMapPane("réseau_routier", zIndex = 430) %>%    # Level 3
      addMapPane("voie_ferré", zIndex = 440) %>%        # Level 4
      addMapPane("station", zIndex = 450) %>% 
      addProviderTiles(provider = "Esri.WorldGrayCanvas",
                       options = providerTileOptions(minZoom = 8, maxZoom = 12)) %>%
      addLayersControl(
        position = "bottomright",
        overlayGroups = c("Communes", "Réseau routier principal", "Réseau ferré", "Stations ferroviaires"),
        options = layersControlOptions(collapsed = T)
      ) %>% 
      fitBounds(lng1 = 1.44, lat1 = 48.12, lng2 = 3.55, lat2 = 49.24) %>% 
      setMaxBounds(-0.05, 46.62, 5.05, 50.73) %>% 
      hideGroup("Réseau routier principal") %>% 
      hideGroup("Réseau ferré") %>% 
      hideGroup("Communes") %>% 
      hideGroup("Stations ferroviaires")
  })
  
  observe({
    shinyjs::showElement(id = 'loading')
    
    if(input$pottyp == "dif"){
      leafletProxy("mappot") %>%
        clearShapes() %>%
        clearImages() %>% clearShapes() %>% clearControls() %>%
        addPolygons(data = st_transform(shapeSf, crs = 4326), stroke = TRUE, weight = 0.5, opacity = 0.5, color = "grey", fill = TRUE,
                    fillColor = "grey", fillOpacity = 0, group = "Communes",
                    options = pathOptions(pane = "communes")) %>% 
        addPolylines(data = st_transform(routier, crs = 4326), color = "grey", opacity = 0.6, weight = 1.3 ,
                     stroke = TRUE, group = "Réseau routier principal",
                     options = pathOptions(pane = "réseau_routier")) %>% 
        addPolylines(data = st_transform(vferre, crs = 4326), color = "grey", opacity = 0.6, weight = 1.3 ,
                     stroke = TRUE, group = "Réseau ferré",  dashArray = 2,
                     options = pathOptions(pane = "voie_ferré")) %>%
        addCircleMarkers(lng = station$longitude, 
                         lat = station$latitude, 
                         radius = 2,
                         stroke = F,
                         color = "grey",
                         fillOpacity = 0.8,
                         group = "Stations ferroviaires",
                         options = pathOptions(pane = "station")) %>%
        addRasterImage(x = SelecPotential(), colors = PotentialPalette(SelecPotential()), opacity = 0.4) %>%
        addLegend(position = "bottomright",
                  colors = c("#B22222", "#E5E5E5", "#000080"),
                  labels = c("Surplus d'emplois (déficit d'actifs)",
                             "Équilibre actifs-emplois",
                             "Surplus d'actifs (déficit d'emplois)"))
    } else if(input$pottyp == "ori"){
      leafletProxy("mappot") %>%
        clearShapes() %>%
        clearImages() %>% clearShapes() %>% clearControls() %>%
        addPolygons(data = st_transform(shapeSf, crs = 4326), stroke = TRUE, weight = 0.5, opacity = 0.5, color = "grey", fill = TRUE,
                    fillColor = "grey", fillOpacity = 0, group = "Communes",
                    options = pathOptions(pane = "communes")) %>% 
        addPolylines(data = st_transform(routier, crs = 4326), color = "grey", opacity = 0.6, weight = 1.3 ,
                     stroke = TRUE, group = "Réseau routier principal",
                     options = pathOptions(pane = "réseau_routier")) %>% 
        addPolylines(data = st_transform(vferre, crs = 4326), color = "grey", opacity = 0.6, weight = 1.3 ,
                     stroke = TRUE, group = "Réseau ferré",  dashArray = 2,
                     options = pathOptions(pane = "voie_ferré")) %>%
        addCircleMarkers(lng = station$longitude, 
                         lat = station$latitude, 
                         radius = 2,
                         stroke = F,
                         color = "grey",
                         fillOpacity = 0.8,
                         group = "Stations ferroviaires",
                         options = pathOptions(pane = "station")) %>%
        addRasterImage(x = sqrt(SelecPotential()), colors = PotentialPalette(sqrt(SelecPotential())), opacity = 0.4) %>%
        addPolygons(data = DrawContour(), stroke = TRUE, fill = FALSE, color = "#a9a9a9", weight = 2,
                    label = paste(as.character(round(DrawContour()$center^2)), "actifs")) %>%
        addLegend(position = "bottomright",
                  colors = c("#B22222", "#E5E5E5"),
                  labels = c("Forte densité d'actifs",
                             "Faible densité d'actifs"))
    } else {
      leafletProxy("mappot") %>%
        clearShapes() %>%
        clearImages() %>% clearShapes() %>% clearControls() %>%
        addPolygons(data = st_transform(shapeSf, crs = 4326), stroke = TRUE, weight = 0.5, opacity = 0.5, color = "grey", fill = TRUE,
                    fillColor = "grey", fillOpacity = 0, group = "Communes",
                    options = pathOptions(pane = "communes")) %>% 
        addPolylines(data = st_transform(routier, crs = 4326), color = "grey", opacity = 0.6, weight = 1.3 ,
                     stroke = TRUE, group = "Réseau routier principal",
                     options = pathOptions(pane = "réseau_routier")) %>% 
        addPolylines(data = st_transform(vferre, crs = 4326), color = "grey", opacity = 0.6, weight = 1.3 ,
                     stroke = TRUE, group = "Réseau ferré",  dashArray = 2,
                     options = pathOptions(pane = "voie_ferré")) %>%
        addCircleMarkers(lng = station$longitude, 
                         lat = station$latitude, 
                         radius = 2,
                         stroke = F,
                         color = "grey",
                         fillOpacity = 0.8,
                         group = "Stations ferroviaires",
                         options = pathOptions(pane = "station")) %>%
        addRasterImage(x = sqrt(SelecPotential()), colors = PotentialPalette(sqrt(SelecPotential())), opacity = 0.4) %>%
        addPolygons(data = DrawContour(), stroke = TRUE, fill = FALSE, color = "#a9a9a9", weight = 2,
                    label = paste(as.character(round(DrawContour()$center^2)), "emplois")) %>%
        addLegend(position = "bottomright",
                  colors = c("#B22222", "#E5E5E5"),
                  labels = c("Forte densité d'emplois (destination)",
                             "Faible densité d'emplois (destination)"))
    }
    
    shinyjs::hideElement(id = 'loading')
  })
  
  # Dominant flow map Display  ####
  output$mapfluDom <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = FALSE, incl.data=TRUE)) %>%
      addMapPane("background_map", zIndex = 410) %>%    # Level 1
      addMapPane("communes", zIndex = 420) %>%          # Level 2
      addMapPane("réseau_routier", zIndex = 430) %>%    # Level 3
      addMapPane("voie_ferré", zIndex = 440) %>%        # Level 4
      addMapPane("station", zIndex = 450) %>%           # Level 5
      addMapPane("flux", zIndex = 460) %>%              # Level 6
      addMapPane("cercles", zIndex = 470) %>%           # Level 7
      
      addProviderTiles(provider = "Esri.WorldGrayCanvas",
                       options = providerTileOptions(minZoom = 8, maxZoom = 12)) %>%
      addLayersControl(
        position = "bottomright",
        overlayGroups = c("Communes", "Réseau routier principal", "Réseau ferré", "Stations ferroviaires"),
        options = layersControlOptions(collapsed = T)
      ) %>% 
      fitBounds(lng1 = 1.44, lat1 = 48.12, lng2 = 3.55, lat2 = 49.24)%>% 
      setMaxBounds(-0.05, 46.62, 5.05, 50.73) %>% 
      hideGroup("Réseau routier principal") %>% 
      hideGroup("Réseau ferré") %>% 
      hideGroup("Communes") %>% 
      hideGroup("Stations ferroviaires")
  })
  
  observe({
    shinyjs::showElement(id = 'loading')
    leafletProxy(mapId = "mapfluDom", data = c(f$dataflu,r$rayon,c$col)) %>% 
      clearShapes() %>%
      addPolygons(data = st_transform(shapeSf, crs = 4326), stroke = TRUE, weight = 0.5, opacity = 0.5, color = "grey", fill = TRUE,
                  fillColor = "grey", fillOpacity = 0, group = "Communes",
                  options = pathOptions(pane = "communes")) %>% 
      addPolylines(data = st_transform(routier, crs = 4326), color = "grey", opacity = 0.6, weight = 1.3 ,
                   stroke = TRUE, group = "Réseau routier principal",
                   options = pathOptions(pane = "réseau_routier")) %>% 
      addPolylines(data = st_transform(vferre, crs = 4326), color = "grey", opacity = 0.6, weight = 1.3 ,
                   stroke = TRUE, group = "Réseau ferré",  dashArray = 2,
                   options = pathOptions(pane = "voie_ferré")) %>% 
      addPolylines(data = st_transform(f$dataflu, crs = 4326), color = "royalblue", opacity = 0.1, weight = 1 ,
                   stroke = TRUE,
                   options = pathOptions(pane = "flux")) %>%
      addCircleMarkers(lng = station$longitude, 
                       lat = station$latitude, 
                       radius = 2,
                       stroke = F,
                       color = "grey",
                       fillOpacity = 0.8,
                       group = "Stations ferroviaires",
                       options = pathOptions(pane = "station")) %>%
      addCircles(lng = c$cercle[["lon"]], 
                 lat = c$cercle[["lat"]], 
                 radius = r$rayon, 
                 color = ~colorNumeric(palette = c("#849EB8", "#4D82B8","#375E84","#283038"), domain = domFlowJob[[1]][["STATUS"]])(domFlowJob[[1]][["STATUS"]]), 
                 stroke = F,
                 fillOpacity = 0.6,
                 highlight = highlightOptions(
                   weight = 5,
                   color = "white",
                   opacity = 1,
                   fillOpacity = 0.8,
                   sendToBack = TRUE),
                 label = sprintf(
                   "<strong>%s</strong><br/> %s %.0f", 
                   nc$comm,
                   nf$nom,
                   vc$valCercle
                 )%>% lapply(htmltools::HTML),
                 labelOptions = labelOptions(
                   style = list("font-weight" = "normal", 
                                padding = "3px 8px"),
                   textsize = "15px",
                   direction = "auto"),
                 options = pathOptions(pane = "cercles")
      )
    shinyjs::hideElement(id = 'loading')
  })
  
  
  # FUNCTIONS  ####
  SelecPotential <- reactive({
    req(input$pottyp, input$potcat)
    if(input$potcat %in% c("femm", "homm")){
      keyRas <- paste(input$pottyp, input$potcat, "tout", "tout", sep = "_")
    } else if (input$potcat %in% c("agri", "arti", "cadr", "inte", "empl", "ouvr")){
      keyRas <- paste(input$pottyp, "tout", input$potcat, "tout", sep = "_")
    } else if (input$potcat %in% c("vp", "tc")){
      keyRas <- paste(input$pottyp, "tout", "tout", input$potcat, sep = "_")
    } else {
      keyRas <- paste(input$pottyp, "tout", "tout", "tout", sep = "_")
    }
    oneRaster <- listPotentials[[keyRas]]
    return(oneRaster)
  })
  
  DrawContour <- reactive({
    potCont <- PotentialContour(ras = sqrt(SelecPotential()))
    return(potCont)
  })
  
  GetTopLinks <- reactive({
    req(input$fluref, input$fluvar, input$flucom, input$fluthr)
    topLinks <- GetLinks(tabnav = tabFlows, spcom = shapeSf, ref = input$fluref, mod = input$flumod, varsort = input$fluvar, oneunit = substring(input$flucom, 9), thres = input$fluthr)
    return(topLinks)
  })
  
  Get_CityValue <- reactive({
    req(input$fluref, input$flucom)
    cityValue <- city_Value(matflow = matflow, spcom = shapeSf, od = input$fluref, city = substr(input$flucom, 1, 5))
    return(cityValue)
  })
  
})

