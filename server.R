shinyServer(function(input, output, session) {
  
  # Graphic Display  ####
  output$plot1 <- renderPlotly({
    plot_ly(as.data.frame(commData), x = ~RelBal, y = ~AutoSuff)
  })
  
  
  
  # Get reactive value based on radiobutton (for 1st panel "Indicators")  ####
  v <- reactiveValues(data = commData$TOTDES)
  n <- reactiveValues(nom = "Emploi : ")
  u <- reactiveValues(unit = "")
  c <- reactiveValues(color = "PuOr")
  b <- reactiveValues(breaks = sort(append(0,getBreaks(commData$RelBal,nclass = 6,method = "fisher-jenks"))))
  l <- reactiveValues(layer = "taux")
  p <- reactiveValues(polygons = "")
  
  observeEvent(input$radioMobi,{
    if(input$radioMobi=="emploi"){
      v$data <- commData$TOTDES
      n$nom <- "Emploi : "
      u$unit <- ""
      c$color <- "PuOr"
      b$breaks <- sort(append(0,getBreaks(commData$RelBal,nclass = 6,method = "fisher-jenks")))
      l$layer <- "taux"
      p$polygons <- ""}
    if(input$radioMobi=="popact"){
      v$data <- commData$TOTORI
      n$nom <- "Population active : "
      u$unit <- ""
      c$color <- "PuOr"
      b$breaks <- sort(append(0,getBreaks(commData$RelBal,nclass = 6,method = "fisher-jenks")))
      l$layer <- "taux"
      p$polygons <- ""}
    if(input$radioMobi=="soldeRel"){
      v$data <- commData$RelBal
      n$nom <- "Solde relatif : "
      u$unit <- ""
      c$color <- "PuOr"
      b$breaks <- sort(append(0,getBreaks(commData$RelBal,nclass = 6,method = "fisher-jenks")))
      l$layer <- "stock"
      p$polygons <- "communes"}
    if(input$radioMobi=="Contention"){
      v$data <- commData$Contention
      n$nom <- "Auto-Contention : "
      u$unit <- ""
      c$color <- "Purples"
      b$breaks <- getBreaks(commData$Contention,nclass = 6,method = "fisher-jenks")
      l$layer <- "stock"
      p$polygons <- "communes"}
    if(input$radioMobi=="Suffisance"){
      v$data <- commData$AutoSuff
      n$nom <- "Auto-Suffisance : "
      u$unit <- ""
      c$color <- "Purples"
      b$breaks <- getBreaks(commData$AutoSuff,nclass = 6,method = "fisher-jenks")
      l$layer <- "stock"
      p$polygons <- "communes"}
    if(input$radioMobi=="Mobility"){
      v$data <- commData$Mobility
      n$nom <- "Mobilité : "
      u$unit <- ""
      c$color <- "Purples"
      b$breaks <- getBreaks( commData$Mobility,nclass = 6,method = "fisher-jenks")
      l$layer <- "stock"
      p$polygons <- "communes"}
    if(input$radioMobi=="meanDistOri"){
      v$data <- commData$MEANDISTORI
      n$nom <- "Distance moyenne à l'origine : "
      u$unit <- "km"
      c$color <- "Purples"
      b$breaks <- getBreaks(commData$MEANDISTORI,nclass = 6,method = "fisher-jenks")
      l$layer <- "stock"
      p$polygons <- "communes"}
    if(input$radioMobi=="meanDistDes"){
      v$data <- commData$MEANDISTDES
      n$nom <- "Distance moyenne à destination : "
      u$unit <- "km"
      c$color <- "Purples"
      b$breaks <- getBreaks(commData$MEANDISTDES,nclass = 6,method = "fisher-jenks")
      l$layer <- "stock"
      p$polygons <- "communes"}
    if(input$radioMobi=="perOri"){
      v$data <- commData$perOri
      n$nom <- "Part des flux à l'origine : "
      u$unit <- "%"
      c$color <- "Purples"
      b$breaks <- getBreaks( commData$perOri,nclass = 6,method = "fisher-jenks")
      l$layer <- "stock"
      p$polygons <- "communes"}
    if(input$radioMobi=="perDes"){
      v$data <- commData$perDes
      n$nom <- "Part des flux à la destination : "
      u$unit <- "%"
      c$color <- "Purples"
      b$breaks <- getBreaks(commData$perDes,nclass = 6,method = "fisher-jenks")
      l$layer <- "stock"
      p$polygons <- "communes"}
  })
  
  
  # Get reactive value based on radiobutton (for 4th panel "flow")  ####
  f <- reactiveValues(dataflu = domFlowJob[[2]])
  r <- reactiveValues(rayon = (sqrt(domFlowJob[[1]][["TOTDES"]])/pi)*17)
  c <- reactiveValues(cercle = domFlowJob[[1]])
  vc <- reactiveValues(valCercle = domFlowJob[[1]][["TOTDES"]])
  nf <- reactiveValues(nom = "Emploi : ")
  nc <- reactiveValues(comm = domFlowJob[[1]][["nomcom"]])
  
  observeEvent(input$radioFlu,{
    if(input$radioFlu=="iEmploi"){
      f$dataflu <- domFlowJob[[2]]
      r$rayon <- (sqrt(domFlowJob[[1]][["TOTDES"]])/pi)*17
      c$cercle <- domFlowJob[[1]]
      vc$valCercle <- domFlowJob[[1]][["TOTDES"]]
      nf$nom <- "Emploi : "
      nc$comm <- domFlowJob[[1]][["nomcom"]]}
    if(input$radioFlu=="iPopulation"){
      f$dataflu <- domFlowPop[[2]]
      r$rayon <- (sqrt(domFlowPop[[1]][["TOTORI"]])/pi)*17
      c$cercle <- domFlowPop[[1]]
      vc$valCercle <- domFlowPop[[1]][["TOTORI"]]
      nf$nom <- "Population : "
      nc$comm <- domFlowPop[[1]][["nomcom"]]}
    if(input$radioFlu=="iEmpPop"){
      f$dataflu <- domFlowJP[[2]]
      r$rayon <- (sqrt(domFlowJP[[1]][["TOTINTRA"]])/pi)*17
      c$cercle <- domFlowJP[[1]]
      vc$valCercle <- domFlowJP[[1]][["TOTINTRA"]]
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
      addMapPane("cercles", zIndex = 450) %>%  # Level 5
      addProviderTiles(provider = "CartoDB.Positron",
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
      hideGroup("Stations ferroviaires")%>%
      hideGroup(l$layer) %>%
      hideGroup(p$polygons)
  })
  
  
  observe({
    shinyjs::showElement(id = 'loading')
    leafletProxy("mapIndic", data = commData) %>%
      clearShapes() %>%
      addPolygons(data = st_transform(pol, crs = 4326), stroke = TRUE, weight = 0.5, opacity = 0.5, color = "grey", fill = TRUE,
                  fillColor = "grey", fillOpacity = 0, group = "communes") %>% 
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
      addCircles(
                 lng = commData[["lon"]],
                 lat = commData[["lat"]],
                 radius = (sqrt(v$data)/pi)*17,
                 color = "purple",
                 stroke = F,
                 fillOpacity = 0.6,
                 highlight = highlightOptions(
                   weight = 5,
                   color = "white",
                   opacity = 1,
                   fillOpacity = 0.8,
                   bringToFront = F),
                 label = sprintf(
                   "<strong>%s</strong><br/> %s %.0f",
                   commData$nomcom,
                   n$nom,
                   v$data
                 )%>% lapply(htmltools::HTML),
                 labelOptions = labelOptions(
                   style = list("font-weight" = "normal",
                                padding = "3px 8px"),
                   textsize = "15px",
                   direction = "auto"),
                 options = pathOptions(pane = "cercles"),
                 group = "stock"
                 )%>%
      addPolygons(
        fillColor = ~colorBin(palette = c$color,
                              bins = b$breaks,
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
          "<strong>%s</strong><br/> %s %.2f %s",
          commData$nomcom,
          n$nom,
          v$data,
          u$unit
        )%>% lapply(htmltools::HTML),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"),
        options = pathOptions(pane = "réseau_routier"),
        group = "taux"
      )
    shinyjs::hideElement(id = 'loading')
  })
  
  observe({
    proxy <- leafletProxy("mapIndic", data =commData)
    proxy %>% clearControls()
    proxy %>% addLegend(pal = colorBin(palette = c$color, 
                                       bins = b$breaks,
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
      addMapPane("station", zIndex = 460) %>%           # Level 6
      addMapPane("lignes", zIndex = 470) %>%           # Level 7
      addProviderTiles(provider = "CartoDB.Positron",
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
      addPolylines(data = topDes, color = "black", opacity = 0.8, weight = 1.5, stroke = TRUE, options = pathOptions(pane = "lignes")) %>%
      addPolygons(
        fillColor = ~colorBin(palette = "Purples",
                              bins = round(getBreaks(cityVal$tabCityFlow, nclass = 6,method = "fisher-jenks")),
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
          "<strong>%s</strong><br/> %.0f",
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
                                       bins = round(getBreaks(cityVal$tabCityFlow, nclass = 6,method = "fisher-jenks")),
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
      addProviderTiles(provider = "CartoDB.Positron",
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
        addPolygons(data = st_transform(pol, crs = 4326), stroke = TRUE, weight = 0.5, opacity = 0.5, color = "grey", fill = TRUE,
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
        addPolygons(data = st_transform(pol, crs = 4326), stroke = TRUE, weight = 0.5, opacity = 0.5, color = "grey", fill = TRUE,
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
        addPolygons(data = st_transform(pol, crs = 4326), stroke = TRUE, weight = 0.5, opacity = 0.5, color = "grey", fill = TRUE,
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
      
      addProviderTiles(provider = "CartoDB.Positron",
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
      addPolygons(data = st_transform(pol, crs = 4326), stroke = TRUE, weight = 0.5, opacity = 0.5, color = "grey", fill = TRUE,
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
                 color = ~colorNumeric(palette = c("#283038","#375E84","#4D82B8"), domain = domFlowJob[[1]][["status"]])(domFlowJob[[1]][["status"]]),
                 stroke = F,
                 fillOpacity = 0.6,
                 highlight = highlightOptions(
                   weight = 5,
                   color = "white",
                   opacity = 1,
                   fillOpacity = 0.8,
                   bringToFront = F),
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
    topLinks <- GetLinks(tabnav = tabFlows, ref = input$fluref, mod = input$flumod, varsort = input$fluvar, oneunit = substring(input$flucom, 9), thres = input$fluthr)
    return(topLinks)
  })
  
  Get_CityValue <- reactive({
    req(input$fluref, input$flucom)
    cityValue <- city_Value(matflow = matflow, spcom = pol, od = input$fluref, city = substr(input$flucom, 1, 5))
    return(cityValue)
  })
  
})

