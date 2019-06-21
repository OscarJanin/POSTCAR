shinyServer(function(input, output, session) {
  
  # Graphic Display  ####
  output$plot1 <- renderPlotly({
    plot_ly(as.data.frame(commData), x = ~RelBal, y = ~AutoSuff)
  })
  
  
  # Get reactive value based on radiobutton (for 4th panel "flow")  ####
  f <- reactiveValues(dataflu = Filter$domFlowJob[[2]])
  r <- reactiveValues(rayon = (sqrt(Filter$domFlowJob[[1]][["TOTDES"]])/pi)*17)
  c <- reactiveValues(cercle = Filter$domFlowJob[[1]])
  vc <- reactiveValues(valCercle = Filter$domFlowJob[[1]][["TOTDES"]])
  nf <- reactiveValues(nom = "Emploi : ")
  nc <- reactiveValues(comm = toupper(Filter$domFlowJob[[1]][["nomcom"]]))
  l2 <- reactiveValues(layer2 = "")
  s <- reactiveValues(size = 1.5)
  o <- reactiveValues(opacity = 0.1)
  
  observeEvent(input$radioFlu,{
    if(input$radioFlu=="iEmploi"){
      f$dataflu <- Filter$domFlowJob[[2]]
      r$rayon <- (sqrt(Filter$domFlowJob[[1]][["TOTDES"]])/pi)*20
      c$cercle <- Filter$domFlowJob[[1]]
      vc$valCercle <- Filter$domFlowJob[[1]][["TOTDES"]]
      nf$nom <- "Emploi : "
      nc$comm <- toupper(Filter$domFlowJob[[1]][["nomcom"]])
      l2$layer2 <- "hotspot"
      s$size <- 1.5
      o$opacity <- 0.2}
    if(input$radioFlu=="iPopulation"){
      f$dataflu <- Filter$domFlowPop[[2]]
      r$rayon <- (sqrt(Filter$domFlowPop[[1]][["TOTORI"]])/pi)*20
      c$cercle <- Filter$domFlowPop[[1]]
      vc$valCercle <- Filter$domFlowPop[[1]][["TOTORI"]]
      nf$nom <- "Population : "
      nc$comm <- toupper(Filter$domFlowPop[[1]][["nomcom"]])
      l2$layer2 <- "hotspot"
      s$size <- 1.5
      o$opacity <- 0.2}
    if(input$radioFlu=="iEmpPop"){
      f$dataflu <- Filter$domFlowJP[[2]]
      r$rayon <- (sqrt(Filter$domFlowJP[[1]][["TOTORIDES"]])/pi)*20
      c$cercle <- Filter$domFlowJP[[1]]
      vc$valCercle <- Filter$domFlowJP[[1]][["TOTORIDES"]]
      nf$nom <- "Flux intra-communaux : "
      nc$comm <- toupper(Filter$domFlowJP[[1]][["nomcom"]])
      l2$layer2 <- "hotspot"
      s$size <- 1.5
      o$opacity <- 0.2}
    if(input$radioFlu=="integrated"){
      f$dataflu <- Filter$icdrI
      l2$layer2 <- "dominant"
      s$size <- 5
      o$opacity <- 1}
    if(input$radioFlu=="convergent"){
      f$dataflu <- Filter$icdrC
      l2$layer2 <- "dominant"
      s$size <- 0.6
      o$opacity <- 0.4}
    if(input$radioFlu=="divergent"){
      f$dataflu <- Filter$icdrD
      l2$layer2 <- "dominant"
      s$size <- 0.6
      o$opacity <- 0.4}
  })

  
  # Indicators map Display  ####
  
  output$mapIndic <- renderLeaflet({
    index <- Get_Index()
    leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
      addMapPane("background_map", zIndex = 410) %>%  # Level 1
      addMapPane("réseau_routier", zIndex = 420) %>%  # Level 2
      addMapPane("voie_ferré", zIndex = 430) %>%  # Level 3
      addMapPane("station", zIndex = 440) %>%  # Level 4
      addMapPane("cercles", zIndex = 450) %>%  # Level 5
      addProviderTiles(provider = "CartoDB.Positron",
                       options = providerTileOptions(minZoom = 8, maxZoom = 12, opacity = 0.5)) %>%
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
      hideGroup(index$layer) %>%
      hideGroup(index$polygons)
  })
  
  
  observe({
    shinyjs::showElement(id = 'loading')
    index <- Get_Index()
    leafletProxy("mapIndic", data = index$commData) %>%
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
                 lng = index$commData[["lon"]],
                 lat = index$commData[["lat"]],
                 radius = (sqrt(index$data)/pi)*17,
                 color = "#54278F",
                 stroke = F,
                 fillOpacity = 0.6,
                 highlight = highlightOptions(
                   weight = 5,
                   color = "white",
                   opacity = 1,
                   fillOpacity = 0.8,
                   bringToFront = F),
                 label = sprintf(
                   "<strong>%s</strong><br/> %.0f %s",
                   toupper(index$commData$nomcom),
                   index$data,
                   index$unit
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
        fillColor = ~colorBin(palette = index$color,
                              bins = index$breaks,
                              domain = index$data)(index$data),
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
          toupper(index$commData$nomcom),
          index$nom,
          index$data,
          index$unit
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
    index <- Get_Index()
    proxy <- leafletProxy("mapIndic", data =index$commData)
    proxy %>% clearControls()
    if (input$radioMobi=="emploi" | input$radioMobi=="popact") {
    }
    else {
      proxy %>% addLegend(pal = colorBin(palette = index$color,
                               bins = index$breaks,
                               domain = index$data,pretty = TRUE),
                values = ~index$data, opacity = 0.7,
                title = NULL, position = "bottomright"
      )
    }
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
                       options = providerTileOptions(minZoom = 8, maxZoom = 12, opacity = 0.5)) %>%
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
    leafletProxy("mapflu", data = cityVal[[1]]) %>%
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
      addPolylines(data = topDes, color = "black", opacity = 0.8, weight = 1.2, stroke = TRUE, options = pathOptions(pane = "lignes")) %>%
      addPolygons(
        fillColor = ~colorBin(palette = "Purples",
                              bins = getBreaks(cityVal[[1]][["DATA"]], nclass = 6,method = "fisher-jenks"),
                              domain = cityVal[[1]][["DATA"]])(cityVal[[1]][["DATA"]]),
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
          "<strong>%s</strong><br/> %.0f %s",
          toupper(cityVal[[1]][["nomcom"]]),
          cityVal[[1]][["DATA"]],
          cityVal[[2]]
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
    proxy <- leafletProxy("mapflu", data =cityVal[[1]])
    proxy %>% clearControls()
    proxy %>% addLegend(pal = colorBin(palette = "Purples", 
                                       bins = round(getBreaks(cityVal[[1]][["DATA"]], nclass = 6,method = "fisher-jenks")),
                                       domain = cityVal[[1]][["DATA"]],pretty = TRUE),
                        values = ~cityVal[[1]][["DATA"]], opacity = 0.7,
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
                       options = providerTileOptions(minZoom = 8, maxZoom = 12, opacity = 0.5)) %>%
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
                    label = paste(as.character(round(DrawContour()$center^2)), "actifs"),
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto")) %>%
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
                    label = paste(as.character(round(DrawContour()$center^2)), "emplois"),
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto")) %>%
        addLegend(position = "bottomright",
                  colors = c("#B22222", "#E5E5E5"),
                  labels = c("Forte densité d'emplois (destination)",
                             "Faible densité d'emplois (destination)"))
    }
    
    shinyjs::hideElement(id = 'loading')
  })
  
  # Dominant flow map Display  ####
  output$mapfluDom <- renderLeaflet({
    structure <- Get_Structure()
    leaflet(options = leafletOptions(zoomControl = FALSE, incl.data=TRUE)) %>%
      addMapPane("background_map", zIndex = 410) %>%    # Level 1
      addMapPane("communes", zIndex = 420) %>%          # Level 2
      addMapPane("réseau_routier", zIndex = 430) %>%    # Level 3
      addMapPane("voie_ferré", zIndex = 440) %>%        # Level 4
      addMapPane("station", zIndex = 450) %>%           # Level 5
      addMapPane("flux", zIndex = 460) %>%              # Level 6
      addMapPane("cercles", zIndex = 470) %>%           # Level 7
      
      addProviderTiles(provider = "CartoDB.Positron",
                       options = providerTileOptions(minZoom = 8, maxZoom = 12, opacity = 0.5)) %>%
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
      hideGroup("Stations ferroviaires")%>%
      hideGroup(structure$layer2)
  })
  
  observe({
    structure <- Get_Structure()
    shinyjs::showElement(id = 'loading')
    leafletProxy(mapId = "mapfluDom", data = c(structure$dataflu,structure$rayon,structure$col)) %>% 
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
      addPolylines(data = st_transform(structure$dataflu, crs = 4326), color = "#82909E", opacity = structure$opacity, weight = s$size ,
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
      addCircles(lng = structure$cercle[["lon"]], 
                 lat = structure$cercle[["lat"]], 
                 radius = r$rayon, 
                 color = ~colorNumeric(palette = c("#B35605","#F1A340","#828F9E"), domain = structure$cercle[["status"]])(structure$cercle[["status"]]),
                 stroke = F,
                 fillOpacity = 0.8,
                 highlight = highlightOptions(
                   weight = 5,
                   color = "white",
                   opacity = 1,
                   fillOpacity = 1,
                   bringToFront = F),
                 label = sprintf(
                   "<strong>%s</strong><br/> %s %.0f", 
                   structure$comm,
                   structure$nom,
                   structure$valCercle
                 )%>% lapply(htmltools::HTML),
                 labelOptions = labelOptions(
                   style = list("font-weight" = "normal", 
                                padding = "3px 8px"),
                   textsize = "15px",
                   direction = "auto"),
                 options = pathOptions(pane = "cercles"),
                 group = "dominant"
      ) %>% 
      addCircles(lng = structure$hotspot[["lon"]], 
                 lat = structure$hotspot[["lat"]], 
                 radius = 500, 
                 color = ifelse(structure$hotspot[["status"]] == "Hotspot de travail", "red", "blue"),
                 stroke = F,
                 fillOpacity = 0.8,
                 highlight = highlightOptions(
                   weight = 5,
                   color = "white",
                   opacity = 1,
                   fillOpacity = 1,
                   bringToFront = F),
                 label = sprintf(
                   "<strong>%s</strong><br/> %s", 
                   toupper(structure$hotspot[["nomcom"]]),
                   structure$hotspot[["status"]]
                 )%>% lapply(htmltools::HTML),
                 labelOptions = labelOptions(
                   style = list("font-weight" = "normal", 
                                padding = "3px 8px"),
                   textsize = "15px",
                   direction = "auto"),
                 options = pathOptions(pane = "cercles"),
                 group = "hotspot"
      )
    shinyjs::hideElement(id = 'loading')
  })
  
  observe({
    structure <- Get_Structure()
    proxy <- leafletProxy("mapfluDom", data =c(structure$dataflu,structure$rayon,structure$col))
    proxy %>% clearControls()
    if (input$radioFlu=="integrated" | input$radioFlu=="convergent" | input$radioFlu=="divergent") {
      proxy %>% addLegendCustom(colors = c("red", "blue"), labels = c("Hotspot de travail", "Hotspot résidentiel"), sizes = c(20, 20))
    }
    else {
      proxy %>% addLegendCustom(colors = c("#B35605","#F1A340","#97A7B8"), labels = c("Dominant", "Intermédiaire", "Dominé"), sizes = c(25, 20, 15))
    }
  })
  
  
  
  
  # description ----
  
  observeEvent(input$index_descr, {
    showModal(modalDialog(
      includeHTML("coat/index_descr.html"),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  observeEvent(input$flux_descr, {
    showModal(modalDialog(
      includeHTML("coat/flux_descr.html"),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  observeEvent(input$pool_descr, {
    showModal(modalDialog(
      includeHTML("coat/pool_descr.html"),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  observeEvent(input$fludom_descr, {
    showModal(modalDialog(
      includeHTML("coat/fludom_descr.html"),
      easyClose = TRUE,
      footer = NULL
    ))
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
    topLinks <- GetLinks(tabnav = Get_Filter_Flux(), ref = input$fluref, varsort = input$fluvar, oneunit = substring(input$flucom, 9), thres = input$fluthr)
    return(topLinks)
  })
  
  Get_CityValue <- reactive({
    req(input$fluref, input$flucom, input$fluvar)
    cityValue <- city_Value(tabflows = Get_Filter_Flux(), matDist = matDist, pol = pol,idpol = "insee", var = input$fluvar, od = input$fluref, city = substr(input$flucom, 1, 5))
    return(cityValue)
  })

  Get_Filter_Flux <- reactive({
    req(input$FiltreFlux)
    shinyjs::showElement(id = 'loading')
    if(input$FiltreFlux == "Tout"){
      variable <- NULL
      label <- NULL
    } else if(input$FiltreFlux == "Agriculteurs exploitants"){
      variable <- "CSP"
        label <- 1
    } else if(input$FiltreFlux == "Artisans, commerçants et chefs d'entreprise"){
      variable <- "CSP"
        label <- 2
    } else if(input$FiltreFlux == "Cadres et professions intellectuelles supérieures"){
      variable <- "CSP"
        label <- 3
    } else if(input$FiltreFlux == "Professions Intermédiaires"){
      variable <- "CSP"
        label <- 4
    } else if(input$FiltreFlux == "Employés"){
      variable <- "CSP"
        label <- 5
    } else if(input$FiltreFlux == "Ouvriers"){
      variable <- "CSP"
        label <- 6
    } else if(input$FiltreFlux == "DOMICILE"){
      variable <- "MODE"
        label <- "DOMICILE"
    } else if(input$FiltreFlux == "NM"){
      variable <- "MODE"
        label <- "NM"
    } else if(input$FiltreFlux == "TC"){
      variable <- "MODE"
      label <- "TC"
    } else if(input$FiltreFlux == "VP"){
      variable <- "MODE"
      label <- "VP"
    }
    filterFlux <- Filter_flux(tabFlows, variable = variable, label = label)
    shinyjs::hideElement(id = 'loading')
    return(filterFlux)
  })
  
  Get_Index <- reactive({
    req(input$radioMobi)
    index <- Index(mobi = input$radioMobi, commData = Get_Filter_Index())
    return(index)
  })
  
  Get_Filter_Index <- reactive({
    req(input$FiltreIndices)
    shinyjs::showElement(id = 'loading')
    if(input$FiltreIndices == "Tout"){
      variable <- NULL
      label <- NULL
    } else if(input$FiltreIndices == "Agriculteurs exploitants"){
      variable <- "CSP"
      label <- 1
    } else if(input$FiltreIndices == "Artisans, commerçants et chefs d'entreprise"){
      variable <- "CSP"
      label <- 2
    } else if(input$FiltreIndices == "Cadres et professions intellectuelles supérieures"){
      variable <- "CSP"
      label <- 3
    } else if(input$FiltreIndices == "Professions Intermédiaires"){
      variable <- "CSP"
      label <- 4
    } else if(input$FiltreIndices == "Employés"){
      variable <- "CSP"
      label <- 5
    } else if(input$FiltreIndices == "Ouvriers"){
      variable <- "CSP"
      label <- 6
    } else if(input$FiltreIndices == "DOMICILE"){
      variable <- "MODE"
      label <- "DOMICILE"
    } else if(input$FiltreIndices == "NM"){
      variable <- "MODE"
      label <- "NM"
    } else if(input$FiltreIndices == "TC"){
      variable <- "MODE"
      label <- "TC"
    } else if(input$FiltreIndices == "VP"){
      variable <- "MODE"
      label <- "VP"
    }
    filterIndice <- Filter_indice(tabFlows, idori = "ORI", iddes = "DES", idflow = "FLOW", iddist = "DIST", pol, idpol = "insee", variable = variable, label = label)
    shinyjs::hideElement(id = 'loading')
    return(filterIndice)
  })
  
  Get_Structure <- reactive({
    req(input$radioFlu) 
    structure <- Structure(Flu = input$radioFlu ,domFlowJob = Get_Filter_Structure()$domFlowJob ,domFlowPop = Get_Filter_Structure()$domFlowPop,domFlowJP = Get_Filter_Structure()$domFlowJP,
                           icdrI = Get_Filter_Structure()$icdrI,icdrC = Get_Filter_Structure()$icdrC,icdrD = Get_Filter_Structure()$icdrD, hotspot = Get_Filter_Structure()$hotspot)
    return(structure)
  })
  
  Get_Filter_Structure <- reactive({
    req(input$FiltreStructure)
    shinyjs::showElement(id = 'loading')
    if(input$FiltreStructure == "Tout"){
      variable <- NULL
      label <- NULL
    } else if(input$FiltreStructure == "Agriculteurs exploitants"){
      variable <- "CSP"
      label <- 1
    } else if(input$FiltreStructure == "Artisans, commerçants et chefs d'entreprise"){
      variable <- "CSP"
      label <- 2
    } else if(input$FiltreStructure == "Cadres et professions intellectuelles supérieures"){
      variable <- "CSP"
      label <- 3
    } else if(input$FiltreStructure == "Professions Intermédiaires"){
      variable <- "CSP"
      label <- 4
    } else if(input$FiltreStructure == "Employés"){
      variable <- "CSP"
      label <- 5
    } else if(input$FiltreStructure == "Ouvriers"){
      variable <- "CSP"
      label <- 6
    } else if(input$FiltreStructure == "DOMICILE"){
      variable <- "MODE"
      label <- "DOMICILE"
    } else if(input$FiltreStructure == "NM"){
      variable <- "MODE"
      label <- "NM"
    } else if(input$FiltreStructure == "TC"){
      variable <- "MODE"
      label <- "TC"
    } else if(input$FiltreStructure == "VP"){
      variable <- "MODE"
      label <- "VP"
    } 
    filterStructure <- Filter_structure(tabFlows, idflow = "FLOW", before, after, pol, idpol = "insee", namepol = "nomcom", nameAgr = "Paris", variable = variable, label = label)
    shinyjs::hideElement(id = 'loading')
    return(filterStructure)
  })
  
  })