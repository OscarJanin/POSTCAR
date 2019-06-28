shinyServer(function(input, output, session) {
  
  # Graphic Display  ####
  output$plot1 <- renderPlotly({
    plot_ly(as.data.frame(commData), x = ~RelBal, y = ~AutoSuff)
  })
  
  # Indicators map Display  ####
  
  output$mapIndic <- renderLeaflet({
    
    index <- Get_Index()
    
    leaflet(options = leafletOptions(zoomControl = FALSE, minZoom = 8, maxZoom = 13)) %>%
      addMapPane("choropleth", zIndex = 410) %>%  # Level 1
      addMapPane("réseau_routier", zIndex = 420) %>%  # Level 2
      addMapPane("voie_ferré", zIndex = 430) %>%  # Level 3
      addMapPane("station", zIndex = 440) %>%  # Level 4
      addMapPane("cercles", zIndex = 450) %>%  # Level 5
      addMapPane("label", zIndex = 460) %>%  # Level 6
      addProviderTiles(provider = "CartoDB.PositronNoLabels",
                       options = providerTileOptions(opacity = 0.5)) %>%
      addProviderTiles(provider = "CartoDB.PositronOnlyLabels",
                       options = pathOptions(pane = "label", opacity = 0.5)) %>% 
      addLayersControl(
        position = "bottomright",
        overlayGroups = c("Réseau routier principal", "Réseau ferré","Stations ferroviaires"),
        options = layersControlOptions(collapsed = T)
      ) %>% 
      fitBounds(lng1 = 1.44, lat1 = 48.12, lng2 = 3.55, lat2 = 49.24)%>% 
      setMaxBounds(-0.05, 46.62, 5.05, 50.73) %>% 
      hideGroup("Réseau routier principal") %>% 
      hideGroup("Réseau ferré") %>% 
      hideGroup("Stations ferroviaires") %>%
      hideGroup(index$polygons)  %>% 
      hideGroup(index$layer)
  })
  
  
  observe({
    index <- Get_Index()
    shinyjs::showElement(id = 'loading-content')
    shinyjs::showElement(id = 'loading')
    leafletProxy("mapIndic", data = index$commdata) %>% 
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
      addCircles(lng = index$commdata[["lon"]],
                 lat = index$commdata[["lat"]],
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
                   toupper(index$commdata$nomcom),
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
          toupper(index$commdata$nomcom),
          index$nom,
          index$data,
          index$unit
        )%>% lapply(htmltools::HTML),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"),
        options = pathOptions(pane = "choropleth"),
        group = "taux"
      )
    shinyjs::hideElement(id = 'loading-content')
    shinyjs::hideElement(id = 'loading')
  })
  
  observe({
    index <- Get_Index()
    proxy <- leafletProxy("mapIndic", data =index$commdata)
    proxy %>% clearControls()
    proxy %>%  hideGroup(index$polygons) 
    proxy %>%  hideGroup(index$layer)
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
    lflFlow <- leaflet(options = leafletOptions(zoomControl = FALSE, minZoom = 8, maxZoom = 13)) %>%
      addMapPane("communes", zIndex = 410) %>%  # Level 1
      addMapPane("réseau_routier", zIndex = 420) %>%  # Level 2
      addMapPane("voie_ferré", zIndex = 430) %>%  # Level 3
      addMapPane("station", zIndex = 440) %>%  # Level 4
      addMapPane("lignes", zIndex = 450) %>%  # Level 5
      addMapPane("label", zIndex = 460) %>%  # Level 6
      addProviderTiles(provider = "CartoDB.PositronNoLabels",
                       options = providerTileOptions(opacity = 0.5)) %>%
      addProviderTiles(provider = "CartoDB.PositronOnlyLabels",
                       options = pathOptions(pane = "label", opacity = 0.5)) %>% 
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
    
    topDes <- isolate(GetTopLinks())
    cityVal <- isolate(Get_CityValue())
    lflFlow %>% 
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
      addPolylines(data = topDes[[1]], color = "black", opacity = 0.8, weight = 1.2, stroke = TRUE, options = pathOptions(pane = "lignes"), group = "lignes") %>% 
      addPolygons(data = cityVal[[1]], fillColor = colorBin(palette = "Purples",
                                                            bins = cityVal[[3]],
                                                            domain = cityVal[[1]][["DATA"]])(cityVal[[1]][["DATA"]])
                  ,
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
      ) %>%
      addLegend(pal = colorBin(palette = "Purples",
                               bins = cityVal[[3]],
                               domain = cityVal[[1]][["DATA"]],pretty = TRUE),
                values = cityVal[[1]][["DATA"]], opacity = 0.7,
                title = NULL, position = "bottomright")
    
  })
  
  observe({
    topDes <- GetTopLinks()
    cityVal <- Get_CityValue()
    shinyjs::showElement(id = 'loading-content')
    shinyjs::showElement(id = 'loading')
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
      addPolylines(data = topDes[[1]], color = "black", opacity = 0.8, weight = 1.2, stroke = TRUE, options = pathOptions(pane = "lignes"), group = "lignes") %>% 
      addPolygons(
        fillColor = ~colorBin(palette = "Purples",
                              bins = cityVal[[3]],
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
    shinyjs::hideElement(id ='loading-content')
    shinyjs::hideElement(id ='loading')
  })
  
  observe({
    cityVal <- Get_CityValue()
    proxy <- leafletProxy("mapflu", data =cityVal[[1]])
    proxy %>% clearControls()
    proxy %>% addLegend(pal = colorBin(palette = "Purples",
                                       bins = cityVal[[3]],
                                       domain = cityVal[[1]][["DATA"]],pretty = TRUE),
                        values = ~cityVal[[1]][["DATA"]], opacity = 0.7,
                        title = NULL, position = "bottomright"
    )
  })
  
  # Dominant flow map Display  ####
  output$mapfluDom <- renderLeaflet({
    
    lflStructure <- leaflet(options = leafletOptions(zoomControl = FALSE, minZoom = 8, maxZoom = 13, incl.data=TRUE)) %>%
      addMapPane("communes", zIndex = 410) %>%         # Level 1
      addMapPane("réseau_routier", zIndex = 420) %>%   # Level 2
      addMapPane("voie_ferré", zIndex = 430) %>%       # Level 3
      addMapPane("station", zIndex = 440) %>%          # Level 4
      addMapPane("flux", zIndex = 450) %>%             # Level 6
      addMapPane("cercles", zIndex = 460) %>%          # Level 7
      addMapPane("label", zIndex = 470) %>%            # Level 5
      addProviderTiles(provider = "CartoDB.PositronNoLabels",
                       options = providerTileOptions(opacity = 0.5)) %>%
      addProviderTiles(provider = "CartoDB.PositronOnlyLabels",
                       options = pathOptions(pane = "label", opacity = 0.5)) %>% 
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
    
    structure <- isolate(Get_Structure())
    lflStructure %>% 
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
      addPolylines(data = st_transform(structure$dataflu, crs = 4326), color = "#82909E", opacity = 0.2, weight = 1.5 ,
                   stroke = TRUE) %>%
      addCircles(lng = structure$cercle[["lon"]], 
                 lat = structure$cercle[["lat"]], 
                 color = colorNumeric(palette = c("#B35605","#F1A340","#828F9E"), domain = structure$cercle[["status"]])(structure$cercle[["status"]]),
                 radius = structure$rayon,
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
                   direction = "auto")
      )%>% 
      addLegendCustom(colors = c("#B35605","#F1A340","#97A7B8"), labels = c("Dominant", "Intermédiaire", "Dominé"), sizes = c(25, 20, 15))
    
    
  })
  
  observe({
    structure <- Get_Structure()
    shinyjs::showElement(id = 'loading-content')
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
      addCircleMarkers(lng = station$longitude, 
                       lat = station$latitude, 
                       radius = 2,                                         
                       stroke = F,                                           
                       color = "grey",
                       fillOpacity = 0.8,
                       group = "Stations ferroviaires",
                       options = pathOptions(pane = "station")) %>%
      addPolylines(data = st_transform(structure$dataflu, crs = 4326), color = "#82909E", opacity = 0.2, weight = 1.5 ,
                   stroke = TRUE)%>%
      addCircles(lng = structure$cercle[["lon"]], 
                 lat = structure$cercle[["lat"]], 
                 radius = structure$rayon, 
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
                   direction = "auto")
      )
    shinyjs::hideElement(id ='loading-content')
    shinyjs::hideElement(id ='loading')
  })
  
  observe({
    structure <- Get_Structure()
    proxy <- leafletProxy("mapfluDom", data =c(structure$dataflu,structure$rayon,structure$col))
    proxy %>% clearControls()
    proxy %>% addLegendCustom(colors = c("#B35605","#F1A340","#97A7B8"), labels = c("Dominant", "Intermédiaire", "Dominé"), sizes = c(25, 20, 15))
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
  
  GetTopLinks <- reactive({
    shinyjs::showElement(id = 'loading-content')
    shinyjs::showElement(id = 'loading')
    req(input$fluref, input$fluvar, input$flucom, input$fluthr)
    topLinks <- GetLinks(tabnav = Get_Filter_Flux(), ref = input$fluref, varsort = input$fluvar, oneunit = substring(input$flucom, 9), thres = input$fluthr)
    shinyjs::hideElement(id = 'loading-content')
    shinyjs::hideElement(id = 'loading')
    return(topLinks)
  })
  
  Get_CityValue <- reactive({
    shinyjs::showElement(id = 'loading-content')
    shinyjs::showElement(id = 'loading')
    req(input$fluref, input$flucom, input$fluvar)
    cityValue <- city_Value(tabflows = Get_Filter_Flux(), matDist = matDist, pol = pol,idpol = "insee", var = input$fluvar, od = input$fluref, city = substr(input$flucom, 1, 5))
    shinyjs::hideElement(id = 'loading-content')
    shinyjs::hideElement(id = 'loading')
    return(cityValue)
  })
  
  Get_Filter_Flux <- reactive({
    shinyjs::showElement(id = 'loading-content')
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
    return(filterFlux)
  })
  
  Get_Index <- reactive({
    shinyjs::showElement(id = 'loading-content')
    shinyjs::showElement(id = 'loading')
    req(input$radioMobi)
    index <- Index(mobi = input$radioMobi, commdata = Get_Filter_Index())
    shinyjs::hideElement(id = 'loading-content')
    shinyjs::hideElement(id = 'loading')
    return(index)
  })
  
  Get_Filter_Index <- reactive({
    req(input$FiltreIndices)
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
    return(filterIndice)
  })
  
  Get_Structure <- reactive({
    shinyjs::showElement(id = 'loading-content')
    shinyjs::showElement(id = 'loading')
    req(input$radioFlu) 
    structure <- Structure(Flu = input$radioFlu ,domFlowJob = Get_Filter_Structure()$domFlowJob ,domFlowPop = Get_Filter_Structure()$domFlowPop,domFlowJP = Get_Filter_Structure()$domFlowJP)
    shinyjs::hideElement(id = 'loading-content')
    shinyjs::hideElement(id = 'loading')
    return(structure)
  })
  
  Get_Filter_Structure <- reactive({
    req(input$FiltreStructure)
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
    return(filterStructure)
  })
})