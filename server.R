source('config.R')

shinyServer(function(input, output, session) {
  
  globaldata <- reactiveVal(NULL) # working data table during session
  googleSessionTable <- reactiveVal(NULL) # session object of google sheets
  
  ############################################### ~ REACTIVE STUFF –
  ###############################################
  
  getCursoDate <- reactive({
    date <- globaldata() %>% filter(Curso == input$whichcursoeditar) %>% pull(Fecha)
    date[1]
  })
  
  ############################################### ~ OBSERVE STUFF –
  ###############################################
  
  observeEvent(input$connecttogogle, {
    gs_auth(new_user = TRUE)
    tables <- gs_ls()$sheet_title
    if (!'Mis_Cursos' %in% tables) {
      showNotification('No se han podido encontrar datos de tus cursos.', duration = 4, 
                       type = 'error')
      sheet <- NULL
    } else {
      sheet <- gs_title('Mis_Cursos') %>% gs_gs() 
      table <- gs_read(sheet, 1)
      globaldata(table)
    }
    googleSessionTable(sheet)
  })
  
  observeEvent(input$guardar, {
    showNotification('Guardando datos .... Espere un momento por favor.')
    if (is.null(globaldata()) == F) {
      if (is.null(googleSessionTable()) == F) {
        gs_delete(googleSessionTable())
      }
      gs_new('Mis_Cursos', ws_title = 'Cursos', input = globaldata(), 
             trim = T, verbose = F)
      showNotification('Los datos se han guardado con éxito.')
    } else {
      showNotification('No hay datos que guardar.')
    }
  })
  
  observeEvent(input$editarcurso, {
    
    tmp <- globaldata() 
    selected <- input$whichcursoeditar
    parttmp <- tmp[tmp$Curso == selected, ]
    
    nombre <- parttmp$Curso[1]
    lugar <- parttmp$Lugar[1]
    empresa <- parttmp$Empresa[1]
    fecha <- input$editarfechadecurso
    evals <- parttmp$Evaluaciones
    lon <- parttmp$Lon[1]
    lat <- parttmp$Lat[1]
    
    nombreTyped <- input$editarnombredecurso != ''
    empresaTyped <- input$editarempresadelcurso != ''
    lugarTyped <- input$editarlugardecurso != ''
    evalsTyped <- input$editarevaluaciones != ''
    fechaChanged <- input$editarfechadecurso != getCursoDate()
    
    if (all(!nombreTyped, !empresaTyped, !lugarTyped, !evalsTyped, !fechaChanged)) {
      showNotification('No se ha hecho ningún cambio.', type = 'warning', duration = 4)
    } else if (all(evalsTyped, evalsInCorrectFormat(input$editarevaluaciones) == F)) {
      showNotification('Las evaluaciones tienen que estar en el formato correcto. 
                        Mueva el ratón sobre la casilla para ver cómo se hace.')
    } else {
      if (input$editarnombredecurso != '') {
        nombre <- input$editarnombredecurso
      }
      if (input$editarempresadelcurso != '') {
        empresa <- input$editarempresadelcurso
      }
      if (input$editarlugardecurso != '') {
        lugar <- input$editarlugardecurso
        coords <- getCityCoordinates(input$editarlugardecurso, CountryOfCursos)
        lon <- coords$lon
        lat <- coords$lat
        if (any(lon == 0, lat == 0)) {
          showNotification('No se ha podido encontrar el lugar en el mapa.', duration = 4, 
                           type = 'warning')
        }
      }
      if (input$editarevaluaciones != '') {
        evals <- as.numeric(unlist(strsplit(input$editarevaluaciones, ","), use.names = F))
      }
      newtmp <- data.frame(
        'Curso' = rep(nombre, length(evals)),
        'Lugar' = rep(lugar, length(evals)),
        'Empresa' = rep(empresa, length(evals)),
        'Fecha' = rep(fecha, length(evals)),
        'Evaluaciones' = evals,
        'Lon' = rep(lon, length(evals)),
        'Lat' = rep(lat, length(evals))
      )
      tmp %<>% filter(Curso != selected) %>% rbind(newtmp) %>% arrange(Fecha)
      globaldata(tmp)
      updateTextInput(session, 'editarnombredecurso', value='')
      updateTextInput(session, 'editarlugardecurso', value='')
      updateTextInput(session, 'editarempresadelcurso', value='')
      updateTextInput(session, 'editarevaluaciones', value='')
      showNotification('Los cambios se han realizado con éxito.', type = 'message', duration = 4)
    }
  })
  
  observeEvent(input$borrarcurso, {
    tmp <- globaldata() %>% filter(Curso != input$whichcursoborrar)
    if (nrow(tmp) == 0) {
      globaldata(NULL) 
    } else {
      globaldata(tmp)
    }
    showNotification('El curso se ha borrado con éxito.', type = 'message', duration = 3)
  })
  
  observeEvent(input$anadircurso, {
    
    curso = input$nombredecurso
    lugar = input$lugardecurso
    empresa = input$empresadelcurso
    evalsCorrect <- evalsInCorrectFormat(input$evaluaciones)
    
    if (any(curso == '', lugar == '', empresa == '', !evalsCorrect)) {
      didntWork <- T
      showNotification('Tiene que llenar todo de forma correcta antes de que se pueda añadir un curso.
                       Mueva el ratón sobre las casillas para ver cómo se hace.',
                       duration = 5, type = 'error')
    } else {
      evals <- as.numeric(unlist(strsplit(input$evaluaciones, ","), use.names = F))
      coords <- getCityCoordinates(input$lugardecurso, CountryOfCursos)
      tmp <- data.frame(
        'Curso' = rep(input$nombredecurso, length(evals)),
        'Lugar' = rep(input$lugardecurso, length(evals)),
        'Empresa' = rep(input$empresadelcurso, length(evals)),
        'Fecha' = rep(input$fechadecurso, length(evals)),
        'Evaluaciones' = evals,
        'Lon' = rep(coords$lon, length(evals)),
        'Lat' = rep(coords$lat, length(evals))
      )
      
      if (is.null(globaldata) == T) {
        globaldata(tmp)
      } else {
        tmp <- rbind(globaldata(), tmp)
        globaldata(tmp)
      }
      updateTextInput(session, 'nombredecurso', value='')
      updateTextInput(session, 'lugardecurso', value='')
      updateTextInput(session, 'empresadelcurso', value='')
      updateTextInput(session, 'evaluaciones', value='')
      
      showNotification('El curso se ha añadido con éxito.', type = 'message', duration = 4)
    }
  })
  
  ############################################### ~ OUTPUT SECTION ~
  ###############################################
  
  ############################################### Tab: Cursos
  
  # # # # Infoboxes
  
  output$mediatotal <- renderInfoBox({
    totalMean <- round(mean(globaldata()$Evaluaciones), digits = 3)
    if (is.na(totalMean) == T) {
      totalMean <- noDataStr
    }
    infoBox(title = 'Media total', value = totalMean, color = 'purple')
  })
  
  output$numeroevals <- renderInfoBox({
    numberEvals <- nrow(globaldata())
    if (is.null(numberEvals) == T) {
      numberEvals <- noDataStr
    }
    title <- HTML('Número<br>Evaluaciones')
    infoBox(title = title, value = numberEvals, color = 'maroon')
  })
  
  output$numerocursos <- renderInfoBox({
    infoBox(title = 'Número Cursos', value = length(unique(globaldata()$Curso)),
            color = 'green')
  })
  
  # # # # Box: Cargar/Guardar
  
  # # # # Box: Editar
  
  output$whichcursoeditar <- renderUI({
    validate(need(is.null(globaldata()) == F, noDataStr))
    selectInput('whichcursoeditar', 'Elige curso para editar', choices = unique(globaldata()$Curso))
  })
  
  output$editarfechadecurso <- renderUI({
    if (is.null(globaldata()) == F) {
      dateInput('editarfechadecurso', label = 'Fecha', value = getCursoDate())
    }
  })
  
  output$editarnombredecurso <- renderUI({
    if (is.null(globaldata()) == F) {
      textInput('editarnombredecurso', label = 'Nombre')
    }
  })
  
  output$editarlugardecurso <- renderUI({
    if (is.null(globaldata()) == F) {
      textInput('editarlugardecurso', label = 'Lugar')
    }
  })
  
  output$editarempresadelcurso <- renderUI({
    if (is.null(globaldata()) == F) {
      textInput('editarempresadelcurso', label = 'Empresa')
    }
  })
  
  output$editarevaluaciones <- renderUI({
    if (is.null(globaldata()) == F) {
      textInput('editarevaluaciones', 'Evaluaciones')
    }
  })
  
  output$editarcurso <- renderUI({
    if (is.null(globaldata()) == F) {
      actionButton('editarcurso', 'Editar')
    }
  })
  
  # # # # Box: Borra 
  
  output$whichcursoborrar <- renderUI({
    selectInput('whichcursoborrar', 'Elige curso para borrar', choices = unique(globaldata()$Curso))
  })
  
  ############################################### Tab: Estadisticas
  
  output$evaldist <- renderPlotly({
    validate(need(is.null(globaldata()) == F, noDataStr))
    evals <- globaldata() %>% group_by(Evaluaciones) %>% summarise(n = n())
    evals$info <- paste('Número: ', evals$n)
    plot_ly(x = evals$Evaluaciones, y = evals$n, type = 'bar', 
            marker = list(color=toRGB('seagreen2')), text = evals$info, hoverinfo = 'text') %>%
      add_plotlayout() %>% 
      layout(title = 'Distribuación de las Evaluaciones')
  })
  
  output$topcursos <- renderPlotly({
    topX <- 5
    validate(need(is.null(globaldata()) == F, noDataStr))
    tmp <- globaldata() %>% group_by(Curso) %>% 
      summarize(mean = round(mean(Evaluaciones), digits = 3)) %>%
      arrange(desc(mean))
    tmp <- tmp[1:min(nrow(tmp), topX), ]
    tmp$info <- paste('Media: ', tmp$mean)
    plot_ly(x = orderXfactors(tmp$Curso, tmp$mean), y = tmp$mean, type = 'bar', 
            marker = list(color=toRGB('coral2')), text = tmp$info, hoverinfo = 'text') %>% 
      add_plotlayout() %>% 
      layout(title = paste0('Top ', nrow(tmp), ' Cursos')) %>% 
      layout(yaxis = list(range = c(min(tmp$mean) - 1, 10)))
  })
  
  output$evolucion <- renderPlotly({
    points <- globaldata() %>% group_by(Fecha, Evaluaciones) %>% summarise(n = n())
    points$info <- paste('Fecha: ', points$Fecha, '<br>Evaluación: ', points$Evaluaciones, 
                         '<br>Número: ', points$n)
    media <- globaldata() %>% group_by(Fecha) %>% 
      summarise(Media = round(mean(Evaluaciones), digits = 3))
    media$info <- paste('Media: ', media$Media)
    p <- plot_ly(x = media$Fecha, y=media$Media, type = 'scatter', mode='lines', 
                 name = 'Media', marker = list(color = toRGB('red')), color='pink',
                 text = media$info, hoverinfo = 'text') %>%
      add_markers(x = points$Fecha, y=points$Evaluaciones, size = points$n, 
                  mode='markers', name = 'Evaluaciones', text = points$info, hoverinfo = 'text',
                  marker = list(sizeref=0.1, sizemode = 'area', color = toRGB('maroon'), 
                                line = list(color = toRGB('cyan'), width = 2),
                                opacity = 1)) %>% 
      add_plotlayout() %>%
      layout(title = 'Evolución de las Evaluaciones')
    p
  })
  
  ############################################### Tab: Mapa
  
  output$mapa <- renderPlotly({
    validate(need(is.null(globaldata()) == F, noDataStr))
    g <- list(
      scope = 'europe',
      lonaxis = list(range = c(-10, 6)),
      lataxis = list(range = c(35, 45)),
      showland = T,
      landcolor = toRGB("gray"),
      subunitcolor = toRGB("darkslategrey"),
      countrycolor = toRGB("darkslategrey"),
      countrywidth = 1.2,
      subunitwidth = 0.5,
      resolution = 400
    )
    
    tmp <- globaldata()
    tmp$Lat <- unlist(lapply(tmp$Lat, convert_coords, LonOrLat = 'Lat'), use.names = F)
    tmp$Lon <- unlist(lapply(tmp$Lon, convert_coords, LonOrLat = 'Lon'), use.names = F)
    Cursos <- tmp %>% group_by(Lugar) %>% 
      summarize(NumCursos = length(unique(Curso)),
                Media = round(mean(Evaluaciones), digits = 3))
    coords <- tmp[!duplicated(tmp[, 'Lugar']), ] %>% select(Lugar, Lon, Lat)
    df <- left_join(coords, Cursos, by = 'Lugar')
    df$info <- paste('Ciudad: ', df$Lugar, '<br>Numero cursos: ', df$NumCursos, '<br>Media: ', df$Media)
    print(df)
    p <- plot_geo(df, lat = ~Lat, lon = ~Lon, size = ~NumCursos, mode='markers',
             color = ~Media, text = df$info, hoverinfo = 'text', 
             marker = list(sizeref = 0.2, sizemode='area')) %>%
      layout(geo = g, margin = list(b=0, t=40, l=0, r=0),
             title = 'Mapa de Cursos',
             plot_bgcolor = "transparent", 
             paper_bgcolor = "transparent",
             font = list(size = 14, color = 'white'),
             legend = list(x = 0.1, y = 0.5))
    p
  })
  
  ############################################### Tab: Ver data
  
  output$viewdata <- DT::renderDataTable({
    validate(need(is.null(globaldata()) == F, noDataStr))
    if(input$alldataoronlycursos == 'Resúmen de Cursos') {
      tmp <- globaldata() %>% select(Curso, Lugar, Empresa, Fecha)
      tmp <- tmp[!duplicated(tmp), ]
    } else {
      tmp <- globaldata() %>% select(Curso, Lugar, Empresa, Fecha, Evaluaciones)
    }
    datatable(tmp, options = list(
      autowidth = F, scrollY = T, searching = T, searchHighlight = T, pageLength = 20
    ))
  })

  
})
