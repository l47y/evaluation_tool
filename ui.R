source('config.R')

ui <- dashboardPage(
  
  dashboardHeader(title='Mis Cursos'),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem('Cursos', tabName = 'cursos'),
      menuItem('Estadísticas', tabName = 'estadisticas'),
      menuItem('Mapa', tabName = 'mapa'),
      menuItem('Ver Data', tabName = 'verdata')

    )
  ),
  
  dashboardBody(
    shinyDashboardThemes(
      theme = 'purple_gradient'
    ),
    
    tabItems(
      
      tabItem(tabName = 'cursos',
        fluidPage(
          column(12,
            infoBoxOutput('numerocursos', width = 3),
            infoBoxOutput('mediatotal', width = 3),
            infoBoxOutput('numeroevals', width = 3)
          ),
          box(title = 'Cargar/Guardar datos', solidHeader = T, collapsible = T, width = 3,
              fileInput('datafile', 'Eligir datos', multiple = FALSE,
                        accept = c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
              downloadButton('guardarydescargar', 'Guardar y Descargar')
          ),
          box(title = 'Añadir curso', collapsible = T, solidHeader = T, width = 3,
            tags$div(title = mouseHoverStr,
              textInput('nombredecurso', label = 'Nombre')
            ),
            tags$div(title = mouseHoverStr,
              textInput('lugardecurso', label = 'Lugar')
            ),
            tags$div(title = mouseHoverStr,
              textInput('empresadelcurso', label = 'Empresa')
            ),
            dateInput('fechadecurso', 'Fecha'),
            tags$div(title = howToInputEvals,
                     textInput('evaluaciones', 'Evaluaciones')
            ),
            actionButton('anadircurso', 'Añadir')
          ),
          box(title = 'Editar curso', collapsible = T, solidHeader = T, width = 3,
            uiOutput('whichcursoeditar'),
            tags$div(title = mouseHoverStr,
              uiOutput('editarnombredecurso')
            ),
            tags$div(title = mouseHoverStr,
              uiOutput('editarlugardecurso')
            ),
            tags$div(title = mouseHoverStr,
              uiOutput('editarempresadelcurso')
            ),
            uiOutput('editarfechadecurso'),
            tags$div(title = howToInputEvals,
                    uiOutput('editarevaluaciones')
            ),
            uiOutput('editarcurso')
          ),
           box(title = 'Borrar curso', collapsible = T, solidHeader = T, width = 3,
              uiOutput('whichcursoborrar'),
              actionButton('borrarcurso', 'Borrar')
          )
        )
      ), 
      
      tabItem(tabName = 'estadisticas',
        fluidPage(
          box(title = '', solidHeader = T, collapsible = T, width = 5, 
            plotlyOutput('evaldist')   
          ), 
          box(title = '', solidHeader = T, collapsible = T, width = 5, 
            plotlyOutput('topcursos')
          ),
          box(title = '', solidHeader = T, collapsible = T, width = 10,
            plotlyOutput('evolucion')   
          )
        )
      ),
      
      tabItem(tabName = 'mapa',
         fluidPage(
           plotlyOutput('mapa', width = 800, height = 600)
         )     
      ),
      
      tabItem(tabName = 'verdata',
              DT::dataTableOutput('viewdata')
      )
    )
  )
)