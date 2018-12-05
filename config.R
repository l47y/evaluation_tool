library(shinydashboard)
library(dashboardthemes)
library(shiny)
library(plotly)
library(DT)
library(shiny)
library(tidyverse)
library(magrittr)
library(RJSONIO)
library(googlesheets)

source('Helper.R')

CountryOfCursos <- 'ES'
mouseHoverStr <- 'Se puede poner cualquier cosa en esta casilla pero no se puede dejarla vacía.'
noDataStr <- 'No hay datos.'
howToInputEvals <- paste0('Inserte las evaluaciones del siguiente formato: ',
                          'Num,Num,Num,Num donde Num es un número entero entre 0 y 10. ',
                          'Ejemplo: 10,10,9,8,7,8')
myColors <- c('seagreen2', 
              'bisque1')