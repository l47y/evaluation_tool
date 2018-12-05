
add_plotlayout <- function(p) {
  p %<>% layout(plot_bgcolor = "transparent", 
                paper_bgcolor = "transparent",
                font = list(size = 14, color = 'white'),
                xaxis = plot_axisstyle <- list(tickfont = list(color = 'white'),
                                               gridwidth = 0, showline = F, showgrid = F, zeroline = F),
                yaxis = plot_axisstyle <- list(tickfont = list(color = 'white'),
                                               gridwith = 0, showline = F, showgrid = F, zeroline = F),
                margin = list(l = 40, r = 40, b = 40, t = 40))
  return(p)
}

orderXfactors <- function(toOrder, orderBy, decr = T) {
  return(factor(toOrder, levels = unique(as.character(toOrder[order(orderBy, decreasing = decr)]))))
}

evalsInCorrectFormat <- function(str) {
  if (str == '') {
    return(F)
  }
  digits <- c('0', '1', '2', '3', '4', '5', '6', '7', '8', '9')
  validSymbols <- c(digits, ',')
  splitted = unlist(str_split(str, pattern = ''), use.names = F)
  if (!all(splitted %in% validSymbols)) {
    return(F)
  }
  where_commas <- which(splitted == ',')
  num_commas <- length(where_commas)
  if (any(where_commas[1] == 1, where_commas[num_commas] == length(splitted))) {
    return(F)
  }
  where_commas <- c(0, where_commas, length(splitted) + 1)
  for (i in 1:(length(where_commas) - 1)) {
    ind1 <- where_commas[i] + 1
    ind2 <- where_commas[i + 1] - 1
    x = splitted[ind1:ind2]
    num <- as.numeric(paste(splitted[ind1:ind2], collapse = ''))
    if (any(num < 0, num > 10)) {
      return(F)
    }
  }
  return(T)
}

getCityCoordinates <- function(city, CountryCode) {
  CityName <- gsub(' ','%20', city) 
  url <- paste(
    'http://nominatim.openstreetmap.org/search?city=',
    CityName,
    '&countrycodes=',
    CountryCode, 
    '&limit=9&format=json',
    sep = '')
  x <- fromJSON(url)
  if(is.vector(x)){
    lon <- x[[1]]$lon
    lat <- x[[1]]$lat    
  } else {
    lon <- 0
    lat <- 0
  }
  return(list('lon' = lon, 'lat' = lat))
}

convert_coords <- function(coord, LonOrLat = 'Lon') {
  splitted <- unlist(strsplit(coord, split = ''), use.names = F)
  where_points <- which(splitted == '.')
  splitted <- splitted[-where_points]
  position <- case_when(all(LonOrLat == 'Lon', splitted[1] == '-') ~ 2,
                        all(LonOrLat == 'Lon', splitted[1] != '-') ~ 1,
                        LonOrLat == 'Lat' ~ 2)
  splitted <- unlist(append(as.list(splitted), '.', position), use.names = F)
  return(as.numeric(str_flatten(splitted)))
}

