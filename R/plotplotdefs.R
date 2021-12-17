src <- "https://raw.githubusercontent.com/OSGeo/PROJ/master/docs/plot"

LINE_LOW = 'data/coastline.geojson'
LINE_MED = 'data/coastline50.geojson'
POLY_LOW = 'data/land.geojson'
POLY_MED = 'data/land50.geojson'

linelow <- sf::read_sf(file.path(src, LINE_LOW))
linemed <- sf::read_sf(file.path(src, LINE_MED))
polylow <- sf::read_sf(file.path(src, POLY_LOW))
polymed <- sf::read_sf(file.path(src, POLY_MED))


GRATICULE_WIDTH = 15
N_POINTS = 1000

# colors
COLOR_LAND = '#000000'
COLOR_COAST = '#000000'
COLOR_GRAT = '#888888'

defs <- jsonlite::read_json("https://raw.githubusercontent.com/OSGeo/PROJ/master/docs/plot/plotdefs.json")

plot_def <- function(x) {
  map <- switch(toupper(sprintf("%s_%s", x$type, x$res)),
                    POLY_LOW = polylow,
                    POLY_MED = polymed,
                    LINE_LOW = linelow,
                    LINE_MED = linemed)
  map <- sf::st_geometry(map)
  if (x$latmax > 89.999) {
    map <-  c(map, sf::st_sfc(sf::st_point(cbind(0, 90)), crs = sf::st_crs(map)))
    COLOR_LAND <- c(rep(COLOR_LAND, length(map) - 1), NA)
    COLOR_COAST <- c(rep(COLOR_LAND, length(map) - 1), NA)

  }
  ## bug in ccon def
  if (x$lonmin > x$lonmax) {
    lmin <- x$lonmin
    x$lonmin <- x$lonmax
    x$lonmax <- lmin
  }
  if (x$lonmin > -180 || x$lonmax < 180 || x$latmin > -90 || x$latmax < 90 ) {
    map <- sf::st_crop(map, sf::st_bbox(c(xmin = x$lonmin, xmax = x$lonmax, ymin = x$latmin, ymax = x$latmax)))
  } else {
    map <- sf::st_crop(map, sf::st_bbox(c(xmin = -179.9, xmax = 179.9, ymin = -89.9, ymax = 89.9)))

  }
map <- sf::st_segmentize(map, 2e4)
pmap <-  sf::st_transform(map, x$projstring)
#  grat <- sf::st_graticule(pmap)
  grat <- sf::st_graticule(pmap, lon = seq(x$lonmin, x$lonmax, by = GRATICULE_WIDTH),
                           lat = seq(x$latmin, x$latmax, by = GRATICULE_WIDTH))

  plot(grat, col = NA)
  if (x$type == "poly")  plot(pmap, col = COLOR_LAND)
  if (x$type == "line")  plot(pmap, col = COLOR_COAST)
  plot(grat, add = TRUE, col = COLOR_GRAT)
  title(sub = x$projstring)
}

for (i in seq_along(defs)[-c(1:18)]) {
  plot_def(defs[[i]])
  scan("", 1)
}
