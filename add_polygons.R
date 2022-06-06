library(phyloregion)
library(sf)
data(africa)
phylo <- africa$phylo
space <- africa$polys
space_reproj <- 
  space %>% 
  st_as_sf() %>% 
  st_transform(crs = "+init=epsg:4326")

leaflet() %>%
  addTiles() %>% 
  addPolygons(data = space_reproj, weight = 2, fillColor = "green", popup = TRUE)

space
plot(space_leaflet)


basemap <- leaflet() %>%
  # add different provider tiles
  addTiles() %>%
  # add a layers control
  addLayersControl(
    baseGroups = c(
      "OpenStreetMap", "Stamen.Toner",
      "Stamen.Terrain", "Esri.WorldStreetMap",
      "Wikimedia", "CartoDB.Positron", "Esri.WorldImagery"
    ),
    # position it on the topleft
    position = "topleft"
  )


basemap %>% 
  addPolygons(data = space_leaflet)
