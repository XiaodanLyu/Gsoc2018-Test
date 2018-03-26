library(rgbif)
library(tidyverse)

## Easy part ####
## download data ####
key <- 4299800 ## Butterfly - Eyebrown
## 1627 occurrences in total according to GBIF
dget <- occ_data(taxonKey = key, limit = 1627)
# dget$data %>% glimpse
draw <- dget$data %>% mutate(lng = decimalLongitude, lat = decimalLatitude)

## cleaning data ####
apply(draw %>% select(lng, lat), 2, function(x) sum(is.na(x))) # 104 missing coordinates
## plot with only occurrences with complete coordinates, 1523 records left
draw2 <- draw %>% filter(!is.na(lng), !is.na(lat))
table(draw2$country) ## only in Canada and US
## exlude misspecified locations since (0,0) is neither in US nor Canada, 1479 records left
d <- draw2 %>% filter(!(lng == 0 & lat == 0))

## make a map ####
## using function in rgbif
gbifmap(input = d)
gbifmap(input = d, region = c("USA", "Canada"))
## or ggplot from scratch
extent <- d %>% select(x = lng, y = lat) %>% raster::extent()
base_layer <- ggplot(data = d, aes(x = lng, y = lat)) + 
  geom_point(size = 2, color = "red") +
  coord_equal() + ggthemes::theme_map()
map.ggplot <- base_layer +
  geom_path(data = map_data("world"), aes(x = long, y = lat, group = group)) +
  scale_x_continuous(limits = c(extent@xmin, extent@xmax)) +
  scale_y_continuous(limits = c(extent@ymin, extent@ymax))
map.ggplot
## Medium part ####
## convex hull polygon
hpts <- chull(d$lng, d$lat)
chull_layer <- geom_polygon(aes(x = lng, y = lat), data = d[hpts,],
                            fill = NA, color = "blue", size = 1.25)
base_layer + chull_layer
map.ggplot + chull_layer
chull.poly <- Polygon(d[hpts, c("lng", "lat")]) %>% list %>% Polygons(ID=1) %>%
  list %>% SpatialPolygons()
proj4string(chull.poly) <- CRS("+init=epsg:4326")

## Hard part ####
library(rworldmap)
world_map <- getMap(resolution = "high") %>% subset(continent = "North America")
world_map <- sp::spTransform(world_map, proj4string(chull.poly))
library(raster)
onland.poly <- intersect(world_map, chull.poly)
lps <- getSpPPolygonsLabptSlots(onland.poly)
IDOneBin <- cut(lps[,1], range(lps[,1]), include.lowest = TRUE)
onland <- maptools::unionSpatialPolygons(onland.poly, IDOneBin)
onland_layer <- geom_polygon(aes(x = long, y = lat, group = piece), data = fortify(onland),
                             fill = NA, color = "blue", size = 1.25)
map.ggplot + onland_layer

## method 2: using leaflet
library(leaflet)
## without cluster options
map.leaflet <- leaflet(data = d) %>% addTiles() %>%
  addCircleMarkers(lng = ~lng, lat = ~lat, color = "red", radius = 2,
                   label = ~sprintf("lng = %.2f, lat = %.2f", lng, lat))
map.leaflet
## with cluster options
leaflet(data = d) %>% addTiles() %>%
  addCircleMarkers(lng = ~lng, lat = ~lat, color = "red", radius = 2,
                   clusterOptions = markerClusterOptions(),
                   label = ~sprintf("lng = %.2f, lat = %.2f", lng, lat))

map.leaflet %>%
  addPolygons(group = "convex hull", data = chull.poly, fill = FALSE,
              stroke = TRUE, color = "black", weight = 2) %>%
  addPolygons(group = "on land", data = onland, fill = FALSE,
              stroke = TRUE, color = "blue", weight = 2) %>%
  addLayersControl(overlayGroups = c("convex hull", "on land"),
                   options = layersControlOptions(collapsed = FALSE)) %>%
  hideGroup("convex hull")

## moreover ####
## misspecified locations
wrongpts <- d %>% filter(lng == 0, lat == 0) %>% select(locality)
## results returned from ggmap geocodes shows the potential coordinates of those records
trypts <- sapply(wrongpts, ggmap::geocode)
data.frame(locality = wrongpts, lng = trypts[[1]], lat = trypts[[2]])
## locations with no returning coordinates could be found out by carefully renaming the locations
