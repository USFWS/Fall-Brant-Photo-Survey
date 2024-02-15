library(httr)
library(sf)
library(tmap)
library(tidyverse)

test_url <-parse_url(
  "https://arcgis.dnr.alaska.gov/arcgis/services/OpenData/Physical_AlaskaCoast/MapServer/WFSServer?request=GetCapabilities&service=WFS")

request <-build_url(test_url)

#There are several layers hosted under this Map server, this lists them
st_layers(request)

#Choosing the simplified coastline to read
ak_coastline <- st_read(request, layer="esri:Alaska_Simplified_Coast")

#Plot
ggplot(ak_coastline)+
  geom_sf()

#Some issue with multisurfaces, code for interactive map doesn't work
# tmap_mode(mode="view")
# tm_shape(ak_coastline)+tm_polygons()


