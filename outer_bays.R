library(tidyverse)
library(sf)
library(tmap)

#get lagoon polygon, need to fix
temp <- tempfile()
unzip(zipfile = "IZF_OcularSegs.zip", exdir = temp)
lag <- read_sf(dsn=temp) %>%
  st_transform(crs=4326) %>%
  filter(ID %in%c(8, 9, 11, 14, 19)) #select just the segments of the outer bays
  # st_union() %>%
  # st_boundary() %>%
  # st_cast("POLYGON")
tmap_mode("view")  
tm_shape(lag) + tm_polygons(col = "red", alpha = 0.5) + 
  tm_basemap(server = "Esri.WorldImagery")
#seem to tight to shoreline and too complex
#we will buffer and simpify
#combine bays into three
df <- lag %>% filter(ID %in%c(8,9,11)) %>%
  st_union()
df <- st_sf(cbind(data.frame(ID = 1, Unit = NA),df)) %>%
  rbind(filter(lag, ID%in%c(14, 19)))
plot(st_geometry(df))
#add buffer in meters
df <- st_transform(df, crs = 3338) %>% st_buffer(dist = 200)
tm_shape(df) + tm_polygons(col = "red", alpha = 0.5) + 
  tm_basemap(server = "Esri.WorldImagery")
#looks OK, remove holes
df2 <-  st_multipolygon(lapply(st_geometry(df), function(x) x[1])) %>%
  st_sfc(crs=3338) %>%
  st_cast("POLYGON") %>%
  cbind(data.frame(Bay = c("A", "B", "C"))) %>% 
  st_sf()
tm_shape(df2) + tm_polygons(col = "red", alpha = 0.5) + 
  tm_basemap(server = "Esri.WorldImagery")
#
