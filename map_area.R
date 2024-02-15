# read and map survey area shape file 
library(sf)
temp <- tempfile()
unzip(zipfile = "Izembek_shapefile.zip", exdir = temp)
lag <- read_sf(dsn=temp)
lag
plot(st_geometry(lag))
st_layers(dsn=temp)
library(tmap)
tmap_mode(mode = c("view"))
tm_shape(lag) + tm_polygons()
# Error: Shape contains invalid polygons. Please fix it or set tmap_options(check.and.fix = TRUE) and rerun the plot
tmap_options(check.and.fix = TRUE)
tm_shape(lag) + tm_polygons(col = "LC_class_d", alpha = 0.5) + 
  tm_basemap(server = "Esri.WorldImagery")
#try transforming and mapping
lag2 <- st_transform(lag, crs = 4326)
tm_shape(lag2) + tm_polygons(col = "LC_class_d", alpha = 0.5) + 
  tm_basemap(server = "Esri.WorldImagery")
#look at invalid polygons
lag2 <- st_cast(lag, "POLYGON")
lagVal <- st_is_valid(lag2)
notVal <- which(lagVal==FALSE)
plot(st_geometry(lag2[notVal[1],]), col = 2)
st_area(lag2[notVal[1],])
st_area( st_make_valid(lag2[notVal[1],]) )
plot( st_geometry( st_make_valid(lag2[notVal[1],]) ) , col=2)
st_coordinates(lag2[notVal[1],]) 
st_coordinates(st_make_valid(lag2[notVal[1],]))

st_area(st_union(lag))

#plot simplified lagoon
lag3 <- st_boundary(st_union(lag))
plot(st_geometry(lag3))
lag3 <- st_cast(lag3, "POLYGON")
tm_shape(lag3) + tm_polygons(col = "red", alpha = 0.5) + 
  tm_basemap(server = "Esri.WorldImagery")
#add buffer
lag4 <- st_buffer(lag3, dist = 500)
tm_shape(lag3) + tm_polygons(col = "red", alpha = 0.5) + 
  tm_shape(lag4) + tm_polygons(col = "blue", alpha = 0.5) + 
  tm_basemap(server = "Esri.WorldImagery")

#take lag3 and make a convex hull out of it
lag5 <- st_convex_hull(lag3)
lag6 <- st_concave_hull(lag3, ratio = 0.5, allow_holes = FALSE) #needs R 4.3 and GOES 3.11
tm_shape(lag3) + tm_polygons(col = "red", alpha = 0.5) + 
  tm_shape(lag4) + tm_polygons(col = "blue", alpha = 0.5) + 
  tm_shape(lag5) + tm_polygons(col = "orange", alpha = 0.5) + 
  tm_shape(lag6) + tm_polygons(col = "green", alpha = 0.5) + 
  tm_basemap(server = "Esri.WorldImagery")
#should due hull over buffered area 
lag7 <- st_concave_hull(lag4, ratio = 0.3, allow_holes = FALSE)
tm_shape(lag3) + tm_polygons(col = "red", alpha = 0.5) + 
  tm_shape(lag4) + tm_polygons(col = "blue", alpha = 0.5) + 
  tm_shape(lag5) + tm_polygons(col = "orange", alpha = 0.5) + 
  tm_shape(lag7) + tm_polygons(col = "green", alpha = 0.5) + 
  tm_basemap(server = "Esri.WorldImagery")

#Source from Alaska coastline data layer at 
# https://gis.data.alaska.gov/datasets/SOA-DNR::alaska-coastline/api?layer=2

# test_url <- httr::parse_url(
#   #"https://arcgis.dnr.alaska.gov/arcgis/rest/services/OpenData/Physical_AlaskaCoast/MapServer/2/query?where=1%3D1&outFields=*&geometry=-174.155%2C59.023%2C-155.303%2C62.760&geometryType=esriGeometryEnvelope&inSR=4326&spatialRel=esriSpatialRelContains&outSR=4326&f=json"
#   "https://arcgis.dnr.alaska.gov/arcgis/rest/services/OpenData/Physical_AlaskaCoast/MapServer/2/query?where=1%3D1&outFields=*&geometry=-163.732%2C54.993%2C-161.375%2C55.541&geometryType=esriGeometryEnvelope&inSR=4326&spatialRel=esriSpatialRelContains&outSR=4326&f=json"
# )
# request <- httr::build_url(test_url)
# 
# ak_coastline <- st_read(request) # Don’t need to set layer, is set in url above as “2”
# plot(st_geometry(ak_coastline))
# tm_shape(lag3) + tm_polygons(col = "red", alpha = 0.5) + 
#   tm_shape(ak_coastline) + tm_polygons(col = "blue", alpha = 0.5) + 
#   tm_basemap(server = "Esri.WorldImagery")
# #that doesn't look so good!

#Map photos from 2019
library(tidyverse)
dat1 <- read_csv(file = "goose_fallSurveyCounts_izembek_weiser/goose_fallSurveyCounts_izembek_weiser/2019/Combined_counts_2019-10-07_Cam3.csv") %>%
  mutate(Replicate = "1")
dat2 <- read_csv(file = "goose_fallSurveyCounts_izembek_weiser/goose_fallSurveyCounts_izembek_weiser/2019/Combined_counts_2019-10-09_Cam3.csv") %>%
  mutate(Replicate = "2")
dat3 <- read_csv(file = "goose_fallSurveyCounts_izembek_weiser/goose_fallSurveyCounts_izembek_weiser/2019/Combined_counts_2019-10-11_Cam3.csv") %>%
  mutate(Replicate = "3")
dat <- rbind(dat1, dat2, dat3) %>%
  filter(!is.na(Latitude)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)
tm_shape(lag2) + tm_polygons(col = "LC_class_d", alpha = 0.5) + #tm_polygons(col = "red", alpha = 0.5) + 
  tm_shape(dat) + tm_dots(col = "Replicate") +
  tm_basemap(server = "Esri.WorldImagery")
#compare to Ward:
unzip(zipfile = "eelgrass_mapping_AKPenNunivak_ward.zip", exdir = temp)
ward <- read_sf(dsn=temp) %>%
  st_union() %>%
  st_boundary() %>%
  st_cast("POLYGON")
tm_shape(ward) + tm_polygons(col = "red", alpha = 0.5) + 
  tm_shape(dat) + tm_dots() +
  tm_basemap(server = "Esri.WorldImagery")
#looks the same
#query and plot some photos
# #use imager
# library(imager)
# #choose photo
# image_filename <- "CAM37613.jpeg"
# image <- load.image(image_filename)
# plot(image)
#dang, don't have access to 17-19 photos.
################################################################################
#map the 2017 photos


################################################################################
#lets map the 2022 photos
files <- list.files(path="2022_geotagged_photo_data", full.names = TRUE)
dat <- map_dfr(files, read_csv, .id = "File") %>%
  filter(!is.na(Latitude)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)
dat <- dat %>%
  mutate(Replicate = if_else(File %in% 1:2, 1, 
                             if_else(File %in% 3:4, 2,
                                     if_else(File %in% 5:6, 3, 
                                             if_else(File %in% 7:8, 4, 
                                                     if_else(File %in% 9:10, 5, NA)))))) %>%
  mutate(Replicate = as.factor(Replicate))
#just load one rep as 53K dots is too slow
df <- filter(dat, Replicate == 1)
tm_shape(lag2) + tm_polygons(col = "LC_class_d", alpha = 0.5) + #tm_polygons(col = "red", alpha = 0.5) + 
  tm_shape(df) + tm_dots(col = "Replicate", 
                          popup.vars=c("Image"="FileName", "Date"="DateTimeOriginal", 
                                       "Replicate"="Replicate")) +
  tm_basemap(server = "Esri.WorldImagery")

library(imager)
#choose photo
#hard path to external drive
#need to select the directory based on
image_filename <- "D:/Iz22photo_10-09-22/CAM4/CAM42256.JPG"
#humm, photos are already filter so that only the ones inside the survey area are in the directory
image <- load.image(image_filename)
plot(image)
