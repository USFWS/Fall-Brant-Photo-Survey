library(sf)
library(dplyr)
library(dsims)
################################################################################
#functions
#try to make point along transect
trans2points <- function(x=NA, dist = 1) {
  length = st_length(x)
  dens = 1/dist
  num <- length*dens
  st_line_sample(x, num, dens, type = "regular")
}
#function to extend lines from their end in the direction they run
# from https://stackoverflow.com/questions/65928126/increase-polyline-length-r-sf
# First you need a function to find in which direction each end is pointing:
st_ends_heading <- function(line)
{
  M <- sf::st_coordinates(line)
  i <- c(2, nrow(M) - 1)
  j <- c(1, -1)
  
  headings <- mapply(i, j, FUN = function(i, j) {
    Ax <- M[i-j,1]
    Ay <- M[i-j,2]
    Bx <- M[i,1]
    By <- M[i,2]
    unname(atan2(Ay-By, Ax-Bx))
  })
  
  return(headings)
}
# function to extend the line
st_extend_line <- function(line, distance, end = "BOTH")
{
  if (!(end %in% c("BOTH", "HEAD", "TAIL")) | length(end) != 1) stop("'end' must be 'BOTH', 'HEAD' or 'TAIL'")
  
  M <- sf::st_coordinates(line)[,1:2]
  keep <- !(end == c("TAIL", "HEAD"))
  
  ends <- c(1, nrow(M))[keep]
  headings <- st_ends_heading(line)[keep]
  distances <- if (length(distance) == 1) rep(distance, 2) else rev(distance[1:2])
  
  M[ends,] <- M[ends,] + distances[keep] * c(cos(headings), sin(headings))
  newline <- sf::st_linestring(M)
  
  # If input is sfc_LINESTRING and not sfg_LINESTRING
  if (is.list(line)) newline <- sf::st_sfc(newline, crs = sf::st_crs(line))
  
  return(newline)
}
################################################################################
#  type in box
pts <- rbind(c(-150.531919, 60.973121), c(-150.466706, 60.940208), c(-150.363625, 60.987604), 
                c(-150.437403, 61.019285), c(-150.531919, 60.973121))

test <- list(pts) %>% st_polygon() %>% st_sfc(crs = 4326) %>%
  st_transform(crs = 3338)
  
plot(st_geometry(test))

set.seed(123)

design <- make.design(region = make.region(region.name = "test", shape = test),
                      design.angle = 145, spacing = 1250)
survey <- generate.transects(design)
plot(test)
plot(survey, add = T)
################################################################################
#should make below into function
test.lines <- survey@samplers %>%
  filter(transect > 1) %>% #remove short transect
  mutate(Replicate = rep(LETTERS[1:2], times = 3), transect = 1:6) %>%
  rename(Transect = transect)
plot(st_geometry(test.lines))
################################################################################
#make navigation files in 2 required formats
# a csv POINT file with header:
# WAYPOINT_NAME, SURVEY, LAT, LON
# WAYPOINT_NAME is a combined survey, transect and waypoint endpoint name
coords <- test.lines %>% 
  st_transform(crs = 4326) %>% #back to lat, long
  st_coordinates()
nav <- test.lines %>% st_cast("POINT", warn = FALSE) %>%
  st_drop_geometry() %>%
  cbind(coords) %>%
  mutate(End = rep(c("N", "S"), times = 6)) %>%
  mutate(WAYPOINT_NAME = paste0(strata, Transect, Replicate, End), SURVEY = strata, 
         LAT = Y, LON = X) %>% 
  select(WAYPOINT_NAME, SURVEY, LAT, LON, L1)
#add a buffer for "lead in lines", current 1 nautical mile or 1.852 km
try <- st_extend_line(st_geometry(test.lines[1,]), 1852)
plot(try, col = "red")
plot(test.lines[1,], add = TRUE)
try2 <- test.lines
for(i in 1:dim(test.lines)[1]){
  try <- st_extend_line(st_geometry(test.lines[i,]), 1852)
  st_geometry(try2)[i] <- st_geometry(try)
}
plot(st_geometry(try2), col = "red")
plot(st_geometry(test.lines), add= TRUE)

coords <- try2 %>% 
  st_transform(crs = 4326) %>% #back to lat, long
  st_coordinates()

nav <- try2 %>% st_cast("POINT", warn = FALSE) %>%
  st_drop_geometry() %>%
  cbind(coords) %>%
  mutate(End = "L") %>%
  mutate(WAYPOINT_NAME = paste0(strata, Transect, Replicate, End), 
         SURVEY = strata, LAT = Y, LON = X) %>% 
  select(WAYPOINT_NAME, SURVEY, LAT, LON, L1) %>% 
  rbind(nav) %>%
  group_by(L1) %>%
  arrange(LAT, .by_group = TRUE) %>%
  ungroup() %>%
  select(WAYPOINT_NAME, SURVEY, LAT, LON) 
#Type 1, with header:
#must be named as this
write.table(nav, file = "nav-test-data/user_waypoints.csv", sep = ",", row.names = FALSE, 
            quote = FALSE)
#Type 2 without header and some strange extension
#need the same darn thing but without the header and some strange extension
#must be named as this
write.table(nav, file = "nav-test-data/user.wpt", sep = ",", row.names = FALSE, 
            col.names = FALSE, quote = FALSE)
################################################################################
#file Type 3:
#make trigger point in required format for Aviatrix
#replicate A
triggers <- test.lines %>% filter(Replicate == "A") %>%
  trans2points(dist = 75) %>%
  st_as_sf() %>%
  mutate(Replicate = "A", Transect = 1:3) 
coords <- triggers %>%
  st_transform(crs = 4326) %>% #back to lat, long
  st_coordinates()
triggers <- triggers %>% 
  st_cast("POINT", warn = FALSE) %>% 
  st_drop_geometry() %>%
  cbind(coords) %>%
  select(Replicate, Transect, Frame = L1, Lat = Y, Lon = X)
write.table(triggers, file = "nav-test-data/triggersA.csv", sep = ",", 
            row.names = FALSE, quote = FALSE)
#replicate B
triggers <- test.lines %>% filter(Replicate == "B") %>%
  trans2points(dist = 75) %>%
  st_as_sf() %>%
  mutate(Replicate = "B", Transect = 1:3) 
coords <- triggers %>%
  st_transform(crs = 4326) %>% #back to lat, long
  st_coordinates()
triggers <- triggers %>% 
  st_cast("POINT", warn = FALSE) %>% 
  st_drop_geometry() %>%
  cbind(coords) %>%
  select(Replicate, Transect, Frame = L1, Lat = Y, Lon = X)
write.table(triggers, file = "nav-test-data/triggersB.csv", sep = ",", 
            row.names = FALSE, quote = FALSE)
################################################################################
#File Type 4:
#Now need a KML, specifications are not well-defined
# Do we need altitude? 
# see this, https://gdal.org/drivers/vector/kml.html
# maybe GDAL does not support all attributes needed for KMLs from simple features
# or perhaps the requirements have not been define or know by us?
#I will assume we want points and altitude at 1500ft = 457.2m
navkml <- nav %>% mutate(ALTITUDE = 457.2) %>%
  st_as_sf(coords=c("LON", "LAT", "ALTITUDE"), dim="XYZ", crs = st_crs(4326))
st_write(navkml, dsn="nav-test-data/nav.kml", driver = "KML", layer = "nav", 
         append = FALSE)
