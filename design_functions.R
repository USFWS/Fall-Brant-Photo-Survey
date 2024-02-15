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
#make navigation files in 4 required formats
#
# a csv POINT file with header:
# WAYPOINT_NAME, SURVEY, LAT, LON
# WAYPOINT_NAME is a combined survey, transect and waypoint endpoint name
# test.lines <- transects %>%
#   filter(Replicate == 1, Panel == "A") %>%
#   mutate(SURVEY = "IZE", Transect = row.names(.))
####function stats here
nav_files <- function(test.lines, path = getwd(), leadin = 1852, 
                      trigger.dist = 75, rep = NULL, panel = NULL, select = 1:4, 
                      digits = 7){
  # path = "design2023"
  # rep = "1"
  # panel = "A"
  
  coords <- test.lines %>% 
    st_transform(crs = 4326) %>% #back to lat, long
    st_coordinates()
  nav <- test.lines %>% st_cast("POINT", warn = FALSE) %>%
    st_drop_geometry() %>%
    cbind(coords) %>%
    mutate(End = rep(c("N", "S"), times = dim(test.lines)[1])) %>%
    mutate(WAYPOINT_NAME = paste0(str_sub(SURVEY, 1,2), Replicate, Panel, Transect, End),  
           LAT = Y, LON = X) %>% 
    select(WAYPOINT_NAME, SURVEY, LAT, LON, L1)
  #add a buffer for "lead in lines", current 1 nautical mile or 1.852 km
  # try <- st_extend_line(st_geometry(test.lines[1,]), 1852)
  # plot(try, col = "red")
  # plot(test.lines[1,], add = TRUE)
  try2 <- test.lines
  for(i in 1:dim(test.lines)[1]){
    try <- st_extend_line(st_geometry(test.lines[i,]), 1852)
    st_geometry(try2)[i] <- st_geometry(try)
  }
  #plot(st_geometry(try2), col = "red")
  #plot(st_geometry(test.lines), add= TRUE)
  
  coords <- try2 %>% 
    st_transform(crs = 4326) %>% #back to lat, long
    st_coordinates()
  
  nav2 <- try2 %>% st_cast("POINT", warn = FALSE) %>%
    st_drop_geometry() %>%
    cbind(coords) %>%
    mutate(End = rep(c("NL", "SL"), times = dim(test.lines)[1])) %>%
    mutate(WAYPOINT_NAME = paste0(str_sub(SURVEY, 1,2), Replicate, Panel, Transect, End), 
           LAT = Y, LON = X) %>% 
    select(WAYPOINT_NAME, SURVEY, LAT, LON, L1) %>% 
    rbind(nav) %>%
    group_by(L1) %>%
    arrange(LAT, .by_group = TRUE) %>%
    ungroup() %>%
    mutate(LAT = round(LAT, digits), LON = round(LON, digits)) %>%
    select(WAYPOINT_NAME, SURVEY, LAT, LON) 
  
  ################################################################################
  #file Type 3:
  #make trigger point in required format for Aviatrix
  triggers <- test.lines %>%
    trans2points(dist = 75) %>%
    st_as_sf() %>%
    cbind(st_drop_geometry(test.lines))
  coords <- triggers %>%
    st_transform(crs = 4326) %>% #back to lat, long
    st_coordinates()
  triggers <- triggers %>% 
    st_cast("POINT", warn = FALSE) %>% 
    st_drop_geometry() %>%
    group_by(Transect) %>% 
    mutate(FRAME = row_number()) %>% 
    cbind(coords) %>%
    select(RUN = Transect, FRAME, LAT = Y, LON = X)
  
  
  ################################################################################
  #File Type 4:
  #Now need a KML, specifications are not well-defined
  # Do we need altitude? 
  # see this, https://gdal.org/drivers/vector/kml.html
  # maybe GDAL does not support all attributes needed for KMLs from simple features
  # or perhaps the requirements have not been define or know by us?
  #I will assume we want points and altitude at 1500ft = 457.2m
  #%>% mutate(ALTITUDE = 457.2) 
  
  tmp2 <- try2 %>% 
    st_transform(crs = 4326) %>%
    select(WAYPOINT_NAME = Transect, SURVEY)
  
  navkml <- nav2 %>%
    #st_as_sf(coords=c("LON", "LAT", "ALTITUDE"), dim="XYZ", crs = st_crs(4326)) %>%
    st_as_sf(coords=c("LON", "LAT"), dim="XY", crs = st_crs(4326)) %>%
    rbind(tmp2)
  ################################################################################
  #write all the files
  #Type 1, with header:
  #must be named as this
  if( 1 %in% select ){
    write.table(nav2, file = paste0(path,"/user_waypoints", rep, panel, ".csv"), sep = ",", 
                row.names = FALSE, quote = FALSE)}
  #Type 2 without header and some strange extension
  #need the same darn thing but without the header and some strange extension
  #must be named as this
  if( 2 %in% select ){
    write.table(nav2, file = paste0(path,"/user", rep, panel, ".wpt"), sep = ",", row.names = FALSE, 
                col.names = FALSE, quote = FALSE)}
  #Type 3 trigger points
  if( 3 %in% select ){
    write.table(triggers, file = paste0(path,"/triggers", rep, panel, ".csv"), sep = ",", 
                row.names = FALSE, quote = FALSE)}
  #File Type 4: KML
  if( 4 %in% select ){
    st_write(navkml, paste0(path,"/IZF_Panel", rep, panel, ".kml"))}
  ################################################################################ 
}
