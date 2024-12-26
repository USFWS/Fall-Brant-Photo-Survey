#script to create and write the 2023 (and potential beyound) Izembek photo brant survey
#Erik Osnas
################################################################################
# File are described in SOP-1.docx
#four files needed:
# 1. user_waypoints.csv: csv point file with specific colmun names for ForeFlight
# 2. user.wpt: same as above but no header, for Garmin GPS
# 3. triggerX.csv: csv point file for Aviatix
# 4. KML file for ForeFlight (glass pannel) navigation)
################################################################################
# Below requires R 4.3 and GOES 3.11
#load packages
library(tidyverse)
library(sf)
library(tmap)
library(dsims)
#load required function
source("design_functions.R")
#First the survey area polygons need to be defined, then lines and points on those.
#These will be buffered and simplified based on the exploration in map_area.R
#main izembek bay
temp <- tempfile()
unzip(zipfile = "../data/Izembek_shapefile.zip", exdir = temp)
lag <- read_sf(dsn=temp)
#simplified lagoon
lag3 <- st_boundary(st_union(lag)) %>% 
  st_cast("POLYGON")
#add buffer
lag4 <- st_buffer(lag3, dist = 500)
#plot
tmap_mode("view")
tm_shape(lag3) + tm_polygons(col = "red", col_alpha = 0.5) + 
  tm_shape(lag4) + tm_polygons(col = "blue", col_alpha = 0.5) + 
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
lag7 <- st_concave_hull(lag4, ratio = 0.2, allow_holes = FALSE)
tm_shape(lag3) + tm_polygons(col = "red", col_alpha = 0.5) + 
  #tm_shape(lag4) + tm_polygons(col = "blue", alpha = 0.5) + 
  #tm_shape(lag5) + tm_polygons(col = "orange", alpha = 0.5) + 
  tm_shape(lag7) + tm_polygons(col = "green", col_alpha = 0.5) + 
  tm_basemap(server = "Esri.WorldImagery")
#lag7 looks good
set.seed(123)
region <- make.region(region.name = "Izembek Lagoon",
                      shape = lag7,
                      units = "m")
parallel.design <- make.design(region = region, 
                               design = "systematic",
                               spacing = 1250/3,
                               edge.protocol = "minus",
                               design.angle = 135,
                               truncation = 50)
p.survey <- generate.transects(parallel.design)
plot(region, p.survey)
transects <- p.survey@samplers %>%
  mutate(Replicate = factor(rep(1:3, length=dim(p.survey@samplers)[1])), 
         Panel = rep(rep(LETTERS[1:2], each = 3), length=dim(p.survey@samplers)[1]))
#plot
ggplot(data = region@region) + 
  geom_sf(fill = "lightgray") + 
  geom_sf(data = transects, aes(col=Replicate, linetype = Panel))

#test
# lines <- transects %>%
#   filter(Replicate == 3, Panel == "B") %>%
#   mutate(SURVEY = "IZE", Transect = row.names(.))
# nav_files(test.lines = lines, path = "design2023", leadin = 1852, 
#           trigger.dist = 75, rep = NULL, panel = NULL, select = 1:4)
#works!
#use it!
################################################################################
RR <- unique(transects$Replicate)
PP <- unique(transects$Panel)
for(i in 1:length(RR)){
  for(j in 1:length(PP)){
    lines <- transects %>%
      filter(Replicate == i, Panel == PP[j]) %>%
      mutate(SURVEY = "IZE", Transect = row.names(.))
    nav_files(test.lines = lines, path = "design2023", leadin = 1852, 
              trigger.dist = 75, rep = i, panel = PP[j], select = 3:4)
    if( i == 1 & j == 1) all.lines <- lines else{
      all.lines <- rbind(all.lines, lines)
    }
  }
}
nav_files(test.lines = all.lines, path = "design2023", leadin = 1852, 
          trigger.dist = 75, rep = NULL, panel = NULL, select = 1:2)
################################################################################
##Outer bays
#look at flight path of Heather
temp <- tempfile()
unzip(zipfile = "IZF_OcularSegs.zip", exdir = temp)
lag <- read_sf(dsn=temp) %>%
  st_transform(crs=4326) %>%
  #select just the segments of the outer bays
  #removing 11 or segment 69 as per Heather: non-habitat
  filter(ID %in%c(8, 9, 14, 19)) 
tmap_mode("view")  
# tm_shape(lag) + tm_polygons(col = "red", alpha = 0.5) + 
#   tm_basemap(server = "Esri.WorldImagery")
#seem to tight to shoreline and too complex
#we will buffer and simplify
#combine bays into three
# df <- lag %>% filter(ID %in%c(8,9)) %>%
#   st_union()
# df <- st_sf(cbind(data.frame(ID = 1, Unit = NA),df)) %>%
#   rbind(filter(lag, ID%in%c(14, 19)))
# plot(st_geometry(df))
#add buffer in meters
df <- st_transform(lag, crs = 3338) %>% st_buffer(dist = 200)
# tm_shape(df) + tm_polygons(col = "red", alpha = 0.5) + 
#   tm_basemap(server = "Esri.WorldImagery")
#looks OK, remove holes
df <-  st_multipolygon(lapply(st_geometry(df), function(x) x[1])) %>%
  st_sfc(crs=3338) %>%
  st_cast("POLYGON") %>%
  cbind(data.frame(Bay = c("A", "B", "C", "D"))) %>% 
  st_sf()
#remove that portion of XX south of 55.047; 
plot(st_geometry(df))
df2 <- st_transform(df, crs = 4326) %>% 
  st_cast("POINT") %>%
  st_coordinates() 
summary(df2)

df2 <- filter(df, Bay == "C") %>% 
  st_transform(df, crs = 4326) %>% 
  st_crop(c(xmin=-164, xmax=-161, ymin=55.04, ymax=55.4)) 
#A little more south than what Heather gave

df <- filter(df, Bay != "C") %>% 
  st_transform(df, crs = 4326) %>%
  rbind(df2)

plot(st_geometry(df))
# seldom any brant according to ocular survey/Heather
tm_shape(df) + tm_polygons(col = "red", alpha = 0.5) + 
  tm_basemap(server = "Esri.WorldImagery")
# simplify to remove convolutions that lead to problems below
# try to make transect in one line, below I can't solve the problem of multilinesstrings
# the nav_files function does not work with MULTILINESTRING geometry objects
# and I can't seem to figure out how to cast these to joins lines
df2 <- st_concave_hull(df, ratio = 1, allow_holes = FALSE)
plot(st_geometry(df2))
tm_shape(df2) + tm_polygons(col = "red", alpha = 0.5) + 
  tm_basemap(server = "Esri.WorldImagery")
#looks good to me!
df <- df2
#############Design survey for bays
set.seed(123)
df <- st_transform(df, crs = 3338) %>%
  arrange(Bay) #reorder bays
region <- make.region(region.name = "Izembek Outer Bays",
                      shape = df,
                      strata.name = LETTERS[1:4],
                      units = "m")
parallel.design <- make.design(region = region, 
                               design = "systematic",
                               spacing = 1250/3,
                               edge.protocol = "minus",
                               design.angle = 135,
                               truncation = 50)
p.survey <- generate.transects(parallel.design)
plot(region, p.survey)
#assume 45.83 m/s ground speed
p.survey@line.length/(45.83*60) #minutes of flight time
p.survey@cyclictrackline/(45.83*60) #minutes of flight time
test <-  p.survey@samplers %>%
  mutate(Replicate = factor(1), Panel = strata, SURVEY = "OUT") %>%
  group_by(strata) %>% #transects ("RUN" for photo software) within strata need to start with 1 
  mutate(Transect = row_number()) %>%
  ungroup()
nav_files(test.lines = test, path = "design2023/test", leadin = 1852,
          trigger.dist = 75, rep = NULL, panel = NULL, select = 1:4)
#They want separate kml and trigger files for each bay
s <- unique(test$strata)
for( i in 1:4){
  df <- filter(test, strata == s[i])
  nav_files(test.lines = df, path = "design2023/outer", leadin = 1852,
            trigger.dist = 75, rep = NULL, panel = s[i], select = 3:4)
}
# #test
# test <- p.survey@samplers %>%
#   mutate(Replicate = factor(1), Panel = strata, SURVEY = "OUT", Transect = transect) %>%
#   st_cast("LINESTRING")
# nav_files(test.lines = test, path = "design2023/outer", leadin = 1852, 
#           trigger.dist = 75, rep = NULL, panel = NULL, select = 1:4)
# 
# tm_shape(df) + tm_polygons(col = "red", alpha = 0.5) + 
#   tm_shape(test[18:19,]) + tm_lines(col="blue") + 
#   tm_basemap(server = "Esri.WorldImagery")
# 
# mls <- test %>% filter(st_geometry_type(.) == "MULTILINESTRING") %>%
#   st_cast("POINT") %>%
#   group_by(Transect) %>%
#   #st_line_merge()
#   summarize(do_union = FALSE) %>% 
#   st_cast("LINESTRING")
#   
# plot(st_geometry(mls)) 
# 
# tm_shape(df) + tm_polygons(col = "red", alpha = 0.5) + 
#   tm_shape(mls) + tm_lines(col="blue") + 
#   tm_basemap(server = "Esri.WorldImagery")
# 

# dat <- read.csv(file = "design2023/user.wpt", header = FALSE)
# dat <- mutate(dat, V3 = round(V3, 4), V4 = round(V4, 4))
# write.table(dat, file = "user.wpt", sep = ",", row.names = FALSE, 
#               col.names = FALSE, quote = FALSE)
