#apply estimate to 17-19 photos
library(tidyverse)
library(glmmTMB)
library(sf)
library(tmap)
# Area of Izembek Lagoon (sq ha)
lagoonha <- 341376583/10000
files <- list.files(path="goose_fallSurveyCounts_izembek_weiser/goose_fallSurveyCounts_izembek_weiser/2017", 
                    full.names = TRUE,
                    pattern = "csv")
#the first csv file seems to have redundant column, delete and add
dat1 <- read_csv(files[1], col_select = 1:20) %>%
  mutate(File = 1)
dat <- map_dfr(files[-1], read_csv, .id = "File") %>%
  mutate(File = as.numeric(File)+1)
#rearrange dat1
dat1 <- dat1[,c(names(dat1)[21],names(dat1)[-21])]
names(dat1) <- names(dat)
dat <- bind_rows(dat1, dat)
rm(dat1)
dat <- dat %>%
  mutate(Replicate = if_else(File %in% 1:2, 1, 
                             if_else(File %in% 3:4, 2,
                                     if_else(File %in% 5:6, 3, 
                                             if_else(File %in% 7:8, 4, 
                                                     if_else(File %in% 9:10, 5, NA)))))) %>%
  mutate(Replicate = as.factor(Replicate), 
         Count = if_else(Auto_Brant==0, 0, Manual_Brant), 
         Density = 10000*Count/PhotoArea) %>% #in sq. ha
  filter(!is.na(Density))
#always plot your data!
hist(dat$Density)
hist(dat$Density[dat$Density>0])
hist(dat$Count[dat$Count>0])
hist(dat$PhotoArea)
hist(dat$Altitude)
plot(dat$Replicate, dat$Altitude)
group_by(dat, Replicate) %>% summarise(mAlt = mean(Altitude), mArea = mean(PhotoArea), 
                                       n = n(), mDen = mean(Density), 
                                       mFocal = mean(FocalLength))
#humm, Rep #1, Cam 1 is 200mm focal length and Cam 2 is 100mm focal length?
#Rep #2 cam 2 is focal length 70!
#Estimate with the combined (see below) is > 400K! What to do?
#plot on map

#get lagoon polygon, need to fix
temp <- tempfile()
unzip(zipfile = "Izembek_shapefile.zip", exdir = temp)
lag <- read_sf(dsn=temp) %>%
  st_union() %>%
  st_boundary() %>%
  st_cast("POLYGON")
tmap_mode("view")
for(i in 1:4){
df <-  dat %>% filter(!is.na(Latitude), Replicate == i) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)
tm <- tm_shape(lag) + tm_polygons(fill = "red", fill_alpha = 0.5) + 
  tm_shape(df) + tm_dots() + 
  tm_title(paste0("Replicate = ", i))
#  tm_basemap(server = "Esri.WorldImagery")
print(tm)
}
#Replicate 3 was not completed! Remove from data set!
dat <- filter(dat, Replicate != "3")
################################################################################
#model as ft by Weiser et al. 2022
fit <- glmmTMB(Density ~ (1|Replicate), ziformula=~1, dispformula=~1,
               data=dat, family="gaussian")
summary(fit)
#plot(profile(fit))
res <- residuals(fit, type = "pearson")
pred <- predict((fit), type = "link")
plot(pred, res)
hist(res[res>0])
#try gamma
fit2 <- glmmTMB(Density ~ (1|Replicate), ziformula=~1, dispformula=~1,
               data=dat, family=ziGamma(link = "log"))
summary(fit2)
#similar
#what about a random effect of the zi part of model?
fit3 <- glmmTMB(Density ~ (1|Replicate), ziformula=~(1|Replicate), dispformula=~1,
               data=dat, family="gaussian")
summary(fit3)
################################################################################
#try mgcv
library(mgcv)
fit2 <- gam(Density~1, family = "tw", data = dat)
summary(fit2)
gam.check(fit2) #strange
#try a zi model in mgcv
fit3 <- gam(list(Count~1, ~1), family=ziplss(), data = dat)
summary(fit3)
gam.check(fit3)
#Fit a spatial smooth to look at distribution of brant
df <- st_as_sf(dat, coords = c("Longitude", "Latitude"), crs = 4326) |>
  st_transform(crs = 3338)
xy <-st_coordinates(df)
df <- st_drop_geometry(df) |> cbind(xy) |> as.data.frame() |>
  filter(Replicate != 3) |> #for now forget rep 3
  mutate(Replicate = factor(Replicate))

fit.sp <- gam(Density ~ s(X, Y, by = Replicate, k = c(100)), family = tw, 
              method = "REML", data = df)
summary(fit.sp)
gam.check(fit.sp) #doesn't look too bad
par(mfrow=c(2,2))
for(i in c(1,2,4)){
vis.gam(fit.sp, plot.type = "contour", cond = list(Replicate = i), too.far = 0.1, 
        main = paste0("Replicate = ", i))}
par(mfrow= c(1,1))
################################################################################
#try a simple design-based plot estimate
est <- dat %>%
  #filter(FocalLength == 200) |> #I think they only used 200
  #group_by(Replicate, FocalLength) %>% #trying to understand why these estimate are different
  group_by(Replicate) %>%
  summarize(mu = mean(Density), sigma = sd(Density), a = sum(PhotoArea)/10000,
            f = a/lagoonha,
            n = n(),
            N_hat = lagoonha*mu, 
            sd_N = sqrt(lagoonha^2 * (1 - f) * (sigma^2)/n)
  )
#average estimate over reps
est %>% summarise(N_bar = mean(N_hat), sd_N_bar = sqrt(sum(sd_N^2)/3), 
                  cv = sd_N_bar/N_bar)
#try grouping by Camera:
est2 <- dat %>%
  group_by(Replicate, Camera) %>%
  summarize(mu = mean(Density), sigma = sd(Density), a = sum(PhotoArea)/10000,
            f = a/lagoonha,
            n = n(),
            N_hat = lagoonha*mu, 
            sd_N = sqrt(lagoonha^2 * (1 - f) * (sigma^2)/n)
  )
#wow, cam #2 rep. #2 has focal length of 70!
est2 %>% ungroup() %>% summarise(N_bar = mean(N_hat), sd_N_bar = sqrt(sum(sd_N^2)/3), 
                  cv = sd_N_bar/N_bar)
#turns out Weiser removed all photos with alt < 350m or > 550m and focal length < 200
dat <- mutate(dat, PhotoType = if_else(Altitude < 350 | Altitude > 550 | FocalLength < 200, 
                                      "Non-standard", "Standard"))
group_by(dat, Replicate, Camera, PhotoType) |> summarise(n = n(), 
                                                 mArea = mean(PhotoArea), 
                                                 mAlt = mean(Altitude), 
                                                 mDensity = mean(Density))
#calculate estimate based on "standard" photos only
est2 <- dat %>%
  filter(PhotoType == "Standard") |> 
  group_by(Replicate) %>%
  summarize(mu = mean(Density), sigma = sd(Density), a = sum(PhotoArea)/10000,
            f = a/lagoonha,
            n = n(),
            N_hat = lagoonha*mu, 
            sd_N = sqrt(lagoonha^2 * (1 - f) * (sigma^2)/n)
  )
################################################################################
#try 2018
files <- list.files(path="goose_fallSurveyCounts_izembek_weiser/goose_fallSurveyCounts_izembek_weiser/2018", 
                    full.names = TRUE,
                    pattern = "csv")
dat <- map_dfr(files, read_csv, .id = "File")
dat <- dat %>%
  mutate(Replicate = if_else(File %in% 1:2, 1, 
                             if_else(File %in% 3:4, 2,
                                     if_else(File %in% 5:6, 3, 
                                             if_else(File %in% 7:8, 4, 
                                                     if_else(File %in% 9:10, 5, NA)))))) %>%
  mutate(Replicate = as.factor(Replicate), 
         Count = if_else(Auto_Brant==0, 0, Manual_Brant), 
         Density = 10000*Count/PhotoArea) %>% #in sq. ha
  filter(!is.na(Density))
group_by(dat, Replicate) %>% summarise(mAlt = mean(Altitude), mArea = mean(PhotoArea), 
                                       n = n(), mDen = mean(Density), 
                                       mFocal = mean(FocalLength))

df <-  dat %>% filter(!is.na(Latitude), Replicate == 1) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)
tm_shape(lag) + tm_polygons(col = "red", alpha = 0.5) + 
  tm_shape(df) + tm_dots() + 
  tm_basemap(server = "Esri.WorldImagery")
#make estimates
est <- dat %>%
  group_by(Replicate) %>%
  summarize(mu = mean(Density), sigma = sd(Density), a = sum(PhotoArea)/10000,
            f = a/lagoonha,
            n = n(),
            N_hat = lagoonha*mu, 
            sd_N = sqrt(lagoonha^2 * (1 - f) * (sigma^2)/n)
  )
est
###########################
#only standard photos
dat <- mutate(dat, PhotoType = if_else(Altitude < 350 | Altitude > 550 | FocalLength < 200, 
                                       "Non-standard", "Standard"))
group_by(dat, Replicate, Camera, PhotoType) |> summarise(n = n(), 
                                                         mArea = mean(PhotoArea), 
                                                         mAlt = mean(Altitude), 
                                                         mDensity = mean(Density))
#calculate estimate based on "standard" photos only
est2 <- dat %>%
  filter(PhotoType == "Standard") |> 
  group_by(Replicate) %>%
  summarize(mu = mean(Density), sigma = sd(Density), a = sum(PhotoArea)/10000,
            f = a/lagoonha,
            n = n(),
            N_hat = lagoonha*mu, 
            sd_N = sqrt(lagoonha^2 * (1 - f) * (sigma^2)/n)
  )
est
est2 #all same, good
###########################
#average estimate over reps
est %>% summarise(N_bar = mean(N_hat), sd_N_bar = sqrt(sum(sd_N^2)/3), 
                  cv = sd_N_bar/N_bar)
###Now try 2019
files <- list.files(path="goose_fallSurveyCounts_izembek_weiser/goose_fallSurveyCounts_izembek_weiser/2019", 
                    full.names = TRUE,
                    pattern = "csv")
dat <- map_dfr(files, read_csv, .id = "File")
dat <- dat %>%
  mutate(Replicate = if_else(File %in% 1:2, 1, 
                             if_else(File %in% 3:4, 2,
                                     if_else(File %in% 5:6, 3, 
                                             if_else(File %in% 7:8, 4, 
                                                     if_else(File %in% 9:10, 5, NA)))))) %>%
  mutate(Replicate = as.factor(Replicate), 
         Count = if_else(Auto_Brant==0, 0, Manual_Brant), 
         Density = 10000*Count/PhotoArea) %>% #in sq. ha
  filter(!is.na(Density))
group_by(dat, Replicate) %>% summarise(mAlt = mean(Altitude), mArea = mean(PhotoArea), 
                                       n = n(), mDen = mean(Density), 
                                       mFocal = mean(FocalLength))
df <-  dat %>% filter(!is.na(Latitude), Replicate == 3) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)
tm_shape(lag) + tm_polygons(col = "red", alpha = 0.5) + 
  tm_shape(df) + tm_dots() + 
  tm_basemap(server = "Esri.WorldImagery")
#rep 3 is missing transect on ends of lagoon
#make estimates
est <- dat %>%
  group_by(Replicate) %>%
  summarize(mu = mean(Density), sigma = sd(Density), a = sum(PhotoArea)/10000,
            f = a/lagoonha,
            n = n(),
            N_hat = lagoonha*mu, 
            sd_N = sqrt(lagoonha^2 * (1 - f) * (sigma^2)/n)
  )
est
###########################
#only standard photos
dat <- mutate(dat, PhotoType = if_else(Altitude < 350 | Altitude > 550 | FocalLength < 200, 
                                       "Non-standard", "Standard"))
group_by(dat, Replicate, Camera, PhotoType) |> summarise(n = n(), 
                                                         mArea = mean(PhotoArea), 
                                                         mAlt = mean(Altitude), 
                                                         mDensity = mean(Density))
#calculate estimate based on "standard" photos only
est2 <- dat %>%
  filter(PhotoType == "Standard") |> 
  group_by(Replicate) %>%
  summarize(mu = mean(Density), sigma = sd(Density), a = sum(PhotoArea)/10000,
            f = a/lagoonha,
            n = n(),
            N_hat = lagoonha*mu, 
            sd_N = sqrt(lagoonha^2 * (1 - f) * (sigma^2)/n)
  )
est
est2
##########################
#average estimate over reps
est %>% summarise(N_bar = mean(N_hat), sd_N_bar = sqrt(sum(sd_N^2)/3), 
                  cv = sd_N_bar/N_bar)
est %>% filter(Replicate != 3) %>% 
  summarise(N_bar = mean(N_hat), sd_N_bar = sqrt(sum(sd_N^2)/3), 
                  cv = sd_N_bar/N_bar)
################################################################################
## Make estimate from YOLO count output
# Replicate #1 2022-20-09
# this is a naive, SRS estimator: mean density in photo*Lagoon area
#   No correction for false negative/positive or misclassification
yolo <- read_csv(file = "image_level_results/counts-1.csv")
detections <- yolo %>% group_by( filename ) %>% filter( any(count_brant > 0) )
ggplot(data = detections, aes(x=threshold_brant, y = count_brant, group = filename)) + 
  geom_line()
#subset by counts
# > 200
ggplot(data = filter(detections, any(count_brant > 200)), aes(x=threshold_brant, y = count_brant, group = filename)) + 
  geom_line()
#100 -200
ggplot(data = filter(detections, aany(between(count_brant, 100, 200))), aes(x=threshold_brant, y = count_brant, group = filename)) + 
  geom_line()
# < 100
ggplot(data = filter(detections, all(count_brant < 100)), aes(x=threshold_brant, y = count_brant, group = filename)) + 
  geom_line()
# < 50
ggplot(data = filter(detections, all(count_brant < 50)), aes(x=threshold_brant, y = count_brant, group = filename)) + 
  geom_line()
######################
## Population estimates by threshold
# read in yolo output
yolofiles <- list.files(path = "image_level_results", pattern = "csv", full.names = TRUE)
yolo <- map(yolofiles, read_csv) %>% list_rbind(names_to = "Replicate")
# need altitude of photo
# filelist <- list(CAM3 = "2022_geotagged_photo_data/Exif_geo_2022-10-09_cam3.csv", 
#                  CAM4 = "2022_geotagged_photo_data/Exif_geo_2022-10-09_cam4.csv")
# camera <- map(filelist, read_csv) %>% list_rbind(names_to = "Camera")
filelist <- list.files(path = "2022_geotagged_photo_data", full.names = TRUE)
names(filelist) <- rep(c("CAM3", "CAM4"), times = 5)
camera <- map(filelist, read_csv) %>% list_rbind(names_to = "Camera") %>%
  mutate(Day = day(as_date(DateTimeOriginal)), 
         Replicate = case_when(
           Day == 9 ~ 1,
           Day ==11 ~ 2,
           Day == 12 ~ 3,
           Day == 16 ~ 4,
           Day == 17 ~ 5
         ))
# need function to calculate area, from Emily Weiser USGS
source("fn_photo_footprint.R")
#calculate photo area and then join to yolo output
camera <- mutate(camera, Photo_Area = fn_photoarea(Height, FocalLength)) %>%
  select(Replicate, Camera, FileName, Height, Photo_Area, Longitude, Latitude, DateTimeOriginal)
yolo <- mutate(yolo, FileName = str_sub(filename, -12, -1), 
               Camera = toupper(str_sub(filename, 1, 4))) %>%
  select(Replicate, Camera, FileName, threshold_brant, count_brant) %>% #just brant for now
  left_join(camera)
###Calculate a population estimate for each confidence level
# ignore non-detections, misclassifications/false positives
mden <- yolo %>% group_by(Replicate, threshold_brant) %>%
  summarize(mDensity = mean(10000*count_brant/Photo_Area), #change units to sq ha
            sdDensity = sd(10000*count_brant/Photo_Area), 
            a = sum(Photo_Area)/10000,
            n = n(),
            f = a/lagoonha,
            N_hat = lagoonha*mDensity, 
            sd_N = sqrt(lagoonha^2 * (1 - f) * (sdDensity^2)/n),
            lower = N_hat - 2*sd_N,
            upper = N_hat + 2*sd_N
            ) %>%
  mutate(Replicate = factor(Replicate))
#Now plot
ggplot(data = mden) + 
  geom_pointrange(aes(x=threshold_brant, y = N_hat, ymin = lower, ymax = upper, 
                      col = Replicate)) + 
  #labs(x = "YOLO confidence threshold", y = "Population estimate") +
  scale_x_continuous(name = "YOLO confidence threshold", breaks = seq(0.1, 0.9, by = 0.1), 
                   labels = seq(0.1, 0.9, by = 0.1)) + 
  scale_y_continuous(name = "Population estimate (K, +/- 2 SE)", breaks = seq(0, 700000, by = 100000), 
                     labels = seq(0, 700, by = 100))
  
#how many photos have birds?
counts <- yolo %>% 
  mutate(Replicate = factor(Replicate), 
         haveB = if_else(count_brant > 0, 1, 0)) %>% 
  group_by(Replicate, threshold_brant) %>%
  summarise(n = n(), photoB = sum(haveB), 
            sumB = sum(count_brant)) %>%
  mutate(proB = photoB/n)
ggplot(data = counts) + 
  geom_point(aes(x = threshold_brant, y = photoB, col = Replicate))
ggplot(data = counts) + 
  geom_point(aes(x = threshold_brant, y = sumB, col = Replicate))
ggplot(data = counts) + 
  geom_point(aes(x = threshold_brant, y = proB, col = Replicate))



