#This is largely dev code to explore fall brant photo data
# various ideas are developed here:
#  (1) various attempts to replicate Weiser's estimates
#  (2) apply simple design-based SRS estimators
#  (3) use a GAM to map and estimate lagoon population and compare to SRS estimate
#apply estimate to 17-19 photos
library(tidyverse)
library(glmmTMB)
library(sf)
library(tmap)
library(mgcv)
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
################################################################################
#get lagoon polygon, need to fix
temp <- tempfile()
unzip(zipfile = "Izembek_shapefile.zip", exdir = temp)
lag <- read_sf(dsn=temp) %>%
  st_transform(crs = 3338) %>% 
  st_union() %>%
  st_boundary() %>%
  st_cast("POLYGON")
#lagoon polygon is not valid!!
st_is_valid(lag, reason = TRUE)
lag <- st_make_valid(lag)
################################################################################
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
gam.check(fit.sp) #doesn't look too bad, k might be larger
par(mfrow=c(2,2))
for(i in c(1,2,4)){
vis.gam(fit.sp, plot.type = "contour", cond = list(Replicate = i), too.far = 0.1, 
        main = paste0("Replicate = ", i))}
par(mfrow= c(1,1))
#Make a prediction over whole lagoon
#Density is per sq ha, see above
grid <- lag  |> 
  st_make_grid(cellsize = 100) |> #c(83.1, 54.8)) |>
  st_intersection(lag)
plot(st_geometry(grid), pch = ".")
cen <- st_centroid(grid)
area <- st_area(grid) |> units::drop_units()
pred_df <- as.data.frame(st_coordinates(cen)) |> mutate(Area = area/10000, Replicate = 1)
preds <- predict(fit.sp, newdata = pred_df, type = "response")
plot_df <-  mutate(pred_df, Prediction = preds, Ex_Count = Area*Prediction) |>
  cbind(grid) |>
  st_as_sf()
ggplot(data = plot_df) + geom_sf(aes(fill=Ex_Count), col = NA) +
  scale_fill_viridis_c(name = "Expected \n Count")
sum(plot_df$Ex_Count)
#Now simulate SEs
map_post <- 
  function(gamfit = NA, newdata = NULL, group.by = "Replicate",
           Nsamples = 1000, exclude.term = NULL){
    library(tidyverse)
    library(mgcv)
    library(sf)
    
    post <- matrix(0, Nsamples, length(unique(newdata$Replicate)) )
    
    Xp <- predict(gamfit, type="lpmatrix", newdata=newdata, exclude = exclude.term, 
                  newdata.guaranteed=TRUE)
    #sample from parameter posterior
    b <- rmvn(Nsamples, coef(gamfit), vcov(gamfit))
    for(j in 1:Nsamples ){
      p <- exp(Xp%*%b[j,]) * as.vector(newdata$Area) #replicate of prediction at all points
      post[j,] <- cbind(newdata, p) %>% 
        group_by(Replicate) %>% 
        summarize( Total = sum(p) ) %>%
        ungroup() %>%
        select(Total) %>% 
        unlist()
    }
    return(post)
  }
# try it
total_post <- map_post(gamfit = fit.sp, newdata = pred_df, Nsamples = 1000)
sd(total_post)
##well it "works" but gives some very large estimates on some sims, maybe it is a sample size thing?
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
est2
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
est2
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
tm_shape(lag) + tm_polygons(fill = "red", fill_alpha = 0.5) + 
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
tm_shape(lag) + tm_polygons(fill = "red", fill_alpha = 0.5) + 
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
################################################################################
################################################################################
#All data
##first read in data and create a df of all photos
files <- list.files(path="goose_fallSurveyCounts_izembek_weiser/goose_fallSurveyCounts_izembek_weiser", 
                    full.names = TRUE,
                    recursive = TRUE,
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
  mutate(Count = if_else(Auto_Brant==0, 0, Manual_Brant), 
         Density = 10000*Count/PhotoArea, #in sq. ha
         Year = year(Date), 
         Camera = toupper(Camera), 
         PhotoType = if_else(Altitude < 350 | Altitude > 550 | FocalLength < 200, 
                             "Non-standard", "Standard")) %>% 
  group_by(Year) %>%
  mutate(Replicate = as.numeric(factor(Date))) %>%
  ungroup() %>%
  filter(!is.na(Density)) %>%
  select(Date, Year, Replicate, Camera, Latitude, Longitude, Altitude, FocalLength, 
         PhotoArea, PhotoType, Count, Density)
group_by(dat, Year, Replicate, Camera) %>% summarise(mAlt = mean(Altitude), 
                                                     mArea = mean(PhotoArea), 
                                       n = n(), mDen = mean(Density), 
                                       mFocal = mean(FocalLength))
################################################################################
#compare to design-based estimate (no spatial correlation)
dat |> group_by(Year, Replicate, Camera) %>%
  summarize(mu = mean(Density), sigma = sd(Density), a = sum(PhotoArea)/10000,
            f = a/lagoonha,
            n = n(),
            N_hat = lagoonha*mu, 
            sd_N = sqrt(lagoonha^2 * (1 - f) * (sigma^2)/n),
            lower = N_hat - 1.96*sd_N, 
            upper = N_hat + 1.96*sd_N)
#Add plot of photos by year and rep:
plotlist <- list()
spot <- 1
for(i in unique(dat$Year)){
  df <- dat %>% filter(Year == i)
  for(j in unique(df$Replicate)){
    df2 <- df %>% filter(Replicate == j)
    for(k in unique(df2$Camera)){
    df3 <- df2 %>% filter(Camera == k) %>%
      mutate(Replicate = factor(Replicate)) %>%
      st_as_sf(coords = c("Longitude", "Latitude"), crs=4326) 
    p <-ggplot(data = lag) + geom_sf() + 
      geom_sf(data = df3, aes(col = PhotoType), size = 0.5) + 
      #labs(title = paste0("Year = ", i, ", Replicate = ", j, "Camera = ", k)) + 
      scale_colour_manual(name = "PhotoType",values = c("red", "blue")) + 
      theme(#legend.position="none", 
            axis.text.x=element_blank(), 
            axis.ticks.x=element_blank(), 
            axis.text.y=element_blank(), 
            axis.ticks.y=element_blank()) 
            #plot.title = element_text(size=10))
    plotlist[[spot]] <- p
    spot <- spot + 1
  }}}
ggpubr::ggarrange(plotlist = plotlist[1:8], nrow= 4, ncol = 2, common.legend = TRUE)
ggpubr::ggarrange(plotlist = plotlist[9:14], nrow= 3, ncol = 2)
ggpubr::ggarrange(plotlist = plotlist[15:20], nrow= 3, ncol = 2)
################################################################################
#pick a year and fit a GAM
#2017
df <- dat %>% filter(Year == 2017) %>%
  mutate(Replicate = factor(Replicate)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs=4326) %>%
  st_transform(crs = 3338)
df <- cbind(st_drop_geometry(df), st_coordinates(df))

fit.sp <- gam(Density ~ s(X, Y, by = Replicate, k = c(100)), family = tw, 
              method = "REML", data = df)
summary(fit.sp)
gam.check(fit.sp)
par(mfrow=c(2,2))
for(i in c(1,2,3,4)){
  vis.gam(fit.sp, plot.type = "contour", cond = list(Replicate = i), too.far = 0.1, 
          main = paste0("Replicate = ", i))}
par(mfrow= c(1,1))
#Make prediction data frame
newdata <- rbind(pred_df, mutate(pred_df, Replicate = 2)) |>
  rbind(mutate(pred_df, Replicate = 3)) |>
  rbind(mutate(pred_df, Replicate = 4))
preds <- predict(fit.sp, newdata = newdata, type = "response")
grid2 <- st_as_sf(grid) |> slice(rep(row_number(), 4))
st_geometry(grid2) <- "geometry"
plot_df <-  mutate(newdata, Prediction = preds, Ex_Count = Area*Prediction) |>
  cbind(grid2) |>
  st_as_sf()
#test
# df <- filter(plot_df, Replicate == 1)
# ggplot(data = df) + geom_sf(aes(fill=Ex_Count), col = NA) +
#   scale_fill_viridis_c(name = "Expected \n Count")
#works!
for(i in 1:4){
  gg <- ggplot(data = filter(plot_df, Replicate == i)) + geom_sf(aes(fill=Ex_Count), col = NA) +
    scale_fill_viridis_c(name = "Expected \n Count") + 
    labs(title = paste0("Replicate = ", i))
  print(gg)
}
#calculate total
st_drop_geometry(plot_df) |> group_by(Replicate) |> 
  summarise(Total = sum(Ex_Count))
total_post <- map_post(gamfit = fit.sp, newdata = newdata, Nsamples = 1000)
apply(total_post, 2, sd)
apply(total_post, 2, quantile, probs = c(0.025, 0.5, 0.975))
#compare to design-based estimate (no spatial correlation)
dat |> group_by(Year, Replicate) %>%
  summarize(mu = mean(Density), sigma = sd(Density), a = sum(PhotoArea)/10000,
            f = a/lagoonha,
            n = n(),
            N_hat = lagoonha*mu, 
            sd_N = sqrt(lagoonha^2 * (1 - f) * (sigma^2)/n),
            lower = N_hat - 1.96*sd_N, 
            upper = N_hat + 1.96*sd_N)
#####2018#####
df <- dat %>% filter(Year == 2018) %>%
  mutate(Replicate = factor(Replicate)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs=4326) %>%
  st_transform(crs = 3338)
df <- cbind(st_drop_geometry(df), st_coordinates(df))
fit.sp <- gam(Density ~ s(X, Y, by = Replicate, k = c(100)), family = tw, 
              method = "REML", data = df)
summary(fit.sp)
gam.check(fit.sp) #not so good, some issues, maybe try a NB?
par(mfrow=c(2,2))
for(i in c(1,2,3)){
  vis.gam(fit.sp, plot.type = "contour", cond = list(Replicate = i), too.far = 0.1, 
          main = paste0("Replicate = ", i))}
par(mfrow= c(1,1))
#Make prediction data frame
newdata <- rbind(pred_df, mutate(pred_df, Replicate = 2)) |>
  rbind(mutate(pred_df, Replicate = 3)) 
preds <- predict(fit.sp, newdata = newdata, type = "response")
grid2 <- st_as_sf(grid) |> slice(rep(row_number(), 3))
st_geometry(grid2) <- "geometry"
plot_df <-  mutate(newdata, Prediction = preds, Ex_Count = Area*Prediction) |>
  cbind(grid2) |>
  st_as_sf()
for(i in 1:3){
  gg <- ggplot(data = filter(plot_df, Replicate == i)) + geom_sf(aes(fill=Ex_Count), col = NA) +
    scale_fill_viridis_c(name = "Expected \n Count") + 
    labs(title = paste0("Replicate = ", i))
  print(gg)
}
#calculate total
st_drop_geometry(plot_df) |> group_by(Replicate) |> 
  summarise(Total = sum(Ex_Count))
total_post <- map_post(gamfit = fit.sp, newdata = newdata, Nsamples = 1000)
apply(total_post, 2, sd)
apply(total_post, 2, quantile, probs = c(0.025, 0.5, 0.975))
#####2019#####
df <- dat %>% filter(Year == 2019) %>%
  mutate(Replicate = factor(Replicate)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs=4326) %>%
  st_transform(crs = 3338)
df <- cbind(st_drop_geometry(df), st_coordinates(df))
fit.sp <- gam(Density ~ s(X, Y, by = Replicate, k = c(100)), family = tw, 
              method = "REML", data = df)
summary(fit.sp)
gam.check(fit.sp) #some issues, try a NB?
par(mfrow=c(2,2))
for(i in c(1,2,3)){
  vis.gam(fit.sp, plot.type = "contour", cond = list(Replicate = i), too.far = 0.1, 
          main = paste0("Replicate = ", i))}
par(mfrow= c(1,1))
#Make prediction data frame
newdata <- rbind(pred_df, mutate(pred_df, Replicate = 2)) |>
  rbind(mutate(pred_df, Replicate = 3)) 
preds <- predict(fit.sp, newdata = newdata, type = "response")
grid2 <- st_as_sf(grid) |> slice(rep(row_number(), 3))
st_geometry(grid2) <- "geometry"
plot_df <-  mutate(newdata, Prediction = preds, Ex_Count = Area*Prediction) |>
  cbind(grid2) |>
  st_as_sf()
for(i in 1:3){
  gg <- ggplot(data = filter(plot_df, Replicate == i)) + geom_sf(aes(fill=Ex_Count), col = NA) +
    scale_fill_viridis_c(name = "Expected \n Count") + 
    labs(title = paste0("Replicate = ", i))
  print(gg)
}
#calculate total
st_drop_geometry(plot_df) |> group_by(Replicate) |> 
  summarise(Total = sum(Ex_Count))
total_post <- map_post(gamfit = fit.sp, newdata = newdata, Nsamples = 1000)
apply(total_post, 2, sd)
apply(total_post, 2, quantile, probs = c(0.025, 0.5, 0.975))
#compare to design-based estimate (no spatial correlation)
dat |> group_by(Year, Replicate) %>%
  summarize(mu = mean(Density), sigma = sd(Density), a = sum(PhotoArea)/10000,
            f = a/lagoonha,
            n = n(),
            N_hat = lagoonha*mu, 
            sd_N = sqrt(lagoonha^2 * (1 - f) * (sigma^2)/n),
            lower = N_hat - 1.96*sd_N, 
            upper = N_hat + 1.96*sd_N)
################################################################################
##Does a NB model work better?
fit.sp.nb <- gam(Density ~ s(X, Y, by = Replicate, k = c(100)), family = nb, 
              method = "REML", data = df)
summary(fit.sp.nb)
gam.check(fit.sp.nb) #this looks worse than the Tweedie
#how about a zero inflated Poisson
df$LogArea <- log(df$PhotoArea)
fit.sp.zp <- gam(Count ~ s(X, Y, by = Replicate, k = c(100)), offset = LogArea, 
                 family = ziP(b = 1), 
                 method = "REML", data = df)
summary(fit.sp.zp) #does not look good, edf = 99, convergence issue?
gam.check(fit.sp.zp) #got error:
# Error in while (sum(ind) > 0 && kk < 50) { : 
#     missing value where TRUE/FALSE needed
# above comment is for b = 0, b = 1 looks even worse, but doesn't give error.
################################################################################
#try increasing the number of knots to 200, does that help?
fit.sp.200 <- gam(Density ~ s(X, Y, by = Replicate, k = c(200)), family = tw, 
              method = "REML", data = df)
summary(fit.sp.200)
gam.check(fit.sp.200)
preds <- predict(fit.sp.200, newdata = newdata, type = "response")
grid2 <- st_as_sf(grid) |> slice(rep(row_number(), 3))
st_geometry(grid2) <- "geometry"
plot_df <-  mutate(newdata, Prediction = preds, Ex_Count = Area*Prediction) |>
  cbind(grid2) |>
  st_as_sf()
for(i in 1:3){
  gg <- ggplot(data = filter(plot_df, Replicate == i)) + geom_sf(aes(fill=Ex_Count), col = NA) +
    scale_fill_viridis_c(name = "Expected \n Count") + 
    labs(title = paste0("Replicate = ", i))
  print(gg)
}
#calculate total
st_drop_geometry(plot_df) |> group_by(Replicate) |> 
  summarise(Total = sum(Ex_Count))
total_post <- map_post(gamfit = fit.sp, newdata = newdata, Nsamples = 1000)
apply(total_post, 2, sd)
apply(total_post, 2, quantile, probs = c(0.025, 0.5, 0.975))
#compare to design-based estimate (no spatial correlation)
dat |> group_by(Year, Replicate) %>%
  summarize(mu = mean(Density), sigma = sd(Density), a = sum(PhotoArea)/10000,
            f = a/lagoonha,
            n = n(),
            N_hat = lagoonha*mu, 
            sd_N = sqrt(lagoonha^2 * (1 - f) * (sigma^2)/n),
            lower = N_hat - 1.96*sd_N, 
            upper = N_hat + 1.96*sd_N)
#Humm, didn't really help the SD issue but increase the model based point (mean) estimates closer to design. 
################################################################################
#Use a gam for 2022 data
library(mgcv)
dat <- read_csv(file = "data/brant_processed.csv")
df <- dat %>% rename(Replicate = rep) %>%
  mutate(Replicate = factor(Replicate)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs=4326) %>%
  st_transform(crs = 3338)
df <- cbind(st_drop_geometry(df), st_coordinates(df))
#Calculate the correction factor 
cf <- df %>% select(Replicate, yolo_density, manual_density) %>%
  drop_na() %>% 
  mutate(has_brant = if_else(yolo_density == 0, 0, 1), 
         cf = manual_density/yolo_density) %>%
  group_by(Replicate, has_brant) %>%
  summarise(n = n(), mcf = sum(manual_density)/sum(yolo_density), 
            cf_se = sum((manual_density - mcf*yolo_density)^2)/(n*(n - 1)))
ccf <- cf %>% group_by(Replicate) %>% summarise(nn = sum(n))
cf <- left_join(cf, ccf) %>% 
  filter(!is.na(mcf)) %>%
  select(-has_brant, -n) %>%
  rename(n = nn)
rm(ccf)
#plot ratio
ggplot(data = drop_na(df), aes(x = manual_density, y = yolo_density, col = Replicate)) + 
  geom_point() + 
  geom_smooth(formula = y ~ x, method = "lm") + 
  geom_abline(intercept = 0, slope = 1)
summary(lm(yolo_density~manual_density, data = drop_na(df)))
df <- left_join(df, cf) %>% mutate(logmcf = log(mcf), Density = yolo_density*10000, 
                                   PhotoArea = area)

#Calculate design-based estimate, this needs to be stratified for the correct variance calculation
df |> group_by(Replicate) %>%
  mutate(n_yolo = n()) %>%
  summarize(mu = mean(yolo_density*10000, na.rm = TRUE), 
            #mu2 = sum(yolo_count)/(sum(PhotoArea)/10000), 
            #sigma = sd(manual_density*10000, na.rm = TRUE), 
            sigma2 = sd(yolo_density*10000, na.rm = TRUE),
            a = sum(PhotoArea)/10000,
            f = a/lagoonha,
            n_yolo = n(), #number of yolo photos
            n = mean(n), #number of dennis validated photos
            cf = mean(mcf),
            cf_se = mean(cf_se),
            N_hat = lagoonha*mu*cf,
            sd_N_yolo = sqrt(lagoonha^2 * (1 - f) * (sigma2^2)/n_yolo), #this should be sigma if done correctly
            sd_N_cf = sqrt(lagoonha^2 * (1 - n/n_yolo) * cf_se),
            sd_N = sqrt(sd_N_yolo^2 + sd_N_cf^2),
            lower = N_hat - 1.96*sd_N, 
            upper = N_hat + 1.96*sd_N) %>%
  select(-cf_se)
#fit gam, use correction factor as offset
fit.22 <- gam(Density ~ s(X, Y, by = Replicate, k = c(200)), offset = logmcf, 
              family = tw, method = "REML", data = df)
summary(fit.22)
gam.check(fit.22)
library(DHARMa)
simulationOutput <- simulateResiduals(fittedModel = fit.22, n = 1000, plot = F)
plot(simulationOutput)
testZeroInflation(simulationOutput) #not looking so good.
plotQQunif(simulationOutput) #significant dispersion test but effect look small
plotResiduals(simulationOutput) #not sure how to interpret that
#########make plots and estimates
par(mfrow=c(2,2))
for(i in c(1,2,3,4)){
  vis.gam(fit.22, plot.type = "contour", cond = list(Replicate = i), too.far = 0.1, 
          main = paste0("Replicate = ", i))}
par(mfrow= c(1,1))
#Make a prediction over whole lagoon
#Density is per sq ha, see above
grid <- lag  |> 
  st_make_grid(cellsize = 100) |> #c(83.1, 54.8)) |>
  st_intersection(lag)
plot(st_geometry(grid), pch = ".")
cen <- st_centroid(grid)
area <- st_area(grid) |> units::drop_units()
pred_df <- as.data.frame(st_coordinates(cen)) |> mutate(Area = area/10000, Replicate = 1)
#Make prediction data frame
newdata <- rbind(pred_df, mutate(pred_df, Replicate = 2)) |>
  rbind(mutate(pred_df, Replicate = 3)) |>
  rbind(mutate(pred_df, Replicate = 4)) |>
  rbind(mutate(pred_df, Replicate = 5))
preds <- predict(fit.22, newdata = newdata, type = "response")
grid2 <- st_as_sf(grid) |> slice(rep(row_number(), 5))
st_geometry(grid2) <- "geometry"
plot_df <-  mutate(newdata, Prediction = preds, Ex_Count = Area*Prediction) |>
  cbind(grid2) |>
  st_as_sf()
for(i in 1:5){
  gg <- ggplot(data = filter(plot_df, Replicate == i)) + geom_sf(aes(fill=Ex_Count), col = NA) +
    scale_fill_viridis_c(name = "Expected \n Count") + 
    geom_sf(data = filter(plot_df, Replicate == i, Ex_Count < 1), col = "darkgray", fill = "darkgray") +
    labs(title = paste0("Replicate = ", i))
  print(gg)
}
#calculate total
st_drop_geometry(plot_df) |> group_by(Replicate) |> 
  summarise(Total = sum(Ex_Count))
total_post <- map_post(gamfit = fit.22, newdata = newdata, Nsamples = 1000)
apply(total_post, 2, sd)
apply(total_post, 2, quantile, probs = c(0.025, 0.5, 0.975)) #looks good
#####How about an average over all five reps"
fit.22.ave <- gam(Density ~ s(X, Y, k = c(400)), offset = logmcf, 
                  family = tw, method = "REML", data = df)
summary(fit.22.ave)
preds <- predict(fit.22.ave, newdata = pred_df, type = "response")
plot_df <-  mutate(pred_df, Prediction = preds, Ex_Count = Area*Prediction) |>
  cbind(grid) |>
  st_as_sf()
ggplot(data = plot_df) + geom_sf(aes(fill=Ex_Count), col = NA) +
  scale_fill_viridis_c(name = "Expected \n Count") + 
  geom_sf(data = filter(plot_df, Ex_Count < 1), col = "darkgray", fill = "darkgray") +
  labs(title = paste0("All Replicates"))
#population estimate 
st_drop_geometry(plot_df) |> 
  summarise(Total = sum(Ex_Count))
total_post <- map_post(gamfit = fit.22.ave, newdata = pred_df, Nsamples = 1000)
apply(total_post, 2, sd)
apply(total_post, 2, quantile, probs = c(0.025, 0.5, 0.975))
AIC(fit.22.ave, fit.22)
################################################################################
#back to glmmTMB and try a guassian process model
#start with 2018 as this year seems to have the least issues
library(glmmTMB)
#####2018#####
# just use one replicate for dev. 
df <- dat %>% filter(Year == 2018, Replicate == 1) %>%
  mutate(Replicate = factor(Replicate)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs=4326) %>%
  st_transform(crs = 3338)
df <- cbind(st_drop_geometry(df), st_coordinates(df))
df$pos <- numFactor(scale(df$X/1000, scale = FALSE), scale(df$Y/1000, scale = FALSE))
df$group <- factor(rep(1, dim(df)[1]))
#model as fit by Weiser et al. 2022
fit <- glmmTMB(Density ~ (1|Replicate), ziformula=~1, dispformula=~1,
               data=dat, family="gaussian")
#now add spatial correlation
fit.gau <- glmmTMB(Density ~ 1 + gau(pos + 0 | Replicate), data=df, ziformula=~1, dispformula=~1)
#try a small subset of data
set.seed(123)
fit.gau <- glmmTMB(Density ~ gau(pos + 0 | Replicate), data=df[sample(1:dim(df)[1], 1000),],
                   ziformula=~1, dispformula=~1)
#this took a very long time for only 1000 data points! then ended with convergence/error problem.
##Try spaMM
library(spaMM)
df$logArea <- log(df$PhotoArea)
#should we add ~60m to one camera so that that do not have the same location?
df <- mutate(df, newX = if_else(Camera == "CAM1", (X - mean(X))/1000, (X + 60 - mean(X))/1000), 
             newY = if_else(Camera == "CAM1", (Y - mean(Y))/1000, (Y + 60 - mean(Y))/1000))
m_spamm <- fitme(Count ~ 1 + Matern(1 | newX + newY) + offset(logArea), data = df, family = "negbin2", 
                 control.HLfit=list(NbThreads=4, algebra = "decorr"))
# (One-time message:) Choosing matrix methods took 31.1 s.
# If you perform many similarly costly fits, setting the method
# by control.HLfit=list(algebra=<"spprec"|"spcorr"|"decorr">) may be useful,
# see help("algebra"). "decorr" has been selected here.
# Using paralellisation might be useful. See help("setNbThreads")
#
#Also got this:
# Error in eval(mc, parent.frame()) : std::bad_alloc
# Error: no more error handlers available (recursive errors?); invoking 'abort' restart
#
# Seems like a memory problem. Not sure what to do. 
################################################################################
##try a random Forest model
library(randomForest)
ranFor <- randomForest(formula=Density~X+Y+Replicate, data = df)
preds <- predict(ranFor, newdata = pred_df, type = "response")
plot_df <-  mutate(pred_df, Prediction = preds, Ex_Count = Area*Prediction) |>
  cbind(grid) |>
  st_as_sf()
ggplot(data = plot_df) + geom_sf(aes(fill=Ex_Count), col = NA) +
  scale_fill_viridis_c(name = "Expected \n Count") +
  geom_sf(data = filter(plot_df, Ex_Count < 1), col = "darkgray", fill = "darkgray") +
  labs(title = paste0("All Replicates"))
#that doesn't look so good
#population estimate 
st_drop_geometry(plot_df) |> 
  summarise(Total = sum(Ex_Count))
################################################################################
#Back to mgcv::gam and try a Gaussian process smooth
df <- dat %>% filter(Year == 2019, Replicate == 1) %>%
  mutate(Replicate = factor(Replicate)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs=4326) %>%
  st_transform(crs = 3338)
df <- cbind(st_drop_geometry(df), st_coordinates(df))

fit.gp <- gam(Density ~ s(X, Y, bs = "gp", k = 100, m = c(3)), family = tw, 
              method = "REML", data = df) #Matern with default range
summary(fit.gp)
gam.check(fit.gp)
plot(fit.gp)
vis.gam(fit.gp, type = "link", plot.type = "contour", too.far = 0.05)
#that doesn't really look all that good either. 