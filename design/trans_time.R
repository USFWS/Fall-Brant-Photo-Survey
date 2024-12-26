#readtransects
trans <- st_read(dsn = "design2023/IZF_Panel1A.kml") |>
  rbind(st_read(dsn = "design2023/IZF_Panel1B.kml")) |>
  st_transform(crs=4326)
#filter out point in transects
trans <- filter(trans, str_length(Name) < 6)
#need to run design2023.code to find lagoon polygons below:
lag7 <- st_transform(lag7, crs=4326)
lag3 <- st_transform(lag3, crs=4326)
tm_shape(lag3) + tm_polygons(col = "red", col_alpha = 0.5) + 
  tm_shape(lag7) + tm_polygons(col = "green", col_alpha = 0.5) + 
  tm_shape(trans) + tm_lines(col = "green", col_alpha = 0.5) + 
  tm_basemap(server = "Esri.WorldImagery")
#clip to design:
trans2 <- st_intersection(lag7, trans)
plot(st_geometry(lag7))
plot(st_geometry(trans2), add=TRUE)
#find length of transects outside of laggon polygon
out.trans <- st_difference(trans2, lag3)
plot(st_geometry(lag7))
plot(st_geometry(out.trans), add=TRUE)
len.out <- sum(st_length(out.trans))
#percent of transect in out:
len.out/sum(st_length(trans2))
#total flight time for out segements:
library(units)
len.out/set_units(100/1.944, "m/s")/set_units(60, "s")
#total flight time
sum(st_length(trans2))/set_units(100/1.944, "m/s")/set_units(60, "s")
#lead in lines
lead.in <- st_difference(trans, lag7)
plot(st_geometry(lag7))
plot(st_geometry(lead.in), add=TRUE)
#put it all together in map:
lag.trans <- st_intersection(lag3, trans2)
tm_shape(lag7) + tm_polygons(fill = "orange", fill_alpha = 0.5) + 
  tm_shape(lag3) + tm_polygons(fill = "purple", fill_alpha = 0.5) + 
  tm_shape(lead.in) + tm_lines(col = "red") + 
  tm_shape(out.trans) + tm_lines(col = "blue") +
  tm_shape(lag.trans) + tm_lines(col = "green") + 
  tm_basemap(server = "Esri.WorldImagery")
