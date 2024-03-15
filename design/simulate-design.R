#simulate 2023 design and estimators to study bias and precision

library(tidyverse)
library(sf)
library(dsims)

#first read in lagoon and survey area polygons
temp <- tempfile()
unzip(zipfile = "Izembek_shapefile.zip", exdir = temp)
lag <- read_sf(dsn=temp)
lag3 <- st_boundary(st_union(lag)) %>% 
  st_cast("POLYGON")
#add buffer
lag4 <- st_buffer(lag3, dist = 500)
#should d0 hull over buffered area 
lag7 <- st_concave_hull(lag4, ratio = 0.2, allow_holes = FALSE)
plot(st_geometry(lag7))
plot(st_geometry(lag3), add = TRUE)

#sample hotspots
hotspots <- st_sample(lag7, 10)
plot(hotspots, pch = 16, add = TRUE)

#set up sims
region <- make.region(region.name = "Izembek Lagoon",
                      shape = lag7,
                      units = "m")
parallel.design <- make.design(region = region, 
                               design = "systematic",
                               spacing = 1250,
                               edge.protocol = "minus",
                               design.angle = 135,
                               truncation = 83)
p.survey <- generate.transects(parallel.design)
plot(region, p.survey)
density <- make.density(region = region, x.space = 1000, constant = 0.1)
for(i in 1:length(hotspots)){
  density <- add.hotspot(object = density,
                         centre = st_coordinates(hotspots)[i,],
                         sigma = 1000, #1000, 2500, 10000
                         amplitude = 1000)
}
plot(density, region)
#define population
#covariates <- list(size = list(distribution = "ztruncpois", mean = 100))
# Define the population description
pop.desc <- make.population.description(region = region,
                                        density = density,
                                        covariates = list(),
                                        N = 2500, #large population causes longer time, 10K = 29 sec, 1K 0.5 sec
                                        fixed.N = TRUE)
# Define the covariate parameters on the log scale
cov.param <- list(size = log(1))
# Create the detectability description
detect <- make.detectability(key.function = "uf",
                             scale.param = 1,
                             cov.param = list(),
                             truncation = 83)
# Plot the simulation detection functions
plot(detect, pop.desc)
#define analysis
analyses <- make.ds.analysis(dfmodel = list(~1),
                             key = c("hn"),
                             truncation = 83,
                             er.var = "R2")
#set up sims
sim.parallel <- make.simulation(reps = 100,
                                design = parallel.design,
                                population.description = pop.desc,
                                detectability = detect,
                                ds.analysis = analyses)
# Generate a single instance of a survey: a population, set of transects 
# and the resulting distance data
now <- Sys.time()
eg.parallel.survey <- run.survey(sim.parallel)
Sys.time() - now
# Plot it to view a summary
plot(eg.parallel.survey, region)
# Running the simulations
sim.parallel <- run.simulation(sim.parallel)
summary(sim.parallel)
histogram.N.ests(sim.parallel)
#loop through number of clusters and sigma (cluster dispersion)
sigma <- c(1000, 2500, 10000)
flocks <- c(1, 2, 4, 8, 16)
df <- data.frame(NULL) #data frame to store results
for(f in flocks){
  #sample hotspots
  hotspots <- st_sample(lag7, f)
  for(s in sigma){
  density <- make.density(region = region, x.space = 1000, constant = 0.1)
  for(i in 1:length(hotspots)){
    density <- add.hotspot(object = density,
                           centre = st_coordinates(hotspots)[i,],
                           sigma = s, 
                           amplitude = 1000)
  }
  pop.desc <- make.population.description(region = region,
                                          density = density,
                                          covariates = list(),
                                          N = 5000, #large population causes longer time, 10K = 29 sec, 1K 0.5 sec
                                          fixed.N = TRUE)
  #set up sims
  sim.parallel <- make.simulation(reps = 300,
                                  design = parallel.design,
                                  population.description = pop.desc,
                                  detectability = detect,
                                  ds.analysis = analyses)
  # Running the simulations
  sim.parallel <- run.simulation(sim.parallel)
  results <- summary(sim.parallel)@individuals$N |>
    mutate(Sigma = s, FlockNum = f)
  df <- rbind(df, results)
  }}
saveRDS(df, file = "df.RDS")
df <- readRDS(file = "df.RDS")
#plot
df <- mutate(df, Sigma = factor(Sigma), FlockNum = factor(FlockNum))
#RMSE
ggplot(data = df, aes(x=FlockNum, y = RMSE, fill = Sigma)) + 
  geom_bar(stat = "identity", position = "dodge")
# % bias
ggplot(data = df, aes(x=FlockNum, y = percent.bias, fill = Sigma)) + 
  geom_bar(stat = "identity", position = "dodge")
# mean.se
ggplot(data = df, aes(x=FlockNum, y = mean.se, fill = Sigma)) + 
  geom_bar(stat = "identity", position = "dodge")
# precision
ggplot(data = df, aes(x=FlockNum, y = sd.of.means, fill = Sigma)) + 
  geom_bar(stat = "identity", position = "dodge")
# estimate
ggplot(data = df, aes(x=FlockNum, y = mean.Estimate, fill = Sigma)) + 
  geom_bar(stat = "identity", position = "dodge") + geom_hline(yintercept = 5000)
