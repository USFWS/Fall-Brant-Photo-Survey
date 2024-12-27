#run simulations to look at bias and variance of estimators. Start simple
# first test a double observer with no mistakes in secondary
library(tidyverse)
source("estimate/simulate.R")
source("estimate/estimate.R")
theta1 = matrix(c(1, 0, 0, 0, 
                 0, 1, 0, 0, 
                 0, 0, 1, 0, 
                 0, 0, 0, 1), 4,4, byrow = TRUE)
theta2 = matrix(c(1, 0, 0, 0, 
                 0, 1, 0, 0, 
                 0, 0, 1, 0, 
                 0, 0, 0, 1), 4,4, byrow = TRUE)
nreps <- 100
result <- list(Primary = c(), Secondary = c(), total = c(), 
               sd = c(), true = c())
df <- data.frame(NULL)
size1 <- seq(1000, 100000, by = 1000)
for(j in size1){
  size2 <- seq(50, 50, by = 100)
  size2 <- size2[size2 < j]
  for(k in size2){
    for(i in 1:nreps){
      data <- sim_obs(sample.size=c(j, k), theta = theta1, theta2 = theta2, 
                      lambda = c(1, 1, 0, 0))
      #need to set up secondary sample vector
      m <- match(data$Samples[[2]], data$Samples[[1]])
      y <- rep(NA, length(data$Samples[[1]]))
      y[m] <- data$Observed[[2]][,1] 
      p <- estimate1(y1 = data$Observed[[1]][,1], y2 = y, total.area = 100000)
      result$total[i] <- p$total[1]
      result$sd[i] <- p$sd.total[1]
      result$true[i] <- data$Truth$Total[1]
      result$Primary[i] <- j
      result$Secondary[i] <- k
    }
    df <- rbind(df, as.data.frame(result))
  }}
#look at results
sumdf <- mutate(df, dev = total - true) |>
  group_by(Primary, Secondary) |>
  summarise(Bias = mean(dev), sdBias = sd(dev))

ggplot(data = sumdf, aes(x = Primary, y = Bias)) + 
  geom_line()
ggplot(data = sumdf, aes(x = Primary, y = sdBias)) + 
  geom_line()
### That seems to be working, now add miss classification error (false positives)
################################################################################
theta1 = matrix(c(1, 0, 0, 0, 
                  0.1, 0.9, 0, 0, #1% error of species 2 id'ed as spp1
                  0, 0, 1, 0, 
                  0, 0, 0, 1), 4,4, byrow = TRUE)
nreps <- 200
result <- list(Primary = c(), Secondary = c(), total = c(), 
               sd = c(), true = c())
df <- data.frame(NULL)
size1 <- seq(1000, 10001, by = 500)
for(j in size1){
  size2 <- seq(100, 1001, by = 100)
  size2 <- size2[size2 < j]
  for(k in size2){
    for(i in 1:nreps){
      data <- sim_obs(sample.size=c(j, k), theta = theta1, theta2 = theta2, 
                      lambda = c(1, 1, 0, 0))
      #need to set up secondary sample vector
      m <- match(data$Samples[[2]], data$Samples[[1]])
      y <- rep(NA, length(data$Samples[[1]]))
      y[m] <- data$Observed[[2]][,1] 
      p <- estimate1(y1 = data$Observed[[1]][,1], y2 = y, total.area = 100000)
      result$total[i] <- p$total[1]
      result$sd[i] <- p$sd.total[1]
      result$true[i] <- data$Truth$Total[1]
      result$Primary[i] <- j
      result$Secondary[i] <- k
    }
    df <- rbind(df, as.data.frame(result))
    print(paste0("Size1 = ", j, ", Size2 = ", k))
  }}
#look at results
sumdf <- mutate(df, dev = total - true) |>
  group_by(Primary, Secondary) |>
  summarise(Bias = mean(dev), SD = sd(dev))

ggplot(data = sumdf, aes(x = Primary, y = Bias, col = factor(Secondary))) + 
  geom_line()
ggplot(data = sumdf, aes(x = Primary, y = SD, col = factor(Secondary))) + 
  geom_line()
##looks good
## Now add non-detection in primary obs ########################################
#need to modify simulation function to allow difference in detection between 
#  observers. Each observer or occassion has a vector of detections
theta1 = matrix(c(1, 0, 0, 0, 
                  0.1, 0.9, 0, 0, #1% error of species 2 id'ed as spp1
                  0, 0, 1, 0, 
                  0, 0, 0, 1), 4,4, byrow = TRUE)
nreps <- 200
result <- list(Primary = c(), Secondary = c(), total = c(), 
               sd = c(), true = c())
df <- data.frame(NULL)
size1 <- seq(1000, 5001, by = 500)
for(j in size1){
  size2 <- seq(100, 1001, by = 200)
  size2 <- size2[size2 < j]
  for(k in size2){
    for(i in 1:nreps){
      data <- sim_obs(sample.size=c(j, k), theta = theta1, theta2 = theta2, 
                      lambda = c(1, 1, 0, 0), r = list(c(0.9, 1, 1, 1), c(0.9, 1, 1, 1)))
      #need to set up secondary sample vector
      m <- match(data$Samples[[2]], data$Samples[[1]])
      y <- rep(NA, length(data$Samples[[1]]))
      y[m] <- data$Observed[[2]][,1] 
      p <- estimate1(y1 = data$Observed[[1]][,1], y2 = y, total.area = 100000)
      result$total[i] <- p$total[1]
      result$sd[i] <- p$sd.total[1]
      result$true[i] <- data$Truth$Total[1]
      result$Primary[i] <- j
      result$Secondary[i] <- k
    }
    df <- rbind(df, as.data.frame(result))
    print(paste0("Size1 = ", j, ", Size2 = ", k))
  }}
#look at results
sumdf <- mutate(df, dev = total - true) |>
  group_by(Primary, Secondary) |>
  summarise(Bias = mean(dev), SD = sd(dev))

ggplot(data = sumdf, aes(x = Primary, y = Bias)) + 
  geom_line(aes(col = factor(Secondary))) + 
  geom_smooth(method = "gam", formula = y ~ s(x, k=3))
ggplot(data = sumdf, aes(x = Primary, y = SD, col = factor(Secondary))) + 
  geom_line()
#seems to work. Bias is zero with perfect detection for secondary and large and negative 
#  (~-4000) when both occasions have 0.9 detection rate. 