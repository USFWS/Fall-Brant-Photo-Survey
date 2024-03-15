#simulate an N-mixture model for fall brant photo survey with classification error
#Erik Osnas 
#library(unmarked)
################################################################################
#Set up true state process
A <- 100000 #survey area in ha
Photo_a <- 1 #area of one photo in ha
Photo_n <- A/Photo_a #number of non-overlapping photos possible in whole survey area
Photo_n
lambda <- c(10, 10, 10, 10) #density of brant, bird2, bird3, and other respectively,
#per ha in occupied photos;
psi <- rep(0.5, 4) #c(0.05, 0.01, 0.01, 0.01) #proportion of photos with each class
#expected population size in survey area
A*psi*lambda


#simulate some states and observed data
#true numbers per photo
z <- rbinom(length(psi)*Photo_n, size = 1, prob = rep(psi, each = Photo_n))
N <- matrix(rpois(length(lambda)*Photo_n, rep(lambda, each = Photo_n)*z), Photo_n, 4)
#true population size
colSums(N)
################################################################################
#Observed states, N%*%theta
#Observation process parameters
r <- c(0.1, 0.1, 0.1, 0.1) #detection probability per individual
# misclassification matrix, rows are true state, col are observed
# theta <- matrix(c(0.97, 0.01, 0.01, 0.01, 
#                   0.01, 0.98,    0, 0.01, 
#                   0.01,    0, 0.98, 0.01, 
#                   0.01, 0.01, 0.01, 0.97), 4,4, byrow = TRUE) 
# theta <- matrix(c(0.7, 0.1, 0.1, 0.1, 
#                   0.1, 0.7,    0.1, 0.1, 
#                   0.1,    0, 0.8, 0.1, 
#                   0.1, 0.1, 0.3, 0.5), 4,4, byrow = TRUE)
theta <- matrix(c(1, 0, 0, 0, 
                  0, 1, 0, 0, 
                  0, 0, 1, 0, 
                  0, 0, 0, 1), 4,4, byrow = TRUE)
#check that rows sum to 1
if (sum(rowSums(theta)) != dim(theta)[1]) 
  warning("Rows of the misclassification matrix do not add to one!")

sample.size <- 10
n.occ <- 3 #number of sampling occassions
samples <- sample(1:dim(N)[1] , sample.size)
Y <- list()
for(j in 1:n.occ){
  #detection process
  #detection is the Royal-Nichols individual detection model for heterogeneity
  # p_i = 1-(1-r)^N_i
  Yd <- matrix(rbinom(sample.size*dim(N)[2], size = as.vector(N[samples,]), 
                      prob = 1-(1-rep(r, each = sample.size))^as.vector(N[samples,])),
               nrow = sample.size, ncol = dim(N)[2]) 
  colSums(Yd)
  #classification process
  Y[[j]] <- matrix(NA, sample.size, length(lambda))
  for ( i in 1: sample.size){
    Y[[j]][i,] <- rmultinom(1, size = Yd[i,1], prob = theta[1,]) + 
      rmultinom(1, size = Yd[i,2], prob = theta[2,]) +
      rmultinom(1, size = Yd[i,3], prob = theta[3,]) +
      rmultinom(1, size = Yd[i,4], prob = theta[4,])
  }
}

# colSums(Y)
# N[samples,]
# Yd
# Y
# #checks
# N[samples,] - Yd #should all be >=0
################################################################################
#fit model
dat <- unmarkedFramePCount(y = matrix(c(y1, y2), byrow=FALSE))
fit <- pcount(formula = ~1 ~1, dat, K = 20, mixture = "ZIP")
summary(fit)
c(exp(coef(fit)[1]), plogis(coef(fit)[2:3]))
