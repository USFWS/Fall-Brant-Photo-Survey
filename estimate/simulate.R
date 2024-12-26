#simulate an N-mixture model for fall brant photo survey with classification error
#Erik Osnas 
#
################################################################################
sim_obs <- function(
#Set up true state process
sample.size = c(1000, 100), #vector to define primary sample size and secondary, etc...
#Observation process parameters
r = c(1, 1, 1, 1), #detection probability per individual
theta = matrix(c(1, 0, 0, 0, 
                  0, 1, 0, 0, 
                  0, 0, 1, 0, 
                  0, 0, 0, 1), 4,4, byrow = TRUE),
theta2 = NULL,
#theta is a confusion matrix, this is the misclassification rates, 
#  true state are rows, classification state are columns
#  theta2 is the confusion matrix for the secondary sample, if null, then use assume no error
A = 100000, #survey area in ha
Photo_a = 1, #area of one photo in ha
lambda = c(1, 1, 1, 1), #density of brant, bird2, bird3, and other respectively,
#per ha in occupied photos;
psi = rep(1, 4) #c(0.05, 0.01, 0.01, 0.01) #proportion of photos with each class
#expected population size in survey area
# A*psi*lambda
){
Photo_n <- A/Photo_a #number of non-overlapping photos possible in whole survey area
#simulate some states and observed data
#true numbers per photo
z <- rbinom(length(psi)*Photo_n, size = 1, prob = rep(psi, each = Photo_n))
N <- matrix(rpois(length(lambda)*Photo_n, rep(lambda, each = Photo_n)*z), Photo_n, 4)
#true population size
# colSums(N)
################################################################################
#Observed states, N%*%theta
# misclassification matrix, rows are true state, col are observed
# theta <- matrix(c(0.97, 0.01, 0.01, 0.01, 
#                   0.01, 0.98,    0, 0.01, 
#                   0.01,    0, 0.98, 0.01, 
#                   0.01, 0.01, 0.01, 0.97), 4,4, byrow = TRUE) 
# theta <- matrix(c(0.7, 0.1, 0.1, 0.1, 
#                   0.1, 0.7,    0.1, 0.1, 
#                   0.1,    0, 0.8, 0.1, 
#                   0.1, 0.1, 0.3, 0.5), 4,4, byrow = TRUE)

#check that rows sum to 1
if (sum(rowSums(theta)) != dim(theta)[1]) 
  warning("Rows of the misclassification matrix do not add to one!")

n.occ <- length(sample.size) #number of sampling occasions
Y <- list()
Y$Observed <- list()
samples1 <- sample(1:dim(N)[1] , sample.size[1]) #primary samples
for(j in 1:n.occ){
  if(j == 2 ){ samples2 <- sample(samples1 , sample.size[j]) } #secondary sample
  if(j == 1) samples <- samples1
  if(j > 1) samples <- samples2
  #detection process
  #detection is the Royal-Nichols individual detection model for heterogeneity
  # p_i = 1-(1-r)^N_i
  Yd <- matrix(rbinom(sample.size[j]*dim(N)[2], size = as.vector(N[samples,]), 
                      prob = 1-(1-rep(r, each = sample.size[j]))^as.vector(N[samples,])),
               nrow = sample.size[j], ncol = dim(N)[2]) 
  #This is 'completely independent' detection across primary ans secondary samples
  #colSums(Yd)
  #classification process
  Y$Observed[[j]] <- matrix(NA, sample.size[j], length(lambda))
  for ( i in 1: sample.size[j]){
    if(j == 1 | is.null(theta2)){
    Y$Observed[[j]][i,] <- rmultinom(1, size = Yd[i,1], prob = theta[1,]) + 
      rmultinom(1, size = Yd[i,2], prob = theta[2,]) +
      rmultinom(1, size = Yd[i,3], prob = theta[3,]) +
      rmultinom(1, size = Yd[i,4], prob = theta[4,])
    }
    if( j > 1 & !is.null(theta2) ){
      Y$Observed[[j]][i,] <- rmultinom(1, size = Yd[i,1], prob = theta2[1,]) + 
        rmultinom(1, size = Yd[i,2], prob = theta2[2,]) +
        rmultinom(1, size = Yd[i,3], prob = theta2[3,]) +
        rmultinom(1, size = Yd[i,4], prob = theta2[4,])
    }
  }
}
Y$Samples <- list(primary=samples1, secondary=samples2)
Y$Truth <- list(Total = colSums(N), primary = N[samples1,], secondary = N[samples2,])
Y$Expected <- A*psi*lambda
return(Y)
}
#test it
sim_obs(sample.size=c(10, 5), lambda = c(10, 1, 0, 0))
m <- matrix(c(0.7, 0.1, 0.1, 0.1,
                  0.1, 0.7,    0.1, 0.1,
                  0.1,    0, 0.8, 0.1,
                  0.1, 0.1, 0.3, 0.5), 4,4, byrow = TRUE)
sim_obs(sample.size=c(10, 5), theta = m, lambda = c(10, 1, 0, 0))

