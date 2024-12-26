#function to estimate population size from primary and secondary samples

estimate1 <- function(y1 = NULL, y2 = NULL, areas = rep(1, length(y1)), total.area = NULL){
  #take a primary and secondary sample for multiple species and 
  # returns an estimate of the total for each species
  # if both y1 and y2 have samples, a double sample estimate is returned,
  #  else a simple single sample estimate is returned based on SRS of plots
  # 
  if(is.null(y1)) stop("Nothing to estimate.")
  
  if(is.null(y2)){
    x <- mean(y1/areas)
    r <- NULL
    sdy <- NULL
    sdx <- sd(y1/areas)
    total <- x*total.area
    sd.total <- sqrt( total.area^2 * (1 - sum(areas)/total.area) * 
                        (sdx^2)/length(y1) )
  } else{
    if( length(y1) != length(y2) ) stop("sample vectors not equal length")
    r <- sum(y1[ !is.na(y2) ])/sum(y2, na.rm = TRUE)
    x <- NULL
    total <- r*total.area*mean(y1)
    sdx <- sd(y1/areas)
    sdy <- sd(y2[ !is.na(y2) ] - r*y1[ !is.na(y2) ])
    sd.total <- sqrt(
      total.area^2 * (1 - sum(areas)/total.area) * (sdx^2)/length(y1) + 
      total.area^2 * (1 - length(y2[ !is.na(y2) ])/length(y1)) * 
      (sdy^2)/length(y2[ !is.na(y2) ]) )
  }
  return(list(x = x, r = r, sdx = sdx, sdy = sdy, total = total, sd.total = sd.total))
}
#test it
#first one SRS
# y = rpois(5000, 1)
# estimate1
# now a double sample
# s <- sample(1:length(y), 100)
# y2 <- rep(NA, length(y))
# y2[s] <- y[s]*(0.7 + rnorm(length(s), 0, 0.05))
# estimate1(y1 = y, y2 = y2, total.area = 10000)
