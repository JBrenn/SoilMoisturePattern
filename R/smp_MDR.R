smp_MRD <- function(data)
{
  # spatial average SMC
  spatialMean <- rowMeans(data, na.rm=TRUE)
  
  # relative differences
  RDij <- apply(data, 2, function(x) (x - spatialMean) / spatialMean)
  
  # mean relative differences MRD
  MRD <- colMeans(RDij, na.rm=TRUE)
  
  MRDij <- rbind(MRD, RDij)
  
  # standard deviation of relative differences
  SDRD <- apply(MRDij, 2, function(x) {
    nr_obs <- sum(!is.na(x[-1]))
    sqrt( 1/(nr_obs-1) * sum((x[-1]-x[1])^2, na.rm=TRUE))
  } )  
  
  #  RMSE <- sqrt(MRD^2 + SDRD^2)
  
  return(list(MRD=MRD, SDRD=SDRD))
} 