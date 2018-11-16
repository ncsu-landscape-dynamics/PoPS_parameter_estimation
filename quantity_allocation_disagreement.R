library(rgdal)
library(raster)

quantity_allocation_disagreement <- function(reference, comparison, parameters=NULL){
  ## test that the comparison raster is the same extent, resolution, and crs as the reference (if not end)
  compareRaster(reference, comparison)
  compare <- reference - comparison
  compare2 <- as.matrix(compare)
  reference2 <- as.matrix(reference)
  comparison2 <- as.matrix(comparison)
  
  # class1 <- match(compare,-1)
  # class2 <- match(compare, 1)
  
  ## create data frame for comparison
  output <- data.frame(quantity_disagreement = 0, allocation_disagreement = 0, total_disagreement = 0 , omission = 0, commission = 0 ,number_of_infected_comp =0,total_allocation_distance = 0)
  output$total_disagreement <- sum(abs(compare2))
  output$quantity_disagreement <- abs(sum(reference2)-sum(comparison2))
  output$allocation_disagreement <- output$total_disagreement - output$quantity_disagreement
  output$omission <- abs(sum(compare2[compare2== -1]))
  output$commission <- abs(sum(compare2[compare2== 1]))
  output$number_of_infected_comp <- sum(comparison[comparison==1])
  
  ## compute distances to closest point of disagreement
  # dist <- gridDistance(compare, 1, omit=0)
  # dist[is.na(dist)] <- 0
  # dist <- as.matrix(dist)
  # output$total_allocation_distance = sum(dist)
  
  return(output)
}
