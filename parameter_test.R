## create parameters to test quantity_allocation_disagreement function for binary predictions
## this example is to reproduce the results in Death to Kappa
source("disagreement.R")
reference <- raster(matrix(c(0,0,0,0,0,0,1,1,1), ncol=3, nrow=3))
comp <- raster(matrix(0,ncol=3, nrow=3))
comp2 <- raster(matrix(c(0,0,0,0,0,0,1,0,0), ncol=3, nrow=3))
comp3 <- raster(matrix(c(0,0,1,0,0,0,0,0,0), ncol=3, nrow=3))
comp4 <- raster(matrix(c(0,0,0,0,0,0,1,1,0), ncol=3, nrow=3))
comp5 <- raster(matrix(c(0,0,1,0,0,0,1,0,0), ncol=3, nrow=3))
comp6 <- raster(matrix(c(0,1,1,0,0,0,0,0,0), ncol=3, nrow=3))
comp7 <- raster(matrix(c(0,0,0,0,0,0,1,1,1), ncol=3, nrow=3))
comp8 <- raster(matrix(c(0,0,1,0,0,0,1,1,0), ncol=3, nrow=3))
comp9 <- raster(matrix(c(0,1,1,0,0,0,1,0,0), ncol=3, nrow=3))
comp10 <- raster(matrix(c(1,1,1,0,0,0,0,0,0), ncol=3, nrow=3))
comp11 <- raster(matrix(c(0,0,0,1,0,0,1,1,1), ncol=3, nrow=3))
comp12 <- raster(matrix(c(0,0,1,1,0,0,1,1,0), ncol=3, nrow=3))
comp13 <- raster(matrix(c(0,1,1,1,0,0,1,0,0), ncol=3, nrow=3))
comp14 <- raster(matrix(c(1,1,1,1,0,0,0,0,0), ncol=3, nrow=3))
comp15 <- raster(matrix(c(0,0,0,1,1,0,1,1,1), ncol=3, nrow=3))
comp16 <- raster(matrix(c(0,0,1,1,1,0,1,1,0), ncol=3, nrow=3))
comp17 <- raster(matrix(c(0,1,1,1,1,0,1,0,0), ncol=3, nrow=3))
comp18 <- raster(matrix(c(1,1,1,1,1,0,0,0,0), ncol=3, nrow=3))
comp19 <- raster(matrix(c(0,0,0,1,1,1,1,1,1), ncol=3, nrow=3))
comp20 <- raster(matrix(c(0,0,1,1,1,1,1,1,0), ncol=3, nrow=3))
comp21 <- raster(matrix(c(0,1,1,1,1,1,1,0,0), ncol=3, nrow=3))
comp22 <- raster(matrix(c(1,1,1,1,1,1,0,0,0), ncol=3, nrow=3))
comp23 <- raster(matrix(c(0,0,1,1,1,1,1,1,1), ncol=3, nrow=3))
comp24 <- raster(matrix(c(1,0,1,1,1,1,1,1,0), ncol=3, nrow=3))
comp25 <- raster(matrix(c(1,1,1,1,1,1,1,0,0), ncol=3, nrow=3))
comp26 <- raster(matrix(c(0,1,1,1,1,1,1,1,1), ncol=3, nrow=3))
comp27 <- raster(matrix(c(1,1,1,1,1,1,1,1,0), ncol=3, nrow=3))
comp28 <- raster(matrix(c(1,1,1,1,1,1,1,1,1), ncol=3, nrow=3))

comps <- list(comp, comp2, comp3, comp4, comp5, comp6, comp7, comp8, comp9, comp10, comp11, comp12, comp13, comp14, comp15, comp16, comp17, comp18, comp19, comp20, comp21, comp22, comp23, comp24, comp25, comp26, comp27, comp28)
comparison <- comps[[1]]
data <- list(quantity_allocation_disagreement(reference = reference, comparison = comparison))
for (i in 2:length(comps)){
  comparison <- comps[[i]]
  data[[i]] <- quantity_allocation_disagreement(reference = reference, comparison = comparison)
  print(i)
}

plot(data$number_of_infected_comp, data$total_disagreement)
