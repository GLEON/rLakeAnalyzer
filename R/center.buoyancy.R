#
# Center of Buoyancy Frequency
# Author: Jordan S Read <jread@usgs.gov>
#
center.buoyancy <- function(wtr, depths){
  
  if (depths[2] - depths[1] < 0 ){stop('depths must be in descending order')}
  N2 <- buoyancy.freq(wtr, depths)
  num.slices <- length(N2)
  areas <- vector('numeric',length = num.slices)
  cent.depths <- vector('numeric',length = num.slices)
  
  for (i in 1:num.slices){
    dz <- depths[i+1] - depths[i]
    areas[i] <- dz * N2[i] # assumes depths are in descending order
    cent.depths[i] <- mean(depths[i:(i+1)])
  }
  
  cent.buoyancy <- sum(cent.depths*areas)/sum(areas)
  
  return(cent.buoyancy)
}


ts.center.buoyancy <- function(wtr){
  
  depths = get.offsets(wtr)
  
  n = nrow(wtr)
  
  wtr.mat = as.matrix(wtr[,-1])
  cent.n2 = rep(NA, n)
    
  for(i in 1:n){
    cent.n2 <- center.buoyancy(wtr.mat[i, ], depths)
  }
  
  cent.buoyancy = data.frame(wtr[,'datetime', drop=F], cent.n2)
  
  return(cent.buoyancy)
}