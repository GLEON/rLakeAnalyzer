# ---Author: Jordan Read, 2013-07-26 ---
# translated from Matlab Script - Author Jordan Read, 2013 
## from layertemperature.m in https://github.com/GLEON/Lake-Analyzer/
#
# top: surface to top of layer
# bottom: surface to bottom of layer
# wtr: water temperature in celsius  
# depths: water depth values in meters 
# bthA: bathymetry area in meters squared 
# bthD: bathymetry depths in meters 
#
# OUTPUT: returns the average average water density of lake layer (i.e. average epilimnion density)

layer.temperature <- function(top, bottom, wtr, depths, bthA, bthD){
  
  # checking input quality 
  if(top>bottom){
    stop('bottom depth must be greater than top')
  }else if(length(wtr)!=length(depths)){
    stop('water temperature vector must be same length as depth vector')
  }else if(length(as.list(match.call()))<4){
    stop('not enough input arguments')
  }else if(any(is.na(wtr),is.na(depths),is.na(bthA),is.na(bthD))){
    stop('input arguments must be numbers')
  }
  
  # if bathymetry has negative values, interpolate to 0 
  if(min(bthD)<0){
    useI <- bthD>=0
    if(!any(bthD==0)){
      depT <- c(0,bthD[useI])
    }else{
      depT <- bthD[useI]
    }
    bthA <- approx(bthD,bthA,depT)$y
    bthD <- depT
  }
  
  dz <- 0.1 #(meters)
  
  numD <- length(wtr)
  if(depths[numD] < bottom){
    wtr[numD+1] <- wtr[numD]
    depths[numD+1] <- bottom
    numD = numD+1
  }
  if(max(bthD) < bottom){
    bthD <- c(bthD,bottom)
    bthA <- c(bthA,0)
  }
  if(min(bthD) < depths[1]){
    wtr <- c(wtr[1],wtr)
    depths <- c(min(bthD),depths)
  }
  
  Io <- which.min(depths)
  Ao <- bthA[Io]
  if(Ao==0){
    stop('surface area cannot be zero, check bathymetry file')
  }
  
  # iterpolate the bathymetry data 
  layerD <- seq(top,bottom,dz)
  layerT <- approx(depths,wtr,layerD)$y
  layerA <- approx(bthD,bthA,layerD)$y
  
  weightedT <- layerA*layerT*dz
  
  aveTemperature <- sum(weightedT)/(sum(layerA))/dz
  return(aveTemperature)  
}
