# ---Author: Jake Zwart, 2013-04-21 ---
# translated from Matlab Script - Author Jordan Read, 2009 
## from layerDensity.m in https://github.com/jread-usgs/Lake-Analyzer/
#
# top: surface to top of layer
# bottom: surface to bottom of layer
# wtr: water temperature in celsius  
# depths: water depth values in meters 
# bthA: bathymetry area in meters squared 
# bthD: bathymetry depths in meters 
# sal: salinity in Pratical Salinity Scale units (dimensionless)
#
# OUTPUT: returns the average average water density of lake layer (i.e. average epilimnion density)

layerDensity <- function(top,bottom,wtr,depths,bthA,bthD,sal){
  
  # find the number of input arguments
  nargin <- length(as.list(match.call())) -1  
  
  # If number of input arguments is equal to 6, assume salinity is 0.
  if (nargin<7){
    sal <- wtr*0
  }
  
  # checking input quality 
  if(top>bottom){
    stop('bottom depth must be greater than top')
  }else if(length(wtr)!=length(depths)){
    stop('water temperature vector must be same length as depth vector')
  }else if(nargin<4){
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
  if(max(bthD)>depths[numD]){
    wtr[numD+1] <- wtr[numD]
    sal[numD+1] <- sal[numD]
    depths[numD+1] <- max(bthD)
  }else if(max(bthD)<depths[numD]){
    bthD <- c(bthD,depths[numD])
    bthA <- c(bthA,0)
  }
  if(min(bthD)<depths[1]){
    wtr <- c(wtr[1],wtr)
    sal <- c(sal[1],sal)
    depths <- c(min(bthD),depths)
  }
  
  Io <- grep(min(depths),depths)
  Ao <- bthA[Io]
  if(Ao[1]==0){
    stop('surface area cannot be zero, check bathymetry file')
  }
  
  # iterpolate the bathymetry data 
  layerD <- seq(top,bottom,dz)
  layerT <- approx(depths,wtr,layerD)$y
  layerS <- approx(depths,sal,layerD)$y
  layerA <- approx(bthD,bthA,layerD)$y
  layerP <- waterDensity(layerT,layerS)
  
  mass <- layerA*layerP*dz
  aveDensity <- sum(mass)/(sum(layerA))/dz
  return(aveDensity)  
}
