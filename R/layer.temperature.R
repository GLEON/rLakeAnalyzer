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



#' Returns the average temperature of a layer between two depths.
#' 
#' This function calculates the average temperature of a layer of water between
#' two depths.
#' 
#' 
#' @param top Numeric value of the depth (m) of the top of the layer from the
#' water surface
#' @param bottom Numeric value of the depth (m) of the bottom of the layer from
#' the water surface
#' @param wtr Numeric vector of water temperature in degrees C
#' @param depths Numeric vector of depths (m) corresponding to water
#' temperature vector
#' @param bthA Numeric vector of water body cross sectional area (m2)
#' corresponding to bthD depths
#' @param bthD Numeric vector of water body bathymetric depths (m)
#' corresponding to areal bthA values
#' @return Numeric value of average water temperature
#' @author Jordan Read
#' @seealso \code{layer.density}
#' @keywords manip
#' @examples
#' 
#' 	# Supply input data
#' 	top     <- 2
#' 	bottom  <- 6
#' 	wtr     <- c(25.2,25.1,24.1,22.0,19.8,15.3,12.0,11.1)
#' 	depths  <- c(0,1,2,3,4,5,6,7) 
#' 	bthA    <- c(10000,8900,5000,3500,2000,1000,300,10)
#' 	bthD    <- c(0,1,2,3,4,5,6,7)
#' 	
#' 	#Return the average temperature of the water column between 2 and 6 meters.
#' 	layer.temperature(top,bottom,wtr,depths,bthA,bthD)
#' @export
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
    bthA <- stats::approx(bthD,bthA,depT)$y
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
  layerT <- stats::approx(depths,wtr,layerD)$y
  layerA <- stats::approx(bthD,bthA,layerD)$y
  
  weightedT <- layerA*layerT*dz
  
  aveTemperature <- sum(weightedT)/(sum(layerA))/dz
  return(aveTemperature)  
}
