#' @title Returns the average density of a layer between two depths.
#' 
#' @description This function calculates the average density of a layer of water between two
#' depths.
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
#' @param sal Optional numeric vector of salinity in Practical Salinity Units
#' corresponding to water temperature vector. If left blank, salinity is set to
#' be zero
#' @return Numeric value of average water density for bounded layer in kg/m^3
#' @seealso \code{water.density}
#' @keywords manip
#' @examples
#' 
#' top     <- 2
#' bottom  <- 6
#' wtr     <- c(25.2,25.1,24.1,22.0,19.8,15.3,12.0,11.1)
#' depths  <- c(0,1,2,3,4,5,6,7) 
#' bthA    <- c(10000,8900,5000,3500,2000,1000,300,10)
#' bthD    <- c(0,1,2,3,4,5,6,7)
#' layer.density(top,bottom,wtr,depths,bthA,bthD)
#' @export
layer.density <- function(top, bottom, wtr, depths, bthA, bthD, sal = wtr*0){
  
  force(sal) #for evaluation of promise for salinity
  
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
  layerT <- stats::approx(depths,wtr,layerD)$y
  layerS <- stats::approx(depths,sal,layerD)$y
  layerA <- stats::approx(bthD,bthA,layerD)$y
  layerP <- water.density(layerT,layerS)
  
  mass <- layerA*layerP*dz
  aveDensity <- sum(mass)/(sum(layerA))/dz
  return(aveDensity)  
}
