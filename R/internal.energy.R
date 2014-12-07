#'@title Internal energy function (Joules)
#'@description Calculates the internal energy of the water column with temperature and hypsography
#'
#'@details Internal energy is the thermal energy in the water column, which is calculated by 
#'multiplying the specific heat of water (J kg-1 K-1) by the temperature and mass of the water 
#'in the lake. 
#'
#'@param wtr a numeric vector of water temperature in degrees C
#'@param depths a numeric vector corresponding to the depths (in m) of the wtr measurements
#'@param bthA a numeric vector of cross sectional areas (m^2) corresponding to bthD depths
#'@param bthD a numeric vector of depths (m) which correspond to areal measures in bthA
#'
#'@return internal energy in Joules. (Currently not vectorized..)
#'@author Jordan S. Read
#'
#'@examples
#'bthA  <-	c(1000,900,864,820,200,10)
#'bthD	<-	c(0,2.3,2.5,4.2,5.8,7)
#'
#'wtr	<-	c(28,27,26.4,26,25.4,24,23.3)
#'depths	<-	c(0,1,2,3,4,5,6)
#'
#'cat('Internal Energy for input is: ')
#'cat(internal.energy(wtr, depths, bthA, bthD))
#'@export

internal.energy = function(wtr, depths, bthA, bthD){
  
  
  # 1D for the time being
  dz = 0.1
  cw = 4186; #J kg-1 degK-1 
  
  # if bathymetry has negative values, drop and interpolate to 0
  if(min(bthD) < 0){
    useI = bthD >= 0
    
    if(any(bthD == 0)){
      depT = bthD[useI]
    }else{
      depT = c(0, bthD[useI])
    }
    
    bthA = approx(bthD, bthA, depT)$y
    bthD = depT
  }
  
  numD = length(wtr)
  if(max(bthD) > depths[numD]){
    wtr[numD+1] = wtr[numD]
    depths[numD+1] = max(bthD)
  }else if(max(bthD) < depths[numD]) {
    bthD = c(bthD, depths[numD])
    bthA = c(bthA, 0)
  }
  
  if(min(bthD) < depths[1]) {
    wtr = c(wtr[1], wtr)
    depths = c(min(bthD), depths)
  }
  
  Zo = min(depths)
  Io = which.min(depths)
  Ao = bthA[Io]
  
  if(Ao == 0){
    stop('Surface area cannot be zero, check *.bth file')
  }
  
  #Calculate water density 
  rhoL = water.density(wtr)
  
  #The approx (interp1 in matlab) just does linear interpolation
  layerD = seq(min(depths), max(depths), by=dz)
  layerP = approx(depths, rhoL, layerD)$y
  layerT = approx(depths,wtr, layerD)$y
  layerA = approx(bthD, bthA, layerD)$y
  
  v_i = layerA*dz
  # -- calculate mass of water in each dz layer --
  m_i = layerP*v_i
  # -- calculate internal energy of each layer in time --
  u_i = layerT*m_i*cw
  # -- sum all layers for individual time points --
  U = sum(u_i)/layerA[1]
  return(U)
}