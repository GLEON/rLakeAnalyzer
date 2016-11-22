### R Lake Analyzer
# Author: J. Brentrup May 2013
# Sources: Iso, S.B. 1973. On the concept of lake stability. Limnol. and Oceanogr. 18: 683-681.
# J.S. Read - Matlab code 2009 (Adapted)

### Equation: St = (g/A0)*[Integral from 0 to zm:(z-zp)*pz*Az*dz]
#St = Schmidt stability
#g = gravity (9.81 m/s2)
#A0 = surface area
#z = depth
#zm = max. depth
#zp = mean density - uses Richard's function water.density.R
#pz = observed denisty at depth z
#Az = area at depth z

schmidt.stability = function(wtr, depths, bthA, bthD, sal = 0){

  if(length(wtr) != length(depths)){
  	stop('water temperature array must be the same length as the depth array')
  }
  
  #having some weird issues with wtr and sal lengths, trying to fix with this
  if(length(sal) == 1){
  	sal = rep(sal, length(wtr))
  }
  
  #Constants
  g = 9.81
  dz = 0.1
  
  # Here is just some madeup data. This should 
  # seem valid to the Schmidt Stability algorithm. Valid enough at least
  #wtr = c(24,24,24,20,17,12,11,10,10)
  #depths = 1:9
  #sal = wtr*0
  #bthD = 1:9
  #bthA = seq(8,0,by=-1)
  
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
  	sal[numD+1] = sal[numD]
  	depths[numD+1] = max(bthD)
  }else if(max(bthD) < depths[numD]) {
  	bthD = c(bthD, depths[numD])
  	bthA = c(bthA, 0)
  }
  
  if(min(bthD) < depths[1]) {
  	wtr = c(wtr[1], wtr)
  	sal = c(sal[1], sal)
  	depths = c(min(bthD), depths)
  }
  
  Zo = min(depths)
  Io = which.min(depths)
  Ao = bthA[Io]
  
  if(Ao == 0){
  	stop('Surface area cannot be zero, check *.bth file')
  }
  
  #Calculate water density 
  rhoL = water.density(wtr, sal)
  
  #The approx (interp1 in matlab) just does linear interpolation
  layerD = seq(min(depths), max(depths), by=dz)
  layerP = approx(depths, rhoL, layerD)$y
  layerA = approx(bthD, bthA, layerD)$y
  
  Zcv <- layerD %*% layerA / sum(layerA)
  St <- layerP %*% ((layerD - Zcv) * layerA) * dz * g / Ao
  
  return(St)

}

