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



#' Calculate the Schmidt stability
#' 
#' Schmidt stability, or the resistance to mechanical mixing due to the
#' potential energy inherent in the stratification of the water column.
#' 
#' Schmidt stability was first defined by Schmidt (1928) and later modified by
#' Hutchinson (1957). This stability index was formalized by Idso (1973) to
#' reduce the effects of lake volume on the calculation (resulting in a mixing
#' energy requirement per unit area).
#' 
#' @param wtr a numeric vector of water temperature in degrees C
#' @param depths a numeric vector corresponding to the depths (in m) of the wtr
#' measurements
#' @param bthA a numeric vector of cross sectional areas (m^2) corresponding to
#' bthD depths
#' @param bthD a numeric vector of depths (m) which correspond to areal
#' measures in bthA
#' @param sal a numeric vector of salinity in Practical Salinity Scale units
#' @return a numeric vector of Schmidt stability (J/m^2)
#' @author Luke Winslow
#' @seealso \code{\link{ts.schmidt.stability}} \code{\link{lake.number}}
#' \code{\link{wedderburn.number}}
#' @references Schmidt, W., 1928. \emph{Ueber Temperatur and
#' Stabilitaetsverhaltnisse von Seen}. Geo- graphiska Annaler 10, 145-177.
#' 
#' Hutchinson, G.E., 1957. \emph{A Treatise on Limnology}, vol. 1. John Wiley &
#' Sons, Inc., New York.
#' 
#' Idso, S.B., 1973. \emph{On the concept of lake stability}. Limnology and
#' Oceanography 18, 681-683.
#' @keywords arith
#' @examples
#' 
#' 
#' 	bthA	<-	c(1000,900,864,820,200,10)
#' 	bthD	<-	c(0,2.3,2.5,4.2,5.8,7)
#' 	
#' 	wtr	<-	c(28,27,26.4,26,25.4,24,23.3)
#' 	depths	<-	c(0,1,2,3,4,5,6)
#' 	
#' 	cat('Schmidt stability for input is: ')
#' 	cat(schmidt.stability(wtr, depths, bthA, bthD))
#' @export
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
  	
  	bthA = stats::approx(bthD, bthA, depT)$y
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
  layerP = stats::approx(depths, rhoL, layerD)$y
  layerA = stats::approx(bthD, bthA, layerD)$y
  
  Zcv <- layerD %*% layerA / sum(layerA)
  St <- layerP %*% ((layerD - as.vector(Zcv)) * layerA) * dz * g / Ao
  
  return(St)

}

