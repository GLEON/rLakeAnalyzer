# wndSpeed: wind speed in m/s
# wndHeight: height of wind measurement in m
# averageEpiDense: average epilimnion density in kg m-3

#----Author: R. Iestyn. Woolway ----
  
uStar <- function(wndSpeed,wndHeight,averageEpiDense){
  
  # define constants
  rhoAir <- 1.2 # density of air
  vonK <- 0.4 # von Karman constant

  # -- calculate drag coefficient (from Hicks, 1972)
  if (wndSpeed < 5){
    Cd <- 0.001
  } else{
    Cd <- 0.0015
  }

  # -- correct for wind measurement height if < 10 m (Amorocho and DeVries, 1980)
  if (wndHeight != 10){
    wndSpeed <- wndSpeed/(1-sqrt(Cd)/vonK*log(10/wndHeight))
  }
    
  # -- calculate shear stress of air (Fischer et al., 1979)
  tau <- Cd*rhoAir*wndSpeed^2
  
  # -- calculate uStar following Imberger (1985)
  uStar <- sqrt(tau/averageEpiDense)
   
}

# -- References
#같 Hicks, B.B., 1972. A procedure for the formulation of bulk transfer
#같 coefficients over water bodies of different sizes. Boundary-Layer
#같 Meterology 3: 201-213

#같 Amorocho, J., DeVries, J.J., 1980. A new evaluation of the wind 같
#같 stress coefficient over water surfaces. Journal of Geophysical  같
#같 Research 85: 433-442.

#같 Fischer, H.B., List, E.J., Koh, R.C.Y., Imberger, J., Brooks, N.H.,
#같 1979. Mixing in inland and coastal waters. Academic Press.

#같 Imberger, J., 1985. The diurnal mixed layer. Limnology and Oceanography
#같 30: 737-770.