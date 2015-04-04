# wndSpeed: wind speed in m/s
# wndHeight: height of wind measurement in m
# averageEpiDense: average epilimnion density in kg m-3

#----Author: R. Iestyn. Woolway ----
  
uStar <- function(wndSpeed,wndHeight,averageEpiDense){
  
  # define constants
  rhoAir <- 1.2 # density of air
  vonK <- 0.4 # von Karman constant

  # -- calculate drag coefficient (from Hicks, 1972)
  Cd <- rep(0.0015, length(wndSpeed))
  Cd[wndSpeed < 5] <- 0.001

  # -- correct for wind measurement height if < 10 m (Amorocho and DeVries, 1980)
  if (wndHeight != 10){
    wndSpeed <- wndSpeed/(1-sqrt(Cd)/vonK*log(10/wndHeight))
  }
    
  # -- calculate shear stress of air (Fischer et al., 1979)
  tau <- Cd*rhoAir*wndSpeed^2
  
  # -- calculate uStar following Imberger (1985)
  uStar <- sqrt(tau/averageEpiDense)
  return(uStar)
}
