# ---Author: Jake Zwart, 2013-04-21 --- 
#translated from Matlab Script - Author Jordan Read, 2009 
## from wedderburnNumber.m in https://github.com/jread-usgs/Lake-Analyzer/

wedderburn.number <- function(delta_rho,metaT,uSt,Ao,AvHyp_rho){
  
  # Calculates the Wedderburn number for a particular system using the following equation:
  #
  #  W = (g*delta_rho*(h^2))/(pHyp*(uSt^2)*Lo)
  #
  # where
  #   g = force of gravity
  #   delta_rho = density difference between the epilimnion and the hypolimnion
  #   metaT = thickness of the surface layer
  #   uSt = water friction velocity due to wind stress 
  #   Lo = fetch length in the direction of the wind.
  #
  
  #Constants
  g = 9.81                   #force of gravity
  
  Lo = 2 * sqrt(Ao/pi);      #Length at thermocline depth
  
  go = g*delta_rho/AvHyp_rho;
  
  # Calculates W according to formula provided
  W = go*metaT^2/(uSt^2*Lo);
  return(W)
}

