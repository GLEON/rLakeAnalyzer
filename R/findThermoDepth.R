#
# FindThermoDepth finds the thermocline depth from a temperature profile.
#
#
# Author: Luke Winslow <lawinslow@gmail.com>
# Adapted from FindThermoDepth.m in https://github.com/jread-usgs/Lake-Analyzer/
#
findThermoDepth <- function(wtr, depths, Smin = 0.1){
  #argh, data structures in R!
  #lets just do this the hard way to start.
  
  #We need water density, not temperature to do this
  rhoVar = waterDensity(wtr)
  
  dRhoPerc = 0.15; #in percentage max for unique thermocline step
  numDepths = length(depths);
  drho_dz = vector(mode="double", length=numDepths-1);
  
  #Calculate the first derivative of density
  for(i in 1:numDepths-1){
	drho_dz[i] = ( rhoVar[i+1]-rhoVar[i] )/( depths[i+1] - depths[i] );
  }
  
  #look for two distinct maximum slopes, lower one assumed to be seasonal
  thermoInd = which.max(drho_dz)  #Find max slope
  mDrhoZ = drho_dz[thermoInd]
  thermoD = mean( depths[thermoInd:(thermoInd+1)] )
  
  if(thermoInd > 1 && thermoInd < numDepths-1){  #if within range
	Sdn = -(depths[thermoInd+1] - depths[thermoInd])/
		(drho_dz[thermoInd+1] - drho_dz[thermoInd])
		
	Sup = (depths[thermoInd]-depths[thermoInd-1])/
		(drho_dz[thermoInd]-drho_dz[thermoInd-1])
		
	upD  = depths[thermoInd];
	dnD  = depths[thermoInd+1];
	if( !is.infinite(Sup) & !is.infinite(Sdn) ){
		thermoD = dnD*(Sdn/(Sdn+Sup))+upD*(Sup/(Sdn+Sup));
	}
  }
  
  dRhoCut = max( c(dRhoPerc*mDrhoZ, Smin) )
  locs = findPeaks(drho_dz, dRhoCut)
  pks = drho_dz[locs]
  
	if(length(pks) == 0){
		SthermoD = thermoD
		SthermoInd = thermoInd
	}else{
		mDrhoZ = pks[length(pks)]
		SthermoInd = locs[length(pks)]

		if(SthermoInd > thermoInd + 1){
			SthermoD = mean(depths[SthermoInd:(SthermoInd+1)])
			
			if(SthermoInd > 1 && SthermoInd < numDepths - 1){
				Sdn = -(depths[SthermoInd+1] - depths[SthermoInd])/
					(drho_dz[SthermoInd+1] - drho_dz[SthermoInd])
					
				Sup = (depths[SthermoInd]-depths[SthermoInd-1])/
					(drho_dz[SthermoInd]-drho_dz[SthermoInd-1])
					
				upD  = depths[SthermoInd]
				dnD  = depths[SthermoInd+1]
				
				if( !is.infinite(Sup) & !is.infinite(Sdn) ){
					SthermoD = dnD*(Sdn/(Sdn+Sup))+upD*(Sup/(Sdn+Sup))
				}
			}
		}else{
			SthermoD = thermoD
			SthermoInd = thermoInd
		}
	}
  
  if(SthermoD < thermoD){
	SthermoD = thermoD
	SthermoInd = thermoInd
  }
  
  list( thermoD,thermoInd,drho_dz,SthermoD,SthermoInd )
}


# Calculates water density with supplied temperature data
# wtr is in degC
# waterDensity is in grams/Liter

# <<--- Effective range of function: 0-40캜 --->>

# -- Author: R. Iestyn. Woolway ----
# -- Modified by : Luke Winslow <lawinslow@gmail.com>
waterDensity <- function(wtr){
  
  if(!is.numeric(wtr)){
    stop("waterDensity input must be in numeric form (vector or matrix).")
  }
  
  # calculate density
  rho <- 1000*(1-(wtr+288.9414)*
              (wtr-3.9863)^2/(508929.2*(wtr+68.12963)))
  
  # << equation provided by:
  # 같 Martin, J.L., McCutcheon, S.C., 1999. Hydrodynamics and Transport 같
  # 같 for Water Quality Modeling. Lewis Publications, Boca              같
  # 같 Raton, FL, 794pp. >>
  
  return(rho)
  
}

# Finds the local peaks in a vector. Checks the optionally supplied threshold 
#  for minimum height.
findPeaks <- function(x, thresh=0){
	pks <- which(diff(sign(diff(x, na.pad=FALSE)),na.pad=FALSE) < 0) + 2
	return(pks[x[pks-1] - x[pks] >= thresh])
}


