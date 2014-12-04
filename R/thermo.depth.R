#
# FindThermoDepth finds the thermocline depth from a temperature profile.
#
#
# Author: Luke Winslow <lawinslow@gmail.com>
# Adapted from FindThermoDepth.m in https://github.com/jread-usgs/Lake-Analyzer/
#
thermo.depth <- function(wtr, depths, Smin = 0.1, seasonal=TRUE, index=FALSE){
  
  if(any(is.na(wtr))){
    return(NaN)
  }
  
  #We can't determine anything with less than 3 measurements
  # just return lake bottom
  if(length(wtr) < 3){
    return(NaN)
  }
  
  if(length(depths) != length(unique(depths))){
    stop('Depths all must be unique')
  }
  
  #We need water density, not temperature to do this
  rhoVar = water.density(wtr)
  
  dRhoPerc = 0.15; #in percentage max for unique thermocline step
  numDepths = length(depths);
  drho_dz = rep(NaN, numDepths-1);
  
  #Calculate the first derivative of density
  for(i in 1:(numDepths-1)){
	  drho_dz[i] = ( rhoVar[i+1]-rhoVar[i] )/( depths[i+1] - depths[i] );
  }
  
  #look for two distinct maximum slopes, lower one assumed to be seasonal
  thermoInd = which.max(drho_dz)  #Find max slope
  mDrhoZ = drho_dz[thermoInd]
  thermoD = mean( depths[thermoInd:(thermoInd+1)] )
  
  if(thermoInd > 1 && thermoInd < (numDepths-1)){  #if within range
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

		if(SthermoInd > (thermoInd + 1)){
			SthermoD = mean(depths[SthermoInd:(SthermoInd+1)])
			
			if(SthermoInd > 1 && SthermoInd < (numDepths - 1)){
				Sdn = -(depths[SthermoInd+1] - depths[SthermoInd])/
					(drho_dz[SthermoInd+1] - drho_dz[SthermoInd])
					
				Sup = (depths[SthermoInd] - depths[SthermoInd-1])/
					(drho_dz[SthermoInd] - drho_dz[SthermoInd-1])
					
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
  
  #Ok, which output was requested. Index or value
  # seasonal or non-seasonal
  if(index){
    if(seasonal){
      return(SthermoInd)
    }else{
      return(thermoInd)
    }
  }else{
    if(seasonal){
      return(SthermoD)
    }else{
    	return(thermoD)
    }
  }
  
  #list( thermoD, thermoInd, drho_dz, SthermoD, SthermoInd )
}

# Finds the local peaks in a vector. Checks the optionally supplied threshold 
#  for minimum height.
findPeaks <- function(dataIn, thresh=0){
	
	varL = length(dataIn);
	locs = rep(FALSE, varL);
	peaks= rep(NaN, varL);
	
	for(i in 2:varL-1){
		pkI = which.max(dataIn[(i-1):(i+1)])
		posPeak = max(dataIn[(i-1):(i+1)]);
		
		if(pkI == 2) {
			peaks[i] = posPeak;
			locs[i]  = TRUE;
		}
	}
	
	inds = 1:varL;
	locs = inds[locs];
	peaks= peaks[locs];
	
	# remove all below threshold value
	
	useI = peaks > thresh;
	locs = locs[useI];
	
	return(locs)
}


