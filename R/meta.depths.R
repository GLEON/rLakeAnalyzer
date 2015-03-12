#'@title Calculate the Top and Bottom Depths of the Metalimnion
#'
#'@description
#'Calculates the top and bottom depths of the metalimnion
#' in a stratified lake. The metalimnion is defined as the 
#' water stratum in a stratified lake with the steepest thermal 
#' gradient and is demarcated by the bottom of the epilimnion and top 
#' of the hypolimnion.
#'
#'
#'@param wtr
#'a numeric vector of water temperature in degrees C
#'@param depths
#'a numeric vector corresponding to the depths (in m) of the wtr measurements
#'@param slope
#'a numeric vector corresponding to the minimum slope
#'@param seasonal
#'a logical value indicating whether the seasonal thermocline should be 
#'returned. This is fed to thermo.depth, which is used as the starting point. 
#'The seasonal thermocline is defined as the deepest density gradient found 
#'in the profile. If \code{FALSE}, the depth of the maximum density gradient is used 
#'as the starting point.
#'@param mixed.cutoff
#'A cutoff (deg C) where below this threshold, thermo.depth and meta.depths 
#'are not calculated (NaN is returned). Defaults to 1 deg C.
#'
#'@return
#'A numeric vector of the top and bottom metalimnion depths in meters. 
#'Returns the bottom depth if no distinct metalimion top and bottom found.
#'
#'@references
#'Wetzel, R. G. 2001. Limnology: Lake and River Ecosystems, 3rd ed. Academic Press.
#'
#'@author Jennifer Brentrup, Luke Winslow
#'
#'@seealso
#'
#'\code{\link{ts.meta.depths}}, \code{\link{thermo.depth}}
#'
#'@examples
#'	wtr = c(22.51, 22.42, 22.4, 22.4, 22.4, 22.36, 22.3, 22.21, 22.11, 21.23, 16.42, 
#'	15.15, 14.24, 13.35, 10.94, 10.43, 10.36, 9.94, 9.45, 9.1, 8.91, 8.58, 8.43)
#'	
#'	depths = c(0, 0.5, 1, 1.5, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 
#'				 17, 18, 19, 20)
#'				 
#'	m.d = meta.depths(wtr, depths, slope=0.1, seasonal=FALSE)
#'	cat('The top depth of the metalimnion is:', m.d[1])
#'	cat('The bottom depth of the metalimnion is:', m.d[2])
#'
#'@keywords manip
#'@export
meta.depths = function(wtr, depths, slope=0.1, seasonal=TRUE, mixed.cutoff=1){
  
	if(any(is.na(wtr))){
		return(rep(NaN, 2))
	}
  
	#We can't determine anything with less than 3 measurements
	# just return lake bottom
	if(length(wtr) < 3){
	  return(c(max(depths), max(depths)))
	}
	
	depths = sort.int(depths, index.return=TRUE)
  wtr = wtr[depths$ix]
  depths = depths$x
  
	thermoD=thermo.depth(wtr, depths, seasonal=seasonal, mixed.cutoff=mixed.cutoff)
	
	# if no thermo depth, then there can be no meta depths
	if(is.na(thermoD)){
		return(c(NaN, NaN))
	}
	
	 #We need water density, not temperature to do this
	rhoVar = water.density(wtr)

	dRhoPerc = 0.15; #in percentage max for unique thermocline step
	numDepths = length(depths)
	drho_dz = vector(mode="double", length=numDepths-1)

	#Calculate the first derivative of density
	for(i in 1:(numDepths-1)){
		drho_dz[i] = ( rhoVar[i+1]-rhoVar[i] )/( depths[i+1] - depths[i] )
	}
	
	#initiate metalimnion bottom as last depth, this is returned if we can't
	# find a bottom
	metaBot_depth = depths[numDepths]
	metaTop_depth = depths[1]
	Tdepth = rep(NaN, numDepths-1)
	
	for(i in 1:(numDepths-1)){
		Tdepth[i] = mean(depths[ i:(i+1) ]);
	}
	
	tmp = sort.int(c(Tdepth, thermoD+1e-6), index.return = TRUE)
	sortDepth = tmp$x
	sortInd = tmp$ix
	drho_dz = approx(Tdepth, drho_dz, sortDepth)
	drho_dz = drho_dz$y
	
	thermo_index = 1
	thermoId = numDepths;
	for(i in 1:numDepths){
		if(thermoId == sortInd[i]){
			thermo_index = i
			break;
		}
	}
	
	for (i in thermo_index:numDepths){ # moving down from thermocline index
		if (!is.na(drho_dz[i]) && drho_dz[i] < slope){ #top of metalimnion
			metaBot_depth = sortDepth[i];
			break
		}
	}
	
	if (i-thermo_index > 1 && (!is.na(drho_dz[thermo_index]) && drho_dz[thermo_index] > slope)){
		metaBot_depth = approx(drho_dz[thermo_index:i],
			sortDepth[thermo_index:i],slope)
		metaBot_depth = metaBot_depth$y
	}
	
	if(is.na(metaBot_depth)){
		metaBot_depth = depths[numDepths]
	}
	
	for(i in seq(thermo_index,1)){
		if(!is.na(drho_dz[i]) && drho_dz[i] < slope){
			metaTop_depth = sortDepth[i];
			break;
		}
	}
	
	if(thermo_index - i > 1 && (!is.na(drho_dz[thermo_index]) && drho_dz[thermo_index] > slope)){
		metaTop_depth = approx(drho_dz[i:thermo_index], sortDepth[i:thermo_index], slope);
		metaTop_depth = metaTop_depth$y
	}
	
  if(is.na(metaTop_depth)){
    metaTop_depth = depths[i]
  }
  
	return(c(metaTop_depth, metaBot_depth))
}


