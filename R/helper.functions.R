## Helper functions for Lake Analyzer R

get.offsets <- function(data){
  
  header = names(data)
  
  header = header[-grep(pattern= "datetime", x= header, ignore.case= TRUE)] #Drop datetime
  
  matches = regexpr("(\\d+\\.?\\d*)" ,header)
  
  lengths = attr(matches,'match.length')
  offsets = vector(mode="numeric", length=length(matches))
  
  for(i in 1:length(matches)){
    offsets[i] = as.numeric(substr(header[i], matches[i], matches[i] + lengths[i]))
  }
  
  return(offsets)
}


get.drho_dz <- function(wtr, depths){
	numDepths = length(wtr)
	
	rhoVar = water.density(wtr)
	
	drho_dz = vector(mode="double", length=numDepths-1);
	
	#Calculate the first derivative of density
	for(i in 1:numDepths-1){
		drho_dz[i] = ( rhoVar[i+1]-rhoVar[i] )/( depths[i+1] - depths[i] );
	}
	drho_dz
}
