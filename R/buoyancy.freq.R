#
# Buoyancy Frequency
# Author: Luke Winslow <lawinslow@gmail.com>
#
buoyancy.freq <- function(wtr, depths){
  
  rhoVar = water.density(wtr)
  
  numDepths = length(depths);
  n2 = rep(NaN, numDepths-1);
  n2depths = rep(NaN, numDepths-1)
  
  #Calculate the first derivative of density
  for(i in 1:(numDepths-1)){
    n2[i] = 9.81/rhoVar[i]*( rhoVar[i+1]-rhoVar[i] )/( depths[i+1] - depths[i] )
    n2depths[i] = (depths[i+1] + depths[i])/2
  }
  
  attr(n2, 'depths') = n2depths
  
  return(n2)
  
}

ts.buoyancy.freq <- function(wtr, at.thermo=TRUE, na.rm=FALSE, ...){

  depths = get.offsets(wtr)
  
  n = nrow(wtr)
  
  #drop the datetime column
  wtr.mat = as.matrix(drop.datetime(wtr))
  
  if(na.rm & !at.thermo){
  	warning('rLakeAnalyzer::ts.buoyancy.freq: na.rm is ignored if full buoyancy frequency profile is calculated')
  }
  
  #If just N2 at the thermocline is requested, pull out just those values
  if(at.thermo){
  
    n2 = rep(NA, n)
    
    for(i in 1:n){
    	if(na.rm){
    		temps = wtr.mat[i,]
    		notNA = !is.na(temps)
    		
    		thermo.indx = thermo.depth(temps[notNA], depths[notNA], index=TRUE, ...)
    		tmp.n2 = buoyancy.freq(temps[notNA], depths[notNA])
    		
    	}else{
      	thermo.indx = thermo.depth(wtr.mat[i,], depths, index=TRUE, ...)
      	tmp.n2 = buoyancy.freq(wtr.mat[i,], depths)
    	}
      if(!is.na(thermo.indx)){
        n2[i] = tmp.n2[thermo.indx]
      }
    }
    n2 = data.frame(datetime=get.datetime(wtr), n2=n2)
    
  }else{ #If N2 is requested for the entire water column, output full data.frame
    
    n2 = matrix(NA, nrow=n, ncol=(length(depths)-1))
    
    for(i in 1:n){
      n2[i,] = buoyancy.freq(wtr.mat[i,], depths)
    }
    
    tmp = buoyancy.freq(wtr.mat[1,], depths)
    attr(n2, 'depths') = attr(tmp, 'depths')
    n2 = as.data.frame(n2)
    names(n2) = paste('N2_', as.character(attr(tmp,'depth')), sep='')
    n2 = rbind(data.frame(datetime=get.datetime(wtr)), n2)
  }
  
  return(n2)
}
