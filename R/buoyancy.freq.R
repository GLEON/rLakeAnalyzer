#
# Buoyancy Frequency
# Author: Luke Winslow <lawinslow@gmail.com>
#


#' Calculates buoyancy frequency.
#' 
#' Calculate the buoyancy frequency (Brunt-Vaisala frequency) for a temperature
#' profile.
#' 
#' 
#' @param wtr a numeric vector of water temperature in degrees C
#' @param depths a numeric vector corresponding to the depths (in m) of the wtr
#' measurements
#' @return Returns a vector of buoyancy frequency in units \code{sec^-2}.
#' Return value has attribute "depths" which define buoyancy frequency depths
#' (which differ from supplied depths).
#' @author Luke Winslow
#' @seealso \code{thermo.depth}, \code{ts.buoyancy.freq}
#' @keywords arith
#' @examples
#' 
#' 
#' 	# A vector of water temperatures
#' 	wtr = c(22.51, 22.42, 22.4, 22.4, 22.4, 22.36, 22.3, 22.21, 22.11, 21.23, 16.42, 
#' 		15.15, 14.24, 13.35, 10.94, 10.43, 10.36, 9.94, 9.45, 9.1, 8.91, 8.58, 8.43)
#' 
#' 	#A vector defining the depths
#' 	depths = c(0, 0.5, 1, 1.5, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 
#' 		17, 18, 19, 20)
#' 	
#' 	b.f = buoyancy.freq(wtr, depths)
#' 	
#' 	plot(b.f, attr(b.f, 'depths'), type='b', 
#' 		ylab='Depth', xlab='Buoyancy Frequency', ylim=c(max(depths), min(depths)))
#' 
#' @export
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



#' Calculate the buoyancy (Brunt-Vaisala) frequency for a temperature profile.
#' 
#' Function for simplifying the calculation of buoyancy frequency. Can usually
#' be called directly on data loaded directly using \code{\link{load.ts}} and
#' \code{\link{load.bathy}}.
#' 
#' 
#' @param wtr A data frame of water temperatures (in Celsius). Loaded using
#' \code{\link{load.ts}}
#' @param at.thermo Boolean indicating if only buoyancy frequency at the
#' thermocline should be returned. If false, full profile is returned.
#' @param na.rm Boolean indicated if step-by-step removal of NA's should be
#' tried. If false, a timestep with any NA values will likely return an NA
#' value. If true, best effort will be made to calculate indices despite NA
#' values.
#' @param ...  Additional parameters will be passed on to \code{thermo.depth}
#' function when extracting buoyancy frequency at only the thermocline.  Common
#' parameters to supply would be \code{seasonal} and \code{slope}.
#' @return Returns a data frame with the timeseries of buoyancy frequency in
#' units \code{sec^-2}. Includes a \sQuote{datetime} column.
#' @author Luke Winslow
#' @seealso \code{buoyancy.freq}
#' @references Imberger, J., Patterson, J.C., 1990. \emph{Physical limnology}.
#' Advances in Applied Mechanics 27, 353-370.
#' @keywords arith
#' @examples
#' 
#' 
#' 	#Get the path for the package example file included
#' 	wtr.path <- system.file('extdata', 'Sparkling.daily.wtr', package="rLakeAnalyzer")
#' 	
#' 	#Load data for example lake, Sparkilng Lake, Wisconsin.
#' 	sp.wtr = load.ts(wtr.path)
#' 	
#' 	N2 = ts.buoyancy.freq(sp.wtr, seasonal=FALSE)
#' 	SN2 = ts.buoyancy.freq(sp.wtr, seasonal=TRUE)
#' 	
#' 	plot(N2, type='l', ylab='Buoyancy Frequency', xlab='Date')
#' 	lines(SN2, col='red')
#' 	
#' @export
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
    n2 = cbind(data.frame(datetime=get.datetime(wtr)), n2)
  }
  
  return(n2)
}
