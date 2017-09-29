#' @title Calculate physical indices for a timeseries.
#' 
#' @description Functions for simplifying the calculation of physical indices for a
#' timeseries of observation data.  Can usually be called directly on data
#' loaded directly using \code{\link{load.ts}} and \code{\link{load.bathy}}.
#' 
#' These are wrapper functions that accept a timeseries of data and call the
#' core physical metric functions (like \code{\link{schmidt.stability}}) on
#' each timestep.
#' 
#' @family Timeseries functions for r Lake Analyzer
#' 
#' @param wtr A data frame of water temperatures (in Celsius). Loaded using
#' \code{\link{load.ts}}. Must have columns \code{datetime}, \code{wtr_##.#}
#' where ##.# is depth in meters.
#' @param slope The minimum density gradient (kg/m^3/m) that can be called the
#' thermocline
#' @param na.rm Boolean indicated if step-by-step removal of NA's should be
#' tried. If false, a timestep with any NA values will return an NA value. If
#' true, best effort will be made to calculate indices despite NA values.
#' @param ...  Additional parameters passed to underlying base function (e.g.,
#' index=TRUE for thermo.depth)
#' @return Returns a data frame with the timeseries of calculated derivatives.
#' All include a \sQuote{datetime} column, but derivative columns differ
#' between functions.
#' @seealso For loading input data \code{\link{load.ts}},
#' \code{\link{load.bathy}}.
#' 
#' For the underlying functions operating at each timestep
#' \code{\link{meta.depths}}, \code{\link{thermo.depth}},
#' \code{\link{schmidt.stability}}, \code{\link{lake.number}},
#' \code{\link{internal.energy}}.
#' @keywords manip
#' @examples
#' 
#' 	#Get the path for the package example file included
#' 	exampleFilePath <- system.file('extdata', 'Sparkling.daily.wtr', package="rLakeAnalyzer")
#' 	
#' 	#Load
#' 	sparkling.temp = load.ts(exampleFilePath)
#' 	
#' 	#calculate and plot the metalimnion depths
#' 	m.d = ts.meta.depths(sparkling.temp)
#' 	
#' 	plot(m.d$datetime, m.d$top, type='l', ylab='Meta Depths (m)', xlab='Date', col='blue')
#' 	lines(m.d$datetime, m.d$bottom, col='red')
#'   
#'   
#' @export

ts.meta.depths <- function(wtr, slope=0.1, na.rm=FALSE, ...){
  
  depths = get.offsets(wtr)
  
  n = nrow(wtr)
  
  #drop the datetime column
  wtr.mat = as.matrix(drop.datetime(wtr))
  
  m.d = matrix(NA, nrow=n, ncol=2)
  
  for(i in 1:n){
    if(na.rm){
      temps = wtr.mat[i,]
      notNA = !is.na(temps)
      m.d[i,] = meta.depths(temps[notNA], depths[notNA], slope, ...) # Assume seasonal thermoD start
    }else{
      m.d[i,] = meta.depths(wtr.mat[i,], depths, slope, ...) # Assume seasonal thermoD start
    }
  }
  
  return(data.frame(datetime=get.datetime(wtr), top=m.d[,1], bottom=m.d[,2]))

}


#' @title Calculate physical indices for a timeseries.
#' 
#' @description Functions for simplifying the calculation of physical indices for a
#' timeseries of observation data.  Can usually be called directly on data
#' loaded directly using \code{\link{load.ts}} and \code{\link{load.bathy}}.
#' 
#' These are wrapper functions that accept a timeseries of data and call the
#' core physical metric functions (like \code{\link{schmidt.stability}}) on
#' each timestep.
#' 
#' @family Timeseries functions for r Lake Analyzer
#' 
#' @param wtr A data frame of water temperatures (in Celsius). Loaded using
#' \code{\link{load.ts}}. Must have columns \code{datetime}, \code{wtr_##.#}
#' where ##.# is depth in meters.
#' @param Smin The minimum density gradient cutoff (kg/m^3/m) defining the
#' metalimion
#' @param na.rm Boolean indicated if step-by-step removal of NA's should be
#' tried. If false, a timestep with any NA values will return an NA value. If
#' true, best effort will be made to calculate indices despite NA values.
#' @param ...  Additional parameters passed to underlying base function (e.g.,
#' index=TRUE for thermo.depth)
#' @return Returns a data frame with the timeseries of calculated derivatives.
#' All include a \sQuote{datetime} column, but derivative columns differ
#' between functions.
#' @seealso For loading input data \code{\link{load.ts}},
#' \code{\link{load.bathy}}.
#' 
#' For the underlying functions operating at each timestep
#' \code{\link{meta.depths}}, \code{\link{thermo.depth}},
#' \code{\link{schmidt.stability}}, \code{\link{lake.number}},
#' \code{\link{internal.energy}}.
#' @keywords manip
#' @examples
#' 
#' 	#Get the path for the package example file included
#' 	exampleFilePath <- system.file('extdata', 'Sparkling.daily.wtr', package="rLakeAnalyzer")
#' 	
#' 	#Load
#' 	sparkling.temp = load.ts(exampleFilePath)
#'   
#'   
#'   #calculate and plot the thermocline depth
#' 	t.d = ts.thermo.depth(sparkling.temp)
#' 	
#' 	plot(t.d$datetime, t.d$thermo.depth, type='l', ylab='Thermocline Depth (m)', xlab='Date')
#'   
#' @export
#' 
 
ts.thermo.depth <- function(wtr, Smin = 0.1, na.rm=FALSE, ...){
  
  depths = get.offsets(wtr)
  
  n = nrow(wtr)
  t.d = rep(NA, n)
  
  #drop the datetime column
  wtr.mat = as.matrix(drop.datetime(wtr))
  
  dimnames(wtr.mat) <- NULL
  
  for(i in 1:n){
    if(na.rm){
      temps = wtr.mat[i,]
      notNA = !is.na(temps)
      t.d[i] = thermo.depth(temps[notNA], depths[notNA], ...)
    }else{
      if(any(is.na(wtr.mat[i,]))){
        t.d[i] = NA
        next
      }
      #thermo.depth <- function(wtr, depths, Smin = 0.1){\
      t.d[i] = thermo.depth(wtr.mat[i,], depths, ...)
    }
  }

  output = data.frame(datetime=get.datetime(wtr), thermo.depth=t.d)
  
  return(output)
}

#' @title Calculate physical indices for a timeseries.
#' 
#' @description Functions for simplifying the calculation of physical indices for a
#' timeseries of observation data.  Can usually be called directly on data
#' loaded directly using \code{\link{load.ts}} and \code{\link{load.bathy}}.
#' 
#' These are wrapper functions that accept a timeseries of data and call the
#' core physical metric functions (like \code{\link{schmidt.stability}}) on
#' each timestep.
#' 
#' @family Timeseries functions for r Lake Analyzer
#' 
#' @param wtr A data frame of water temperatures (in Celsius). Loaded using
#' \code{\link{load.ts}}. Must have columns \code{datetime}, \code{wtr_##.#}
#' where ##.# is depth in meters.
#' @param bathy A data frame containing hypsometric data. Loaded using
#' \code{\link{load.bathy}}
#' @param na.rm Boolean indicated if step-by-step removal of NA's should be
#' tried. If false, a timestep with any NA values will return an NA value. If
#' true, best effort will be made to calculate indices despite NA values.
#' @return Returns a data frame with the timeseries of calculated derivatives.
#' All include a \sQuote{datetime} column, but derivative columns differ
#' between functions.
#' @seealso For loading input data \code{\link{load.ts}},
#' \code{\link{load.bathy}}.
#' 
#' For the underlying functions operating at each timestep
#' \code{\link{meta.depths}}, \code{\link{thermo.depth}},
#' \code{\link{schmidt.stability}}, \code{\link{lake.number}},
#' \code{\link{internal.energy}}.
#' @keywords manip
#' @export
ts.schmidt.stability <- function(wtr, bathy, na.rm=FALSE){
	
	depths = get.offsets(wtr)
	
	n = nrow(wtr)
	s.s = rep(NA, n)
	
	#drop the datetime column
	wtr.mat = as.matrix(drop.datetime(wtr))
	
	dimnames(wtr.mat) <- NULL
	
	for(i in 1:n){
    if(na.rm){
      temps = wtr.mat[i,]
      if(all(is.na(temps))){
      	next
      }
      notNA = !is.na(temps)
      s.s[i] = schmidt.stability(temps[notNA], depths[notNA], bathy$areas, bathy$depths)
    }else{
  		if(any(is.na(wtr.mat[i,]))){
  			s.s[i] = NA
  			next
  		}
  		#thermo.depth <- function(wtr, depths, Smin = 0.1){\
  		s.s[i] = schmidt.stability(wtr.mat[i,], depths, bathy$areas, bathy$depths)
    }
	}
	
	output = data.frame(datetime=get.datetime(wtr), schmidt.stability=s.s)
	
	return(output)
	
}

#' @title Calculate physical indices for a timeseries.
#' 
#' @description Functions for simplifying the calculation of physical indices for a
#' timeseries of observation data.  Can usually be called directly on data
#' loaded directly using \code{\link{load.ts}} and \code{\link{load.bathy}}.
#' 
#' These are wrapper functions that accept a timeseries of data and call the
#' core physical metric functions (like \code{\link{schmidt.stability}}) on
#' each timestep.
#' 
#' @family Timeseries functions for r Lake Analyzer
#' 
#' @param wtr A data frame of water temperatures (in Celsius). Loaded using
#' \code{\link{load.ts}}. Must have columns \code{datetime}, \code{wtr_##.#}
#' where ##.# is depth in meters.
#' @param bathy A data frame containing hypsometric data. Loaded using
#' \code{\link{load.bathy}}
#' @param wnd A data frame of wind speeds (in m/s). Loaded using
#' \code{\link{load.ts}}
#' @param wnd.height Height of the anemometer above the lake surface in meters
#' @param seasonal Boolean indicating if seasonal thermocline should be used in
#' calculation.
#' @return Returns a data frame with the timeseries of calculated derivatives.
#' All include a \sQuote{datetime} column, but derivative columns differ
#' between functions.
#' @seealso For loading input data \code{\link{load.ts}},
#' \code{\link{load.bathy}}.
#' 
#' For the underlying functions operating at each timestep
#' \code{\link{meta.depths}}, \code{\link{thermo.depth}},
#' \code{\link{schmidt.stability}}, \code{\link{lake.number}},
#' \code{\link{internal.energy}}.
#' @keywords manip
#' @export
#' 


ts.lake.number <- function(wtr, wnd, wnd.height, bathy, seasonal=TRUE){
	
	depths = get.offsets(wtr)
	
	# Make sure data frames match by date/time. 
	all.data = merge(wtr, wnd, by='datetime')
	
	cols = ncol(all.data)
	wtr = all.data[,-cols]
	wnd = all.data[,c(1, cols)]
	
	n = nrow(wtr)
	l.n = rep(NA, n)
	
	wtr.mat = as.matrix(wtr[,-1])
	dimnames(wtr.mat) <- NULL
	
	for(i in 1:n){
		if(any(is.na(wtr.mat[i,])) || is.na(wnd[i,2])){
			
			next
		}
		
		m.d = meta.depths(wtr.mat[i,], depths, seasonal=seasonal)
		if(any(is.na(m.d))){
			next
		}
		
		epi.dens = layer.density(0, m.d[1], wtr.mat[i,], depths, bathy$areas, bathy$depths)
		hypo.dens = layer.density(m.d[2], max(depths), wtr.mat[i,], depths, bathy$areas, bathy$depths)
		
		
		uS = uStar(wnd[i,2], wnd.height, epi.dens)
		
		St = schmidt.stability(wtr.mat[i,], depths, bathy$areas, bathy$depths)
		
		#thermo.depth <- function(wtr, depths, Smin = 0.1){\
		l.n[i] = lake.number(bathy$areas, bathy$depths, uS, St, m.d[1], m.d[2], hypo.dens)
	}
	
	output = data.frame(datetime=get.datetime(wtr), lake.number=l.n)
	
	return(output)
}

#' @title Calculate physical indices for a timeseries.
#' 
#' @description Functions for simplifying the calculation of physical indices for a
#' timeseries of observation data.  Can usually be called directly on data
#' loaded directly using \code{\link{load.ts}} and \code{\link{load.bathy}}.
#' 
#' These are wrapper functions that accept a timeseries of data and call the
#' core physical metric functions (like \code{\link{schmidt.stability}}) on
#' each timestep.
#' 
#' @family Timeseries functions for r Lake Analyzer
#' 
#' @param wtr A data frame of water temperatures (in Celsius). Loaded using
#' \code{\link{load.ts}}. Must have columns \code{datetime}, \code{wtr_##.#}
#' where ##.# is depth in meters.
#' @param bathy A data frame containing hypsometric data. Loaded using
#' \code{\link{load.bathy}}
#' @param wnd A data frame of wind speeds (in m/s). Loaded using
#' \code{\link{load.ts}}
#' @param wnd.height Height of the anemometer above the lake surface in meters
#' @param seasonal Boolean indicating if seasonal thermocline should be used in
#' calculation.
#' @return Returns a data frame with the timeseries of calculated derivatives.
#' All include a \sQuote{datetime} column, but derivative columns differ
#' between functions.
#' @seealso For loading input data \code{\link{load.ts}},
#' \code{\link{load.bathy}}.
#' 
#' For the underlying functions operating at each timestep
#' \code{\link{meta.depths}}, \code{\link{thermo.depth}},
#' \code{\link{schmidt.stability}}, \code{\link{lake.number}},
#' \code{\link{internal.energy}}.
#' @keywords manip
#' @export
ts.uStar <- function(wtr, wnd, wnd.height, bathy, seasonal=TRUE){
	
	depths = get.offsets(wtr)
	
	# Make sure data frames match by date/time. 
	all.data = merge(wtr, wnd, by='datetime')
	
	cols = ncol(all.data)
	wtr = all.data[,-cols]
	wnd = all.data[,c(1, cols)]
	
	n = nrow(wtr)
	uStar = rep(NA, n)
	
	wtr.mat = as.matrix(wtr[,-1])
	dimnames(wtr.mat) <- NULL
	
	for(i in 1:n){
		if(any(is.na(wtr.mat[i,])) || is.na(wnd[i,2])){
			
			next
		}
		
		m.d = meta.depths(wtr.mat[i,], depths, seasonal=seasonal)
		if(any(is.na(m.d))){
			next
		}
		
		epi.dens = layer.density(0, m.d[1], wtr.mat[i,], depths, bathy$areas, bathy$depths)
		
		
		uStar[i] = uStar(wnd[i,2], wnd.height, epi.dens)
	}
	
	output = data.frame(datetime=get.datetime(wtr), uStar=uStar)
	
	return(output)
}




#' Calculate Wedderburn number for a timeseries.
#' 
#' Function for simplifying the calculation of Wedderburn Number. Can usually
#' be called directly on data loaded directly using \code{\link{load.ts}} and
#' \code{\link{load.bathy}}.
#' 
#' 
#' @param wtr A data frame of water temperatures (in Celsius). Loaded using
#' \code{\link{load.ts}}
#' @param wnd A data frame of wind speeds (in m/s). Loaded using
#' \code{\link{load.ts}}
#' @param wnd.height Height of the anemometer above the lake surface in meters
#' @param bathy A data frame containing hypsometric data. Loaded using
#' \code{\link{load.bathy}}
#' @param Ao Numeric value for the water body surface area (m^2) at zero meters
#' depth
#' @param seasonal Boolean indicating if seasonal thermocline should be used in
#' calculation.
#' @return Returns a data frame with the timeseries of Wedderburn number.
#' Includes a \sQuote{datetime} column.
#' @seealso \code{wedderburn.number},\code{ts.lake.number}
#' @references Imberger, J., Patterson, J.C., 1990. \emph{Physical limnology}.
#' Advances in Applied Mechanics 27, 353-370.
#' @keywords arith
#' @examples
#' 
#' 
#' 	#Get the path for the package example file included
#' 	wtr.path <- system.file('extdata', 'Sparkling.daily.wtr', package="rLakeAnalyzer")
#' 	wnd.path <- system.file('extdata', 'Sparkling.daily.wnd', package="rLakeAnalyzer")
#' 	bathy.path <- system.file('extdata', 'Sparkling.bth', package="rLakeAnalyzer")
#' 	
#' 	#Load data for example lake, Sparkilng lake, in Wisconsin.
#' 	sp.wtr = load.ts(wtr.path)
#' 	sp.wnd = load.ts(wnd.path)
#' 	sp.bathy = load.bathy(bathy.path)
#' 	
#' 	sp.area = 64e4  #Area of Sparkling lake in m^2
#' 	wnd.height = 2  #Height of Sparkling lake anemometer
#' 	
#' 	w.n = ts.wedderburn.number(sp.wtr, sp.wnd, wnd.height, sp.bathy, sp.area)
#' 	plot(w.n$datetime, w.n$wedderburn.number, type='l', ylab='Wedderburn Number', xlab='Date')
#' 	
#' @export
ts.wedderburn.number <- function(wtr, wnd, wnd.height, bathy, Ao, seasonal=TRUE){
  
  depths = get.offsets(wtr)
  
  # Make sure data frames match by date/time. 
  all.data = merge(wtr, wnd, by='datetime')
  
  cols = ncol(all.data)
  wtr = all.data[,-cols]
  wnd = all.data[,c(1, cols)]
  
  n = nrow(wtr)
  w.n = rep(NA, n)
  
  wtr.mat = as.matrix(wtr[,-1])
  dimnames(wtr.mat) <- NULL
  
  for(i in 1:n){
    #check we have all the data necessary
    if(any(is.na(wtr.mat[i,])) || is.na(wnd[i,2])){
      next
    }
    
    m.d = meta.depths(wtr.mat[i,], depths, seasonal=seasonal)
    if(any(is.na(m.d))){
      next
    }
    
    #Need epi and hypo density for wedderburn.number calc
    epi.dens = layer.density(0, m.d[1], wtr.mat[i,], depths, bathy$areas, bathy$depths)
    hypo.dens = layer.density(m.d[2], max(depths), wtr.mat[i,], depths, bathy$areas, bathy$depths)
    
    
    uS = uStar(wnd[i,2], wnd.height, epi.dens)
    
    #St = schmidt.stability(wtr.mat[i,], depths, bathy$areas, bathy$depths)
    
    #thermo.depth <- function(wtr, depths, Smin = 0.1){\
    #l.n[i] = lake.number(bathy$areas, bathy$depths, uS, St, m.d[1], m.d[2], hypo.dens)
    
    w.n[i] = wedderburn.number(hypo.dens - epi.dens, m.d[1], uS, Ao, hypo.dens)
    
  }
  
  output = data.frame(datetime=get.datetime(wtr), wedderburn.number=w.n)
  
  return(output)
}




#' Calculate volume-weighted average water temperature across a range of depths
#' for a timeseries.
#' 
#' Function for simplifying the calculation of Wedderburn Number. Can usually
#' be called directly on data loaded directly using \code{\link{load.ts}} and
#' \code{\link{load.bathy}}.
#' 
#' 
#' @param wtr A data frame of water temperatures (in Celsius). Loaded using
#' \code{\link{load.ts}}
#' @param top Either a single numeric depth value to be used across the entire
#' timeseries, or a vector of same length as the timeseries (e.g.,
#' \code{nrow(wtr)}). This is useful when calculating a time-varying layer
#' average, like average epilimnion temperature.
#' @param bottom Either a single numeric depth value to be used across the
#' entire timeseries, or a vector of same length as the timeseries (e.g.,
#' \code{nrow(wtr)}). This is useful when calculating a time-varying layer
#' average, like average epilimnion temperature.
#' @param bathy A data frame containing hypsometric data. Loaded using
#' \code{\link{load.bathy}}
#' @param na.rm Boolean indicated if step-by-step removal of NA's should be
#' tried. If false, a timestep with any NA values will return an NA value. If
#' true, best effort will be made to calculate indices despite NA values.
#' @return Returns a data frame with the timeseries of the average layer
#' temperature. Includes \sQuote{datetime} and \sQuote{layer.temp} columns.
#' @seealso \code{layer.temperature}
#' @keywords arith
#' @examples
#' 
#' 
#' 	#Get the path for the package example file included
#' 	wtr.path <- system.file('extdata', 'Sparkling.daily.wtr', package="rLakeAnalyzer")
#' 	bathy.path <- system.file('extdata', 'Sparkling.bth', package="rLakeAnalyzer")
#' 	
#' 	#Load data for example lake, Sparkilng lake, in Wisconsin.
#' 	sp.wtr = load.ts(wtr.path)
#' 	sp.bathy = load.bathy(bathy.path)
#' 	
#' 	
#'     l.t = ts.layer.temperature(sp.wtr, 0, 18, sp.bathy)
#' 	plot(l.t$datetime, l.t$layer.temp, type='l', 
#'         ylab='Volumetrically averaged lake temperature', xlab='Date')
#' 	
#' @export
ts.layer.temperature <- function(wtr, top, bottom, bathy, na.rm=FALSE){
  
  depths = get.offsets(wtr)
  
  n = nrow(wtr)
  
  #We can repeat one depth across the whole timeseries, or accept a vector
  # the same length as the timeseries (useful for epi and meta depths for example)
  if(length(top) == 1){
	  top = rep(top, n)
  } else if(length(top) == n){
	  #Do nothing
  } else {
	  stop('top depth must be of either length one or the same length as the timeseries')
  }
  
  if(length(bottom) == 1){
	  bottom = rep(bottom, n)
  } else if(length(bottom) == n){
	  #Do nothing
  } else {
	  stop('bottom depth must be of either length one or the same length as the timeseries')
  }
  
  wtr.mat = as.matrix(wtr[,-1])
  
  l.t = rep(NA, n)
  
  for(i in 1:n){
    if(is.na(top[i]) || is.na(bottom[i])){
    	l.t[i] = NA #nothing we can do with NA bottom or top depths
    }else{
      if(na.rm){
        temps = wtr.mat[i,]
        notNA = !is.na(temps)
        if(any(notNA)){
          l.t[i] = layer.temperature(top[i], bottom[i], temps[notNA], depths[notNA], bathy$areas, bathy$depths)
        }else{
          l.t[i] = NA
        }
      }else{
    	  l.t[i] = layer.temperature(top[i], bottom[i], wtr.mat[i,], depths, bathy$areas, bathy$depths)
      }
    }
  }
  
  return(data.frame(datetime=get.datetime(wtr), layer.temp=l.t))
}

#' @title Calculate physical indices for a timeseries.
#' 
#' @description Functions for simplifying the calculation of physical indices for a
#' timeseries of observation data.  Can usually be called directly on data
#' loaded directly using \code{\link{load.ts}} and \code{\link{load.bathy}}.
#' 
#' These are wrapper functions that accept a timeseries of data and call the
#' core physical metric functions (like \code{\link{schmidt.stability}}) on
#' each timestep.
#' 
#' @family Timeseries functions for r Lake Analyzer
#' 
#' @param wtr A data frame of water temperatures (in Celsius). Loaded using
#' \code{\link{load.ts}}. Must have columns \code{datetime}, \code{wtr_##.#}
#' where ##.# is depth in meters.
#' @param bathy A data frame containing hypsometric data. Loaded using
#' \code{\link{load.bathy}}
#' @param na.rm Boolean indicated if step-by-step removal of NA's should be
#' tried. If false, a timestep with any NA values will return an NA value. If
#' true, best effort will be made to calculate indices despite NA values.
#' @return Returns a data frame with the timeseries of calculated derivatives.
#' All include a \sQuote{datetime} column, but derivative columns differ
#' between functions.
#' @seealso For loading input data \code{\link{load.ts}},
#' \code{\link{load.bathy}}.
#' 
#' For the underlying functions operating at each timestep
#' \code{\link{meta.depths}}, \code{\link{thermo.depth}},
#' \code{\link{schmidt.stability}}, \code{\link{lake.number}},
#' \code{\link{internal.energy}}.
#' @keywords manip
#' @export
ts.internal.energy <- function(wtr, bathy, na.rm=FALSE){
	
	depths = get.offsets(wtr)
	
	n = nrow(wtr)
	i.e = rep(NA, n)
	
	wtr.mat = as.matrix(wtr[,-1])
	dimnames(wtr.mat) <- NULL
	
	for(i in 1:n){
		if(na.rm){
			temps = wtr.mat[i,]
			if(all(is.na(temps))){
				next
			}
			notNA = !is.na(temps)
		  i.e[i] = internal.energy(temps[notNA], depths[notNA], bathy$areas, bathy$depths)
		}else{
			if(any(is.na(wtr.mat[i,]))){
				i.e[i] = NA
				next
			}
			i.e[i] = internal.energy(wtr.mat[i,], depths, bathy$areas, bathy$depths)
		}
	}
	
	output = data.frame(datetime=get.datetime(wtr), internal.energy=i.e)
	
	return(output)
}

