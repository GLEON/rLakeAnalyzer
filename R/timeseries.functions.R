#Timeseries functions for r Lake Analyzer

ts.meta.depths <- function(wtr, slope=0.1, seasonal=TRUE, na.rm=FALSE){
  
  depths = get.offsets(wtr)
  
  n = nrow(wtr)
  
  wtr.mat = as.matrix(wtr[,-1])
  
  m.d = matrix(NA, nrow=n, ncol=2)
  
  for(i in 1:n){
    if(na.rm){
      temps = wtr.mat[i,]
      notNA = !is.na(temps)
      m.d[i,] = meta.depths(temps[notNA], depths[notNA], slope, seasonal=seasonal) # Assume seasonal thermoD start
    }else{
      m.d[i,] = meta.depths(wtr.mat[i,], depths, slope, seasonal=seasonal) # Assume seasonal thermoD start
    }
  }
  
  return(data.frame(datetime=wtr$datetime, top=m.d[,1], bottom=m.d[,2]))

}

ts.thermo.depth <- function(wtr, Smin = 0.1, seasonal=TRUE, na.rm=FALSE){
  
  depths = get.offsets(wtr)
  
  n = nrow(wtr)
  t.d = rep(NA, n)
  
  wtr.mat = as.matrix(wtr[,-1])
  dimnames(wtr.mat) <- NULL
  
  for(i in 1:n){
    if(na.rm){
      temps = wtr.mat[i,]
      notNA = !is.na(temps)
      t.d[i] = thermo.depth(temps[notNA], depths[notNA], seasonal=seasonal)
    }else{
      if(any(is.na(wtr.mat[i,]))){
        t.d[i] = NA
        next
      }
      #thermo.depth <- function(wtr, depths, Smin = 0.1){\
      t.d[i] = thermo.depth(wtr.mat[i,], depths, seasonal=seasonal)
    }
  }

  output = data.frame(datetime=wtr$datetime, thermo.depth=t.d)
  
  return(output)
}

ts.schmidt.stability <- function(wtr, bathy, na.rm=FALSE){
	
	depths = get.offsets(wtr)
	
	n = nrow(wtr)
	s.s = rep(NA, n)
	
	wtr.mat = as.matrix(wtr[,-1])
	dimnames(wtr.mat) <- NULL
	
	for(i in 1:n){
    if(na.rm){
      temps = wtr.mat[i,]
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
	
	output = data.frame(datetime=wtr$datetime, schmidt.stability=s.s)
	
	return(output)
	
}

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
	
	output = data.frame(datetime=wtr$datetime, lake.number=l.n)
	
	return(output)
}


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
	
	output = data.frame(datetime=wtr$datetime, uStar=uStar)
	
	return(output)
}


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
  
  output = data.frame(datetime=wtr$datetime, wedderburn.number=w.n)
  
  return(output)
}


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
  
  return(data.frame(datetime=wtr$datetime, layer.temp=l.t))
}

