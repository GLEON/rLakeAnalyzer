#Timeseries functions for r Lake Analyzer

ts.meta.depths <- function(wtr, slope=0.1){
  
  depths = get.offsets(wtr)
  
  n = nrow(wtr)
  
  wtr.mat = as.matrix(wtr[,-1])
  
  m.d = matrix(NA, nrow=n, ncol=2)
  
  for(i in 1:n){
    m.d[i,] = meta.depths(wtr.mat[i,], depths, slope) # Assume seasonal thermoD start
  }
  
  return(data.frame(datetime=wtr$datetime, top=m.d[,1], bottom=m.d[,2]))

}

ts.thermo.depth <- function(wtr, Smin = 0.1, seasonal=TRUE){
  
  depths = get.offsets(wtr)
  
  n = nrow(wtr)
  t.d = rep(NA, n)
  
  wtr.mat = as.matrix(wtr[,-1])
  dimnames(wtr.mat) <- NULL
  
  for(i in 1:n){
    if(any(is.na(wtr.mat[i,]))){
      t.d[i] = NA
      next
    }
    #thermo.depth <- function(wtr, depths, Smin = 0.1){\
    t.d[i] = thermo.depth(wtr.mat[i,], depths, seasonal=seasonal)
  }

  output = data.frame(datetime=wtr$datetime, thermo.depth=t.d)
  
  return(output)
}

ts.schmidt.stability <- function(wtr, bathy){
	
	depths = get.offsets(wtr)
	
	n = nrow(wtr)
	s.s = rep(NA, n)
	
	wtr.mat = as.matrix(wtr[,-1])
	dimnames(wtr.mat) <- NULL
	
	for(i in 1:n){
		if(any(is.na(wtr.mat[i,]))){
			s.s[i] = NA
			next
		}
		#thermo.depth <- function(wtr, depths, Smin = 0.1){\
		s.s[i] = schmidt.stability(wtr.mat[i,], depths, bathy$areas, bathy$depths)
	}
	
	output = data.frame(datetime=wtr$datetime, schmidt.stability=s.s)
	
	return(output)
	
}

ts.lake.number <- function(wtr, wnd, wnd.height, bathy){
	
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
		
		m.d = meta.depths(wtr.mat[i,], depths)
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


ts.uStar <- function(wtr, wnd, wnd.height, bathy){
	
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
		
		m.d = meta.depths(wtr.mat[i,], depths)
		if(any(is.na(m.d))){
			next
		}
		
		epi.dens = layer.density(0, m.d[1], wtr.mat[i,], depths, bathy$areas, bathy$depths)
		
		
		uStar[i] = uStar(wnd[i,2], wnd.height, epi.dens)
	}
	
	output = data.frame(datetime=wtr$datetime, uStar=uStar)
	
	return(output)
}


ts.wedderburn.number <- function(wtr, wnd, wnd.height, bathy, Ao){
  
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
    
    m.d = meta.depths(wtr.mat[i,], depths)
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


