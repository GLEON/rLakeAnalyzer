#Timeseries functions for r Lake Analyzer

ts.meta.depths <- function(wtr, slope=0.1){
  
  depths = get.offsets(wtr)
  
  n = nrow(wtr)
  
  wtr.mat = as.matrix(wtr[,-1])
  
  m.d = matrix(NA, nrow=n, ncol=2)
  
  for(i in 1:n){
    m.d[i,] = meta.depths(wtr.mat[i,], depths, slope)
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


