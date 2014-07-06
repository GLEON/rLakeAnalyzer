## Data load functions

load.bathy <- function(fPath){
  
  d = read.table(fPath, sep=c(','), header=TRUE)
  if(ncol(d) < 2){
  	d = read.table(fPath, sep=c('\t'), header=TRUE)
  }
  
  if(ncol(d) < 2){
  	stop('Error loading bathymetry file, check that it is a tab delimited file two columns.')
  }
  
  headNames = tolower(names(d))
  #names(d) = tolower(d)
  
  dI = grep("depths", headNames)
  aI = grep("areas", headNames)
  
  if(length(aI) < 1 || length(dI) < 1){
    stop('Bathymetry header must be labeled with "areas" and "depths".')
  }
  
  #Standardize names
  names(d)[dI] = 'depths'
  names(d)[aI] = 'areas'
  
  return(d)
}

load.ts <- function(fPath, tz='GMT'){
  #Load data
  d = read.table(fPath, sep='\t', header=TRUE, as.is=TRUE)
  
  #Just standardize all headers as lowercase
  names(d) = tolower(names(d))
  
  if( !any('datetime' %in% names(d)) ){
    stop('Timeseries file must be tab delimited and contain a column labeled \'datetime\'');
  }
  
  #n.unique.datetime = length(unique(d$datetime))
  
  #convert column to a real date/time format
  d$datetime = as.POSIXct(d$datetime, tz)
  
  #if(n.unique.datetime != unique(d$datetime)){
  #	stop('Error parsing datetimes. Check your format. No duplicate datetime values allowed.')
  #}
  
  return(d)
}