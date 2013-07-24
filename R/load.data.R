## Data load functions

load.bathy <- function(fPath){
  
  d = read.table(fPath, sep=c('\t', ','), header=TRUE)
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

load.ts <- function(fPath){
  #Load data
  d = read.table(fPath, sep='\t', header=TRUE)
  
  #Just standardize all headers as lowercase
  names(d) = tolower(names(d))
  
  #convert column to a real date/time format
  d$datetime = as.POSIXct(d$datetime)
  
  return(d)
}