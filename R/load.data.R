## Data load functions



#' Import lake bathymetry data.
#' 
#' Imports lake bathymetry data. Bathymetric data file must be a 2 column array
#' where depth (in meters) and area (in meters^2) information are provided in
#' columns with headers containing the words "depths" and "areas" respectively.
#' 
#' 
#' @param fPath File path to the bathymetry file.
#' @return data.frame of depth and area for given lake.
#' @author Luke Winslow
#' @seealso \code{\link{load.ts}}
#' @keywords file
#' @examples
#' 
#'   #Get the path for the package example file included
#'   exampleFilePath <- system.file('extdata', 'Sparkling.bth', package="rLakeAnalyzer")
#'   
#'   #Load and plot the hypsometric curve
#'   sparkling.bathy = load.bathy(exampleFilePath)
#'   
#'   #If successful, there will be two colums. "depths", and "areas".
#'   plot(sparkling.bathy$areas, sparkling.bathy$depths, type='l', ylim=c(20,0), 
#'     ylab='Depths (m)', xlab='Areas (m^2)')
#'   
#' @export
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



#' Load timeseries from properly formatted text file.
#' 
#' A convenience function to load timeseries data into R based on the
#' standardized format used by Lake Analyzer.
#' 
#' Timeseries files must follow a common format. The first column must have the
#' label 'datetime' and be of the format \emph{yyyy-mm-dd HH:MM:SS} (ISO 8601
#' without the "T" delimiter). The second can be skipped if not using
#' sub-minute data.
#' 
#' @param fPath The file path as a string.
#' @param tz Timezone string to be supplied to \code{\link{as.POSIXct}}.
#' Defaults to GMT (UTC-0). This often can be left to the default unless
#' timezone support is specifically required.
#' @return A data frame in the required format for use with other rLakeAnalyzer
#' timeseries functions.
#' @author Luke Winslow
#' @seealso For dataloading \code{\link{ts.meta.depths}}, \cr For analyzing
#' timeseries data, see \code{\link{ts.meta.depths}},
#' \code{\link{ts.thermo.depth}}, \code{\link{ts.schmidt.stability}},
#' \code{\link{ts.lake.number}}.
#' @keywords file
#' @examples
#' 
#' 	#Get the path for the package example file included
#' 	exampleFilePath <- system.file('extdata', 'Sparkling.wtr', package="rLakeAnalyzer")
#' 	
#' 	#Load
#' 	sparkling.temp = load.ts(exampleFilePath)
#' 	
#' 	#calculate and plot the thermocline depth
#' 	t.d = ts.thermo.depth(sparkling.temp)
#' 	
#' 	plot(t.d$datetime, t.d$thermo.depth, type='l', ylab='Thermocline Depth (m)', xlab='Date')
#' @export
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
