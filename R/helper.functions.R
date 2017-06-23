## Helper functions for Lake Analyzer R

datetime.pattern = "(datetime|timestamp|time|date)"


#' @title Gets depths from data frame containing profile info.
#' 
#' @description 
#' Extracts the depth information from a data frame containing multi-depth observation data.
#' Relies on the format of the header to get information and may fail if your file format is incorrect.
#' Please follow 'VAR_##.#' format, where ##.# is the depth of data for that column. VAR is typically
#' 'wtr' to indicate water temperature. 
#' 
#' @param data Data frame returned from \code{\link{load.ts}}.
#' 
#' 
#' @return 
#' A numeric vector of depth values. Should be the \code{ncol(data) - 1} 
#' in length as the first column contains date/time data.
#' 
#' 
#' @seealso \code{\link{load.ts}}
#' 
#' @examples 
#' 
#' #Get the path for the package example file included
#' exampleFilePath <- system.file('extdata', 'Sparkling.wtr', package="rLakeAnalyzer")
#' 
#' #Load
#' sparkling.temp = load.ts(exampleFilePath)
#' 
#' #get the lake depths associated with each column
#' depths = get.offsets(sparkling.temp)
#' 
#' print(depths)
#' 
#' @keywords manip
#' 
#' @export
get.offsets <- function(data){
  
  header = names(drop.datetime(data))
  
  #check for existence of datetime header and drop if there
  dt_indx = grep(pattern= "datetime", x= header, ignore.case= TRUE)
  if(length(dt_indx) > 0){
  	header = header[-dt_indx] #Drop datetime
  }
  
  #match anything digits after the last _ (at the end of the line)
  matches = regexpr("_\\d+\\.?\\d*$" ,header)
  
  lengths = attr(matches,'match.length')
  offsets = vector(mode="numeric", length=length(matches))
  
  for(i in 1:length(matches)){
    offsets[i] = as.numeric(substr(header[i], matches[i]+1, matches[i] + lengths[i]))
  }
  
  if(any(is.na(offsets))){
    warning('Problem determining variable depths from column names.
Please use the \'var_#.#\' format for your data.frame header.' )
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


#@title Find and drop the datetime column from the datatable
#
#description Liberally looks for a datetime column and drops it, 
#returning a data.frame with only water temperature. Errors if datetime column is 
#ambiguous. Warns if there is no match.
#
#@return A data.frame with only the data, after datetime has been dropped
#
drop.datetime = function(data, error=FALSE){
	
	header = names(data)
	dt_indx = grep(datetime.pattern, header, ignore.case=TRUE)
	
	if(length(dt_indx) < 1){
		if(error){
			stop('Unable to find a datetime column. Datetime column was supplied.')
		}else{
			warning('Unable to find a datetime column. Assuming no datetime column was supplied.')
			return(data)
		}
		
	}else if(length(dt_indx) > 1){
		stop('datetime column ambiguity. You can only have one column of datetime.')
	}
	
	return(data[,-dt_indx, drop=FALSE])
}

#@title Search for and return the datetime column from a ts data.frame
#
#Warns if unavailable then returns NULL.
#
get.datetime = function(data, error=FALSE){
	
	header = names(data)
	dt_indx = grep(datetime.pattern, header, ignore.case=TRUE)
	
	if(length(dt_indx) < 1){
		if(error){
			stop('Unable to find a datetime column.')
		}else{
			warning('Unable to find a datetime column, attempting to ignore.')
			return(NULL)
		}
	}else if(length(dt_indx) > 1){
		stop('datetime column ambiguity. You can only have one column of datetime.')
	}
	
	return(data[,dt_indx])
}
