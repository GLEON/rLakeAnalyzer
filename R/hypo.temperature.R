#' @title Get volumetrically averaged hypolimnion temp
#' 
#' @description Calculates volumetrically weighted average hypolimnetic temperature using
#' the supplied water temperature timeseries. If the lake is not stratified, an NA
#' value is returned. 
#'
#' @param wtr a numeric vector of water temperature in degrees C.
#' @param depths a numeric vector corresponding to the depths (in m) of the wtr measurements
#' @param bthA a numeric vector of cross sectional areas (m^2) corresponding to bthD depths
#' @param bthD a numeric vector of depths (m) which correspond to areal measures in bthA
#'
#'
#' @seealso \code{\link{epi.temperature}}, \code{\link{whole.lake.temperature}}
#'
#' @export
hypo.temperature <- function(wtr, depths, bthA, bthD){
	
	md = rLakeAnalyzer::meta.depths(wtr, depths)
	
	if(is.na(md[1])){
		avg_temp = NA
	}else{
		avg_temp = layer.temperature(md[2],max(depths), wtr=wtr, depths=depths, bthA=bthA, bthD=bthD)
	}
	return(avg_temp)
}
