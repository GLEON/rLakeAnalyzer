#' @title Get volumetrically averaged epilimnion temp
#' 
#' @description Calculates volumetrically weighted average epilimnetic temperature using the
#' supplied water temperature timeseries. If the lake is not stratified, the
#' bottom of the epilimnion is calculated as the full depth of the lake.
#' 
#' 
#' @param wtr a numeric vector of water temperature in degrees C.
#' @param depths a numeric vector corresponding to the depths (in m) of the wtr
#' measurements
#' @param bthA a numeric vector of cross sectional areas (m^2) corresponding to
#' bthD depths
#' @param bthD a numeric vector of depths (m) which correspond to areal
#' measures in bthA
#' @seealso \code{\link{hypo.temperature}} \code{\link{whole.lake.temperature}}
#' @export
epi.temperature <- function(wtr, depths, bthA, bthD){
	
	md = rLakeAnalyzer::meta.depths(wtr, depths)
	
	if(is.na(md[1])){
		avg_temp = layer.temperature(0,max(depths), wtr=wtr, depths=depths, bthA=bthA, bthD=bthD)
	}else{
		avg_temp = layer.temperature(0,md[1], wtr=wtr, depths=depths, bthA=bthA, bthD=bthD)
	}
	return(avg_temp)
}
