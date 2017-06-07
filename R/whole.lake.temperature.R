#' @title Get volumetrically averaged whole lake temperature
#'
#' @description Calculates volumetrically weighted average whole lake temperature using
#' the supplied water temperature timeseries.
#'
#' @param wtr a numeric vector of water temperature in degrees C.
#' @param depths a numeric vector corresponding to the depths (in m) of the wtr measurements
#' @param bthA a numeric vector of cross sectional areas (m^2) corresponding to bthD depths
#' @param bthD a numeric vector of depths (m) which correspond to areal measures in bthA
#'
#' @import rLakeAnalyzer
#'
#' @seealso \code{\link{hypo.temperature}}, \code{\link{epi.temperature}}
#' 
#' @export
whole.lake.temperature <- function(wtr, depths, bthA, bthD){
	
	
	avg_temp = layer.temperature(0,max(depths), wtr=wtr, depths=depths, bthA=bthA, bthD=bthD)
	
	return(avg_temp)
}
