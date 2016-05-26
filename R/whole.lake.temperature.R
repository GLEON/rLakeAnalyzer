#'@title Get volumetrically averaged whole lake temperature
#'
#'@details Calculates volumetrically weighted average whole lake temperature using
#'the supplied water temperature timeseries.
#'
#'@param wtr Data frame of water temperature loaded with \code{\link{load.ts}}.
#'
#'@import rLakeAnalyzer
#'
#'@seealso \code{\link{sim_metrics}}, \code{\link{compare_to_field}}, \code{\link{validate_sim}}
#'@export
whole.lake.temperature <- function(wtr, depths, bthA, bthD){
	
	
	avg_temp = layer.temperature(0,max(depths), wtr=wtr, depths=depths, bthA=bthA, bthD=bthD)
	
	return(avg_temp)
}
