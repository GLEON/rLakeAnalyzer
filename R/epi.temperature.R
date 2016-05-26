#'@title Get volumetrically averaged epilimnion temp
#'@details Calculates volumetrically weighted average epilimnetic temperature using
#'the supplied water temperature timeseries. If the lake is not stratified, the bottom 
#'of the epilimnion is calculated as the full depth of the lake. 
#'
#'@param wtr Data frame of water temperature loaded with \code{\link{load.ts}}.
#'@param depths 
#'
#'
#'@seealso \code{\link{hyp.temperature}}, \code{\link{whole.lake.temperature}}
#'
#'@export
epi.temperature <- function(wtr, depths, bthA, bthD){
	
	md = rLakeAnalyzer::meta.depths(wtr, depths)
	
	if(is.na(md[1])){
		avg_temp = layer.temperature(0,max(depths), wtr=wtr, depths=depths, bthA=bthA, bthD=bthD)
	}else{
		avg_temp = layer.temperature(0,md[1], wtr=wtr, depths=depths, bthA=bthA, bthD=bthD)
	}
	return(avg_temp)
}
