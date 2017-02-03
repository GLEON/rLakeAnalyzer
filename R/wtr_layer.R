#' @export
#' @title Exploration of lake water column layers
#' @param thres error norm; defaults to 0.1
#' @param z0 initial depth in metres; defaults to 2.5m
#' @param zmax maximum depth in metres: default to 150m
#' @param depth depth in metres; should be an increasing vector
#' @param measure parameter measured in the water column profile
#' @param optional parameter to define the number of segments a priori; defaults to an unconstrained approach whereby the algorithm determines segmentations by minimzing the error norm over each segment
#' @return a dataframe of nseg (number of segments), mld (mix layer depth), maxbd (the midpoint of the segment connecting inflection points that has the maximum slope; thermocline for temperature measures)
#' @description  Extract water column parameters of a given parameter from a profile using the split-and-merge algorithm.
#' @references Thomson & Fine. 2003. Estimating Mixed Layer Depth from Oceanic Profile Data. Journal of Atmospheric and Oceanic Technology. 20(2), 319-329.
#' @examples
#' wtr_layer(depth=latesummer$depth, measure = latesummer$temper)
#' wtr_layer(depth=latesummer$depth, measure = latesummer$temper, nseg=4)
#' 
## Problem! Unconstrained and nseg specify not matching.
wtr_layer <- function(thres=0.1,z0=2.5,zmax=150,depth=depth,measure=measure, nseg="unconstrained"){
  
  if (nseg=="unconstrained"){
    sam_list = by_s_m(thres=thres,z0=z0,zmax=zmax,z=depth,sigma=measure)
    return(data.frame(nseg=sam_list[["nimax"]], mld=sam_list[["by_s_m"]], maxbd=sam_list[["maxbd"]])) ##maybe maxbd could be called "cline"
    }
  else {
    sam_list = by_s_m3(nr=nseg,z0=z0,zmax=zmax,z=depth,sigma=measure)
    return(data.frame(nseg=nseg, mld=sam_list[["by_s_m"]], maxbd=sam_list[["maxbd"]]))
  }
}