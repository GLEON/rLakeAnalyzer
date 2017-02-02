#' @export
#' @title Exploration of lake water column segments
#' @param thres error norm; defaults to 0.1
#' @param z0 initial depth in metres; defaults to 2.5m
#' @param zmax maximum depth in metres: default to 150m
#' @param depth depth in metres; should be an increasing vector
#' @param measure parameter measured in the water column profile
#' @param optional parameter to define the number of segments a priori; defaults to an unconstrained approach whereby the algorithm determines segmentations by minimzing the error norm over each segment
#' @return a dataframe of nseg (number of segments) and the x and y coordinates of the segments produced by the split and merge approach.
#' @description  Extract water column segments of a given parameter from a profile using the split-and-merge algorithm.
#' @references Thomson & Fine. 2003. Estimating Mixed Layer Depth from Oceanic Profile Data. Journal of Atmospheric and Oceanic Technology. 20(2), 319-329.
#' @examples
#' wtr_segments(depth=latesummer$depth, measure = latesummer$temper)
#' wtr_segments(depth=latesummer$depth, measure = latesummer$temper, nseg=3)
#' 

## Need to add +1 to nimax to get actual number of segments
wtr_segments <- function(thres=0.1,z0=2.5,zmax=150,depth=depth,measure=measure, nseg="unconstrained"){
  
  if (nseg=="unconstrained"){
    sam_list = by_s_m(thres=thres,z0=z0,zmax=zmax,z=depth,sigma=measure)
    return(data.frame(nseg=sam_list[["nimax"]]+1, depth=sam_list[["smz"]], measure=sam_list[["sms"]]))
  }
  else {
    sam_list = by_s_m3(nr=nseg,z0=z0,zmax=zmax,z=depth,sigma=measure)
    return(data.frame(nseg=nseg+1, depth=sam_list[["smz"]], measure=sam_list[["sms"]]))
  }
}