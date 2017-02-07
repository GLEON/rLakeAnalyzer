#' @export
#' @title Exploration of lake water column segments
#' @param thres error norm; defaults to 0.1
#' @param z0 initial depth in metres; defaults to auto whereby z0 is calculate as the first value of the longest ordered portion of the depth vector to minimum of 1. 
#' @param zmax maximum depth in metres: default to 150m
#' @param depth depth in metres; should be an increasing vector
#' @param measure parameter measured in the water column profile
#' @param nseg optional parameter to define the number of segments a priori; defaults to an unconstrained approach whereby the algorithm determines segmentations by minimzing the error norm over each segment
#' @return a dataframe of nseg (number of segments) and the x and y coordinates of the segments produced by the split and merge approach.
#' @description  Extract water column segments of a given parameter from a profile using the split-and-merge algorithm.
#' @references Thomson & Fine. 2003. Estimating Mixed Layer Depth from Oceanic Profile Data. Journal of Atmospheric and Oceanic Technology. 20(2), 319-329.
#' @examples
#' wtr_segments(depth=latesummer$depth, measure = latesummer$temper)
#' wtr_segments(depth=latesummer$depth, measure = latesummer$temper, nseg=4)
#' 

## Note accounting for difference between interval (nimax=neg-1) and segments (nseg=nimax+1)  
wtr_segments <- function(thres=0.1,z0="auto",zmax=150,depth=depth,measure=measure, nseg="unconstrained"){
  
  
  ## Index numbers of longest ordered portion of a vector
   ## http://stackoverflow.com/a/42077739/5596534
   order_seq <- function(x) {
     s = 1L + c(0L, which( x[-1L] < x[-length(x)] ), length(x))
     w = which.max(diff(s))
     return(s[w]:(s[w+1]-1L))
   }
   
   ## For manual setting of depth vector
   if( z0 == "auto" ){
     z0 = depth[min(order_seq(depth))]
     ##z0 must have minimum of 1 if using auto
     if( z0 < 1 ){
       z0 = 1
     } 
   } else {z0=z0}
  
  if( nseg=="unconstrained" ){
    sam_list = by_s_m(thres=thres,z0=z0,zmax=zmax,z=depth,sigma=measure)
    return(data.frame(min_depth=z0,nseg=sam_list[["nimax"]]+1, depth=sam_list[["smz"]], measure=sam_list[["sms"]]))
  }
  else {
    sam_list = by_s_m3(nr=nseg-1,z0=z0,zmax=zmax,z=depth,sigma=measure)
    return(data.frame(min_depth=z0,nseg=nseg, depth=sam_list[["smz"]], measure=sam_list[["sms"]]))
  }
}