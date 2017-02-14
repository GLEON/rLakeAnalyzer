#' @export
#' @title Exploration of lake water column layers
#' @param thres error norm; defaults to 0.1
#' @param z0 initial depth in metres; defaults to auto whereby z0 is calculate as the first value of the longest ordered portion of the depth vector to minimum of 1. 
#' @param zmax maximum depth in metres: defaults to 150m
#' @param depth depth in metres; should be an increasing vector
#' @param measure parameter measured in the water column profile
#' @param nseg optional parameter to define the number of segments a priori; defaults to an unconstrained approach whereby the algorithm determines segmentations by minimzing the error norm over each segment
#' @param depth_filter option to turn of depth_filter process. 
#' @return a dataframe of nseg (number of segments), mld (mix layer depth), cline (the midpoint of the segment connecting inflection points that has the maximum slope; thermocline for temperature measures)
#' @description  Extract water column parameters of a given parameter from a profile using the split-and-merge algorithm. The cline is defined as the midpoint of the layer of water where the physical property change in the greatest over a small difference. The exact cline depends on the specification of measure. For example if temperature is specified,  then we can expect cline to output the thermocline. 
#' @references Thomson, R. and I. Fine. 2003. Estimating Mixed Layer Depth from Oceanic Profile Data. Journal of Atmospheric and Oceanic Technology. 20(2), 319-329.
#' @examples
#' Estimation using by_s_m() and an unspecified number of segments
#' wtr_layer(depth=latesummer$depth, measure = latesummer$temper)
#' 
#' Estimation using by_s_m3() and a fixed number of segments
#' wtr_layer(depth=latesummer$depth, measure = latesummer$temper, nseg=4)
#' 
## Note accounting for difference between interval (nimax=neg-1) and segments (nseg=nimax+1)  
#' @seealso \code{cline_calc()}, \code{depth_filter()}
wtr_layer <-
  function(thres = 0.1,
           z0 = "auto",
           zmax = 150,
           depth = depth,
           measure = measure,
           nseg = "unconstrained",
           depth_filter=TRUE) {
    
    if( length(depth) >= 10 ) {  
      
      if( depth_filter=="TRUE" ){
      
      ## Remove heave, soak and upcast
      depth=depth[depth_filter(depth)]
      measure=measure[depth_filter(depth)]
      
      } else { 
        depth=depth
        measure=measure}


        
      ## Auto detecting minimum of depth vector to a minimum of 1
      if (z0 == "auto") {
        ## What is the minimum depth after finding longest ordered portion?
        z0 = min(depth)
        ##z0 must have minimum of 1 if using auto
        if (z0 < 1) {
          z0 = 1
        }
      } else {
        z0 = z0
      }
      
      
      if (nseg == "unconstrained") {
        sam_list = by_s_m(thres = thres, z0 = z0, zmax = zmax, z = depth, sigma = measure )
        return(data.frame(
          min_depth = z0,
          nseg = sam_list[["nimax"]] + 1,
          mld = sam_list[["by_s_m"]],
          cline = cline_calc(z_seg = sam_list[["smz"]], sigma_seg = sam_list[["sms"]])
          )
        )
      }
      else {
        sam_list = by_s_m3(nr = nseg - 1, z0 = z0, zmax = zmax, z = depth, sigma = measure)
        return(data.frame(
          min_depth = z0,
          nseg = nseg,
          mld = sam_list[["by_s_m"]],
          cline = cline_calc(z_seg = sam_list[["smz"]], sigma_seg = sam_list[["sms"]])
        ))
      }
    } else {
      warning("Profile does not have enough readings for sm algorithm (<10)")
      return(data.frame(min_depth = NA, nseg = NA, mld = NA, cline = NA))
    }
  }