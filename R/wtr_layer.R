#' @export
#' @title Exploration of lake water column layers
#' @param thres error norm; defaults to 0.1
#' @param z0 initial depth in metres. Defaults to 2.5m
#' @param zmax maximum depth in metres: defaults to 150m
#' @param depth depth in metres; should be an increasing vector
#' @param measure parameter measured in the water column profile
#' @param nseg optional parameter to define the number of segments a priori; defaults to an unconstrained approach whereby the algorithm determines segmentations by minimzing the error norm over each segment
#' @return a dataframe of nseg (number of segments), mld (mix layer depth), cline (the midpoint of the segment connecting inflection points that has the maximum slope; thermocline for temperature measures)
#' @description  Extract water column parameters of a given parameter from a profile using the split-and-merge algorithm. The cline is defined as the midpoint of the layer of water where the physical property change in the greatest over a small difference. The exact cline depends on the specification of measure. For example if temperature is specified,  then we can expect cline to output the thermocline. 
#' @references Thomson, R. and I. Fine. 2003. Estimating Mixed Layer Depth from Oceanic Profile Data. Journal of Atmospheric and Oceanic Technology. 20(2), 319-329.
#' @examples
#' wtr_layer(depth=latesummer$depth, measure = latesummer$temper)
#' 
#' wtr_layer(depth=latesummer$depth, measure = latesummer$temper, nseg=4)
#' 
## Note accounting for difference between interval (nimax=neg-1) and segments (nseg=nimax+1)  
#' @seealso \code{cline_calc()}, \code{depth_filter()}
wtr_layer <-
  function(thres = 0.1,
           z0 = 2.5,
           zmax = 150,
           depth = depth,
           measure = measure,
           nseg = "unconstrained") {
    
    ## TODO need to be figure out a better threshold of points for algorithm
    if( length(depth) <= 30 ) {  
        warning("Profile does not have enough readings for sm algorithm (<30): returning NA")
        return(data.frame(min_depth = NA, nseg = NA, mld = NA, cline = NA))
      } 

      
      if (nseg == "unconstrained") {
        sam_list = by_s_m(thres = thres, z0 = z0, zmax = zmax, z = depth, sigma = measure)
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
  }