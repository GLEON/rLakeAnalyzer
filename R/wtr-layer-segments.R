#' @export
#' @title Exploration of lake water column layers
#' @description  Extract water column parameters of a given parameter from a profile using the split-and-merge algorithm. The cline is defined as the midpoint of the layer of water where the physical property change in the greatest over a small difference. The exact cline depends on the specification of measure. For example if temperature is specified,  then we can expect cline to output the thermocline. 
#' @param thres error norm; defaults to 0.1
#' @param z0 initial depth in metres. Defaults to 2.5m
#' @param zmax maximum depth in metres: defaults to 150m
#' @param depth depth in metres; should be an increasing vector
#' @param measure parameter measured in the water column profile
#' @param nseg optional parameter to define the number of segments a priori; defaults to an unconstrained approach whereby the algorithm determines segmentations by minimzing the error norm over each segment
#' @return a list of two dataframes. layers: nseg (number of segments), mld (mix layer depth), cline (the midpoint of the segment connecting inflection points that has the maximum slope; thermocline for temperature measures) and segments calculated by the sm algorithm.

#' @references Thomson, R. and I. Fine. 2003. Estimating Mixed Layer Depth from Oceanic Profile Data. Journal of Atmospheric and Oceanic Technology. 20(2), 319-329.
#' @examples
#' df1 <- wtr_layer(depth=latesummer$depth, measure = latesummer$temper)
#' df1$layers
#' df1$segments
#' 
#' wtr_layer(depth=latesummer$depth, measure = latesummer$temper, nseg=4)
#' 


wtr_layer <-
  function(depth = depth,
           measure = measure,
           thres = 0.1,
           z0 = 2.5,
           zmax = 150,
           nseg = "unconstrained") {
    
    ## TODO need to be figure out a better threshold of points for algorithm
    if( length(depth) <= 30 ) {  
        warning("Profile does not have enough readings for sm algorithm (<30): returning NA")
        return(data.frame(min_depth = NA, nseg = NA, mld = NA, cline = NA))
    } 
    ## Note accounting for difference between interval (nimax=neg-1) and segments (nseg=nimax+1)  

      
      if (nseg == "unconstrained") {
        sam_list = by_s_m(thres = thres, z0 = z0, zmax = zmax, z = depth, sigma = measure)
        list(
          layers = data.frame(
            min_depth = z0,
            nseg = sam_list[["nimax"]] + 1,
            mld = sam_list[["by_s_m"]],
            cline = cline_calc(z_seg = sam_list[["smz"]], sigma_seg = sam_list[["sms"]])
          ),
          segments = data.frame(segment_depth = sam_list[["smz"]],
                                segment_measure = sam_list[["sms"]])
        )
      }
      else {
        sam_list = by_s_m3(nr = nseg - 1, z0 = z0, zmax = zmax, z = depth, sigma = measure)
        list(
          layers = data.frame(
            min_depth = z0,
            nseg = nseg,
            mld = sam_list[["by_s_m"]],
            cline = cline_calc(z_seg = sam_list[["smz"]], sigma_seg = sam_list[["sms"]])
          ),
          segments = data.frame(segment_depth = sam_list[["smz"]],
                                segment_measure = sam_list[["sms"]])
        )
      }
  }


#' @title Calculate cline of series of segments
#' @param z_seg depth in metres; should be an increasing vector
#' @param sigma_seg parameter measured in the water column profile
#' @return the depth of the cline
#' @description  Cline depth is defined as the midpoint of the segment connecting inflection points that has the maximum slope. A cline cannot occur over a depth range of less than 1m and also cannot be the shallowest segment. Violating both conditions will thrown warnings though this function handles both differently. Used mostly with \code{wtr_layer}
#' @references Fiedler, Paul C. Comparison of objective descriptions of the thermocline. Limnology and Oceanography: Methods 8.6 (2010): 313-325.
#' @keywords internal 
#' 
#' @seealso 
#' \code{wtr_layer()}
#' 
#' 
cline_calc <- function(z_seg = z_seg, sigma_seg = sigma_seg) {
  ## index top of segment with greatest change of measure (sigma) over depth (z) - metalimnion
  top = which.max(diff(z_seg)/diff(sigma_seg))
  
  ## The cline can not be the top segment. If this is the case, kick it down one interval
  if( top == 1){
    warning('Algorithm calculates cline to be in top segment. This is likely due to surface scatter. Using the next interval.')
    top = top + 1
  } else{top=top}
  
  
  ## index bottom of segment with greatest change of measure (sigma) over depth (z) - metalimnion
  bot = top+1
  
  ## if depth difference between two segments is lt 1 throw an error
  ## TODO: Also kick interval down one and 
  if( diff(z_seg[c(top,bot)]) <= 1 ){
    warning('depth range of segment with greatst change over depth is less than one. Likely not a cline.')
    cline=NA
  } else{ mean(z_seg[c(top,bot)]) }
  
}

#' @export
#' @title Data filter to remove soak, heave and upcast
#' @param z0 depth vector
#' @param run_length Length of run upon which to start the soak removal
#' @param index Logical: Should the function return an index value or actual value?
#' @return index values of z0 of filtered data. Will return a warning if the function removed more than 10% of the data
#' @description  
#' \itemize{
#'  \item Soak period: water profiling instruments typically require a soak period where you let the instrument rest submerged at the surface. While it is "soaking" it is collecting data. We don't want that data
#'  \item Upcast versus downcast: typically instruments are turned on before you put them in the water and turn them off once you pull them out. The data consequence of that is that you collect both the "downcast" and the "upcast". In some case the upcast is of interest but usually it isn't. And because we would prefer increasing depth data it is better to remove an upcast if it is present. 
#'  \item Heave: when lowering the instrument in rough weather a boat will heave side to side. Sometimes it will heave enough that you get small data groupings where the decreases a little while the boat heaves then go down. The overall trend is still down but those slight upticks in depth cause problems for our algorithm.
#' }
#' 
#' @keywords internal
#' depth_filter(z0=latesummer$depth)

depth_filter <- function(z0, run_length=20, index = FALSE) {
  n_start = length(z0)
  
  ##REMOVES SOAK PERIOD
  ## s are where the runs start;  tack on length(z0)+1, where the next run would start if the vector continued
  ## subsequent runs start where there is a 
  s = 1L + c(0L, which(z0[-1L] < z0[-length(z0)]), length(z0))
  ## Index of first run of numbers greater than run_length (defaults to 20)
  
  #if( length(s) > run_length ) {
  w = min(which(diff(s) >= run_length))
  
  ##Index from first run GTE 20
  start = s[w]
  ## TODO: fix the max depth identification
  end = which.max(z0)
  
  ## Index numbers of x of non soak
  idx_soak = start:end
  
  ## Depth values using soak index
  ## I'm sure there is a better way to do this.
  x1 = z0[idx_soak]
  
  ## Index of heave 
  idx_heave <- unique(Reduce(function(p,i) if(x1[i] > x1[p]) i else p, seq_along(x1), accumulate = TRUE))
  
  ## Percent data loss
  p_loss = 100-(length(z0[idx_heave])/n_start)*100
  
  if (p_loss>10){
    message(paste0("Soak, heave and bottom data filter removed ",round(p_loss,2),"% of the data"))
  }
  
  idx = idx_soak[idx_heave]
  
  if( index == TRUE ){
    return(idx)
  } else{
    return(z0[idx])
  }
  
}





