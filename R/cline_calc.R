#' @export
#' @title Calculate cline of series of segments
#' @param z_seg depth in metres; should be an increasing vector
#' @param sigma_seg parameter measured in the water column profile
#' @return the depth of the cline
#' @description  Cline depth is defined as the midpoint of the segment connecting inflection points that has the maximum slope (–dT/dz). A cline cannot occur over a depth range of less than 1m and also cannot be the shallowest segment. Violating both conditions will thrown warnings though this function handles both differently. Used mostly with \code{wtr_layer}
#' @references Fiedler, Paul C. "Comparison of objective descriptions of the thermocline." Limnology and Oceanography: Methods 8.6 (2010): 313-325.
#' @seealso 
#' \code{wtr_layer()}


cline_calc <- function(z_seg = z_seg, sigma_seg = sigma_seg) {
  ## index top of segment with greatest change of measure (sigma) over depth (z) - metalimnion
  top = which.max(diff(z_seg)/diff(sigma_seg))
  
  ## The cline can not be the top segment. If this is the case, kick it down one interval
  if( top == 1){
    warning('Algorithm calculates cline to be in top segment. This is likely due to surface scatter. Trying the next interval')
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