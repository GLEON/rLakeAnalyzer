#' @export
#' @title Data filter to remove soak, heave and upcast
#' @param z0 depth vector
#' @param run_length Length of run upon which to start the soak removal
#' @return index values of z0 of filtered data. Will return a warning if the function removed more than 10% of the data
#' @description  
#' \itemize{
#'  \item Soak period: water profiling instruments typically require a soak period where you let the instrument rest submerged at the surface. While it is "soaking" it is collecting data. We don't want that data
#'  \item Upcast versus downcast: typically instruments are turned on before you put them in the water and turn them off once you pull them out. The data consequence of that is that you collect both the "downcast" and the "upcast". In some case the upcast is of interest but usually it isn't. And because we would prefer increasing depth data it is better to remove an upcast if it is present. 
#'  \item Heave: when lowering the instrument in rough weather a boat will heave side to side. Sometimes it will heave enough that you get small data groupings where the decreases a little while the boat heaves then go down. The overall trend is still down but those slight upticks in depth cause problems for our algorithm.
#' }
#' @examples
#' depth_filter(z0=latesummer$depth)

depth_filter <- function(z0, run_length=20) {
  n_start = length(z0)
  
  ##REMOVES SOAK PERIOD
  ## s are where the runs start;  tack on length(z0)+1, where the next run would start if the vector continued
  ## subsequent runs start where there is a 
  s = 1L + c(0L, which(z0[-1L] < z0[-length(z0)]), length(z0))
  ## Index of first run of numbers greater than run_length (defaults to 20)
  
  if( length(s) > run_length ) {
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
  warning(paste0("Soak, heave and bottom data filter removed ",round(p_loss,2),"% of the data"))
  }
  
  return(idx_soak[idx_heave])
  
  } else {warning("run length less than filter depth length")}

}




