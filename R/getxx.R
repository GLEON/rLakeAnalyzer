#' @export
#' @title Linear interpolation of the Input Data Series
#' @description Linear interpolation of the Input Data Series
#'
#' @param x0 [REAL] start point along the x-axis
#' @param x [REAL(N)] input x-axis array (should be in "chronological" order)
#' @param y [REAL(N)] input y-axis array
#'
#' @return Interpolation of both x and y

getxx <- function(x,y, x0) {
  yy <- rep(0,length(x0))
  j <- 1
  for (i in 1:length(x0) ) {
    ## !!!The original code has a bug in the guard j<length(x)!!!
    ## !!!Also > was corrected to >= !!!
    while( (x0[i] >= x[j]) && (j < length(x)) ) {
      j <- j+1
    }
    if( j == 1 ) {
      yy[i] <- y[1]
    }
    else {
      yy[i] <- y[j-1]+(x0[i]-x[j-1])/(x[j]-x[j-1])*(y[j]-y[j-1])
    }
  }
  yy
}
