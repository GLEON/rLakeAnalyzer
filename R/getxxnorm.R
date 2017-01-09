
#' @description Interpolate and normalize a vector of samples. X and Y will be linearly interpolated to match the length of X0 and will be normalized so that the first point is (0,0) and the last point is (1,1). Vector x are the input x values, must be in ascending order.
#'
#' @title Interpolate and normalize a vector of samples.
#' @param x0 [REAL] start point along the x-axis
#' @param X [REAL(N)] input x-axis array (should be in "chronological" order)
#' @param Y [REAL(N)] input y-axis array
#'
#' @return Output is a data.frame with: anormx, the normalization value used to make the last x == 1. anormy, the normalization value used to make the last y == 1. xx,yy  vectors of x,y values interpolated to be the same length as x0.


getxxnorm <- function(x,y, x0) {
  y0 <- rep(0,length(x0))  # Reserve space for y values.
  j <- 1
  for (i in 1:length(x0) ) {
    while( (x0[i] >= x[j]) && (j < length(x)) ) {
      j <- j+1
    }
    if( j == 1 ) {
      y0[i] <- y[1]
    }
    else {
      y0[i] <- y[j-1]+(x0[i]-x[j-1])/(x[j]-x[j-1])*(y[j]-y[j-1])
    }
  }
  anormx = x0[length(x0)] - x0[1]
  anormy = y0[length(y0)] - y0[1]
  x0 = (x0 - x0[1])/anormx
  y0 = (y0 - y0[1])/anormy

  data.frame(anormx,anormy,x0,y0)
}
