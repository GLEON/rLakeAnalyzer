#' @export
#' @description Interpolate and normalize a vector of samples. X and Y will be linearly interpolated to match the length of X0 and will be normalized so that the first point is (0,0) and the last point is (1,1). Vector x are the input x values, must be in ascending order.
#'
#' @title Interpolate and normalize a vector of samples.
#' @param x [REAL(N)] input x-axis array (should be in "chronological" order)
#' @param y [REAL(N)] input y-axis array
#' @param nn [INTEGER] input, desired dimension of output xx,yy vectors
#' @param x0 [REAL] start point along the x-axis; i.e., xx[1] = x0
#' @param dx [REAL] step value for xx; i.e., dx = xx[i+1]-xx[i] for all i<nn
#' @return Output is a data.frame with: anormx, the normalization value used to make the last x == 1. anormy, the normalization value used to make the last y == 1. xx,yy  vectors of x,y values interpolated to be the same length as x0.
#'
#' @examples
#' x0 <- c(0,1,2,3,4,5)
#' getxxnorm(y=t11$depth,x=t11$temper, x0=x0)


getxxnorm <- function(x,y,nn,x0,dx) {
  xx = x0 + dx*(0:(nn-1))    # Spread xx out linearly.
  yy <- rep(0,nn)            # Reserve space for y values.

  j = 1
#print(c("j:",j)) #DEBUG
  for ( i in 1:nn ) { # Loop over output calculating yy[i]
    # j = 1 + sum(xx[i] >= x)  # Equivalent to following three lines.
    while( (xx[i] >= x[j]) && (j < length(x)) ) {
      j <- j+1
    }
# print(c("j:",j)) #DEBUG
    if( j == 1 ) {
      yy[i] <- y[1]
    }
    else {
      # print(c("xx[i],x[j-1],-:",xx[i],x[j-1],xx[i]-x[j-1])) #DEBUG
      # print(c("x[j],x[j-1],-:",x[j],x[j-1],x[j]-x[j-1])) #DEBUG
      # print(c("j,y[j],y[j-1]:",j,y[j],y[j-1],(y[j]-y[j-1]))) #DEBUG
      yy[i] <- y[j-1]+(xx[i]-x[j-1])/(x[j]-x[j-1])*(y[j]-y[j-1]) #DEBUG
      # print(c("yy[i]:",yy[i])) #DEBUG
      # print("") #DEBUG
    }
  }
  anormx = xx[length(xx)] - xx[1]
  anormy = yy[length(yy)] - yy[1]
  xx = (xx - xx[1])/anormx
  yy = (yy - yy[1])/anormy

  data.frame(anormx,anormy,xx,yy)
}
