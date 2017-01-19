#' @export
#' @description Alternate gettxxnorm
#'
#' @title Alternate gettxxnorm
#' @param x [REAL(N)] input x-axis array (should be in "chronological" order)
#' @param y [REAL(N)] input y-axis array
#' @param nn [INTEGER] input, desired dimension of output xx,yy vectors
#' #@param x0 [REAL] start point along the x-axis; i.e., xx[1] = x0
#' @param dx [REAL] step value for xx; i.e., dx = xx[i+1]-xx[i] for all i<nn
#' @return Output is a data.frame with: anormx, the normalization value used to make the last x == 1.
#' anormy, the normalization value used to make the last y == 1.
#' xx,yy  vectors of x,y values interpolated to be the same length as x0
#'
# @examples
#' x <- c(1,2,3,3.5)
#' y <- c(10.0,20.0,10.0,5.0)
#' n1 <- 10 #desired dimension of output x,y vectors
#' dx=(max(x)-min(x))/(n1-1) #i.e. step value for x; i.e., dx = x[i+1]-x[i] for all i<n1
#'
#' getxxnorm2(x=x,y=y,dx=dx, nn=n1)


getxxnorm2 <- function(x, y, dx, nn) {
  ## Step1: linearly interpolate given data points
  linear_df=as.data.frame(approx(x=x, y=y, xout=seq(min(x), max(x), by=dx), method = "linear", rule=2))

  ## Step2: normalize the points using min/max
  ## TODO: Could make the normalization process an argument in case one wanted to try algorithm on raw data
  linear_df$xx=(linear_df$x - min(linear_df$x)) / (max(linear_df$x) - min(linear_df$x))
  linear_df$yy=(linear_df$y - min(linear_df$y)) / (max(linear_df$y) - min(linear_df$y))

  linear_df$anormx=tail(linear_df$xx, n=1) - head(linear_df$xx, n=1)
  linear_df$anormy=tail(linear_df$yy, n=1) - head(linear_df$yy, n=1)

  ## I don't know is this ever be true; will eventually be a test
  if( nrow(linear_df)!=nn ) stop("nn is not equal to number of dimensions outputted")

  ## Same order as getxxnorm()
  ## Returns interpolated data to illustrate wacky anormx and anormy coefficients
  return(linear_df[,c(5,6,3,4,1,2)])
}



x <- c(1,2,3,3.5)
y <- c(10.0,20.0,10.0,5.0)
x0 <- 1
ni <- 10

dx=0.18


## Direct from FORTRAN code
#getxxnorm <- function(x0, nn, x, y, dx) {
#  x1=x0
#  j=1
#  xx <- NULL
#  yy <- NULL
#
#  for( i in 1:nn ) {
#    xx[i]=x1
#    while( x1 >= x[j] && j<length(x) ){
#      j=j+1
#    }
#    if( j == 1 ) {
#      yy[i] <- y[1]
#      } else {
#        yy[i] <- y[j-1] + (x1 - x[j-1]) / (x[j] - x[j-1]) * (y[j] - y[j-1])
#      }
#    x1 <- x1+dx
#  }
#  y0=yy[1]
#  anormx=xx[nn]-x0
#  anormy=yy[nn]-y0
#
#  for( i in 1:nn ){
#    xx[i]=(xx[i]-x0)/anormx
#    yy[i]=(yy[i]-y0)/anormy
#  }
#  data.frame(anormx, anormy, xx, yy)
#}




