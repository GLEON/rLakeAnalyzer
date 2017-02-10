#' @export
#' @title computing a norm value for the segment from point k1 to k2-1
#' @param k1 [INTEGER] start point
#' @param k2 [INTEGER] end point+1
#' @param x [REAL(?)] input x-axis array (predictor)
#' @param y [REAL(?)] input y-axis array (response)
#' @return A -[REAL] coefficient of linear regression (will zero if K1=0) (y=Ax+B), B -[REAL] coefficient of linear regression, R2B -[REAL] norm of the segment (maximum "distance" measured perpendicular to the regression line)
#' @description  A -[REAL] coefficient of linear regression (will zero if K1=0) (y=Ax+B), B -[REAL] coefficient of linear regression, R2B -[REAL] norm of the segment (maximum "distance" measured perpendicular to the regression line)
#' @examples
#' ni <- c( 1, 201, 402 )
#' i <- 1
#' k1 <- ni[i]
#' k2 <- ni[i+1]
#'
#'
#' r2b(k1, k2, y=t11$temper, x=t11$depth)

# Doug could not find anywhere in the original code where outputs a and b are actually used
# he has taken then out to avoid having to return a data.frame.

## Sam has replaced r2b (again)

r2b = function(k1,k2,x,y) {
  
  if(k2-k1<=2) {
    r2b=0
    #    a = 0  # original code does not produce a or b?
    #    b = 0
  } else {
    is = k1:(k2-1)
    sxx = sum(x[is]^2)
    sxy = sum(x[is]*y[is])
    sy = sum(y[is])
    sx = sum(x[is])
    
    n = k2-k1
    
    a = 0.0
    if(k1 > 1) {
      a = (n*sxy-sy*sx)/(n*sxx-sx*sx)
    }
    b = (sy-a*sx)/n
    r2b = max( abs( y[is] - a*x[is] - b )/sqrt(a^2 + 1) )
  }
  
  #return(data.frame(r2b=r2b,a=a,b=b))
  return(r2b)
}