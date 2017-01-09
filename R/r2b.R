


#' @title computing a norm value for the segment from point k1 to k2-1
#' @param k1 [INTEGER] start point
#' @param k2 [INTEGER] end point+1
#' @param x [REAL(?)] input x-axis array
#' @param y [REAL(?)] input y-axis array
#' @return A -[REAL] coefficient of linear regression (will zero if K1=0) (y=Ax+B), B -[REAL] coefficient of linear regression, R2B -[REAL] norm of the segment (maximum "distance" measured perpendicular to the regression line)
#' @description  A -[REAL] coefficient of linear regression (will zero if K1=0) (y=Ax+B), B -[REAL] coefficient of linear regression, R2B -[REAL] norm of the segment (maximum "distance" measured perpendicular to the regression line)



r2b = function(k1,k2,x,y) {
  # k1,k2: indexes of a segment of data in x,y
  # x,y: data arrays
  # return data.frame (r2b,a,b)

  if(k2-k1<=2) {
    r2b=0
    a = 0  # original code does not produce a or b?
    b = 0
  } else {
    #  sx=0
    #  sxx=0
    #  sy=0
    #  sxy=0
    #  eps=0
    i = k1:(k2-1)
    sxx = sum(x[i]^2)
    sxy = sum(x[i]*y[i])
    sy = sum(y[i])
    sx = sum(x[i])
    #  for (i in k1:k2-1){
    #    sxx=sxx+df$x[i]^2
    #    sxy=sxy+df$x[i]*df$y[i]
    #    sy=sy+y[i]
    #    sx=sx+x[i]
    #  }

    n = k2-k1
    a = 0.0
    if(k1 > 1) {
      a = (n*sxy-sy*sx)/(n*sxx-sx*sx)
    }
    b = (sy-a*sx)/n
    r2b = max( abs( y[i] - a*x[i] - b )/sqrt(a^2 + 1) )
  }
  data.frame(r2b=r2b,a=a,b=b)
}

