#################
### Functions ###
#################


#############
### getxx ###
#############

#####################
### Documentation ###
#####################
#c     Linear interpolation of the Input Data Series
#c
#c     Input:
#c         N - [INTEGER] number of points in the input array
#c         N1- [INTEGER] number of points in the output array
#c         X0- [REAL] start point along the x-axis
#c         DX- [REAL] data step along the x-axis
#c         X-  [REAL(N)] input x-axis array (should be in "chronological" order)
#c         Y-  [REAL(N)] input y-axis array
#c
#c     outputs:
#c         XX-  [REAL(N1)] output x-axis array (contains X0+dx(I-1))
#c         YY-  [REAL(N1)] output y-axis array

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

#################
### getxxnorm ###
#################

#####################
### Documentation ###
#####################

#getxxnorm(x,y, x0)
# Interpolate and normalize a vector of samples.
# X and Y will be linearly interpolated to match the length of X0
# and will be normalized so that the first point is (0,0) and the
# last point is (1,1).
# Input:
#   Vector x are the input x values, must be in ascending order.
#   Vector y are the input y values.
#   Vector x0 are the values of x at which the new interpolated samples
#   should be evaluated.
# Output is a data.frame with:
#   anormx, the normalization value used to make the last x == 1.
#   anormy, the normalization value used to make the last y == 1.
#   xx,yy  vectors of x,y values interpolated to be the same length as x0.
#


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



###########
### spl ###
###########

#####################
### Documentation ###
#####################

#SUBROUTINE spl(i,Nr,Ni)
#INTEGER NR,NI(nr+2)
#
#C     {spliting interval i into 2 pieces
#  c     inputs:
#  c       I- [INTEGER] interval number, should be less than NR+1
#  c       NR- [INTEGER] current maximum of interval number
#  c       NI- [INTEGER(NR)] current array with interval start point number
#  c
#  c     outputs:
#  c       NR- [INTEGER] new maximum of interval number
#  c       NI- [INTEGER(NR)] new array with interval start point number


spl <- function(ni, i, nr) {
  if( i >=nr+1 ) {"i needs to be less that nr+1"}
  else {
    k1=ni[i]
    k2=ni[i+1]

    ## Feels a little silly to defining this explicitly
    #jsplit_cond=floor((k1+k2)/2)

    ## Using double condition in an attempt to be defensive
    #jsplit <- ifelse(jsplit_cond >= k2-1, k2-2,
    #                 ifelse(jsplit_cond <= k1+1, k1+2,
    #                        "Condition Not Satisfied")
    #)

    ## I think the following achieves the same result:
    jsplit <- max(min(floor((k1 + k2) / 2), k2 - 2), k1 + 2)

    nn = ni
    nn[i:nr] = ni[(nr+1):(i+1)]
    nn[i+1] =jsplit
    nn
  }
}

#################
### merge/zerge ###
#################


#####################
### Documentation ###
#####################

##      SUBROUTINE merge_(i,Nr,Ni)
#      INTEGER NI(nr+2)
#C{merge interval i and i+1}
#c     inputs:
#c       I- [INTEGER] interval number of the first segment to merge
#c       NR- [INTEGER] current maximum of the interval number
#c       NI- [INTEGER(NR)] current array with interval start point numbers
#c
#c     outputs:
#c       NR- [INTEGER] new maximum of interval number
#c       NI- [INTEGER(NR)] new array with interval start point numbers



# See merge_.for - this now gets the same answers, though is a function, NB.

zerge = function(i,nr,ni) {
  nn = ni
  nn[i:nr] = ni[(i+1):(nr+1)]
  nn
}


###########
### r2b ###
###########

#####################
### Documentation ###
#####################

#REAL function r2b(k1,k2,x,y,a,b)
#c      computing a norm value for the segment from point k1 to k2-1
#c     inputs:
#c         K1 -[INTEGER] start point
#c         K2 -[INTEGER] end point+1
#c         X -[REAL(?)] input x-axis array
#c         Y -[REAL(?)] input y-axis array
#c     outputs:
#c         A -[REAL] coefficient of linear regression (will zero if K1=0) (y=Ax+B)
#c         B -[REAL] coefficient of linear regression
#c         R2B -[REAL] norm of the segment
#c                     (maximum "distance" measured perpendicular to the regression line)



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
    ssx = sum(df$x[i]^2)
    sxy = sum(df$x[i]*df$y[i])
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

