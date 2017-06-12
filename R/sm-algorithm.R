## All these functions are export for use but hidden from basica documentation so as not to clutter up the package documentation. 
#' @export
#' @title spliting interval i into 2 pieces
#' @param i [INTEGER] interval number, should be less than NR+1
#' @param ni [INTEGER(NR)] current array with interval start point number
#' 
#' @keywords internal
#'
#' @return new array with interval start point number
#' @description spliting interval i into 2 pieces
# @examples
#' ni = c(1,5,10,15,20)
#' nr = 4
#' ni
#' ni = split_interval(ni,i=2)
#' ni
#' ni = split_interval(ni,i=5)
#' ni
#' ni = split_interval(ni,i=2)
#' ni
#' ni = split_interval(ni,i=1)
#' ni
#' ni = split_interval(ni,i=length(ni)-1)
#' ni
#' ni = split_interval(ni,i=length(ni)-1)
#' ni
#' ni

split_interval <- function(ni,i) {
  stopifnot(i < length(ni)) # Otherwise ni[i+1] would fail.
  k1 = ni[i]
  k2 = ni[i+1]
  jsplit = floor((k1+k2)/2) # Is an index, must be an integer.
  
  # The following lines are taken from the original code but actually
  # are in error. They are included simply to make the tests produce the
  # identical answers in every case.
  if (jsplit>=k2-1) jsplit=k2-2
  if (jsplit<=k1+1) jsplit=k1+2
  
  stopifnot(k1<jsplit & jsplit<k2) # Verify that the interval has actually been split.
  # Create a new vector with jsplit at position i
  c( ni[1:i], jsplit, ni[(i+1):length(ni)] )
}

#' @export
#' @title merge_intervals
#' @description merge interval i and i+1
#' @param i [INTEGER] interval number of the first segment to merge
#' @param ni [INTEGER(NR)] current array with interval start point numbers
#' 
#' @keywords internal

merge_intervals = function(i,ni) {
  stopifnot(i > 1) # Can't merge the first interval
  stopifnot(i<length(ni)) # Must have an element at n[i+1]
  stopifnot(length(ni)>2) # Only one interval so can't merge
  c( ni[1:i-1], ni[(i+1):length(ni)] ) # Just removes element i
}


#' @export
#' @description Normalize a vector of samples. X and Y  will be normalized so that the first point is (0,0) and the last point is (1,1). Vector x are the input x values, must be in ascending order.
#'
#' @title Normalize a vector of samples.
#' @param x [REAL(N)] input x-axis array (should be in "chronological" order)
#' @param y [REAL(N)] input y-axis array
#' @return Output is a list with:
#' anormx, the normalization value used to make the last x == 1.
#' anormy, the normalization value used to make the last y == 1.
#' xx,yy  vectors of x,y values interpolated to be the same length as x0.
#' 
#' @keywords internal
#'

getxxnorm <- function(x,y) {
  #xx = x0 + dx*(0:(nn-1))    # Spread xx out linearly.
  #yy <- rep(0,nn)            # Reserve space for y values.
  
  #j = 1
  #for ( i in 1:nn ) { # Loop over output calculating yy[i]
  #  # j = 1 + sum(xx[i] >= x)  # Equivalent to following three lines.
  #  while( (xx[i] >= x[j]) && (j < length(x)) ) {
  #    j <- j+1
  #  }
  #  if( j == 1 ) {
  #    yy[i] <- y[1]
  #  }
  #  else {
  #    yy[i] <- y[j-1]+(xx[i]-x[j-1])/(x[j]-x[j-1])*(y[j]-y[j-1])
  #  }
  #}
  anormx = x[length(x)] - x[1]
  anormy = y[length(y)] - y[1]
  xx = (x - x[1])/anormx
  yy = (y - y[1])/anormy
  
  list(anormx=anormx,anormy=anormy,xx=xx,yy=yy)
}


#' @export
#' @title computing a norm value for the segment from point k1 to k2-1
#' @param k1 [INTEGER] start point
#' @param k2 [INTEGER] end point+1
#' @param x [REAL(?)] input x-axis array (predictor)
#' @param y [REAL(?)] input y-axis array (response)
#' @return A -[REAL] coefficient of linear regression (will zero if K1=0) (y=Ax+B), B -[REAL] coefficient of linear regression, R2B -[REAL] norm of the segment (maximum "distance" measured perpendicular to the regression line)
#' @description  A -[REAL] coefficient of linear regression (will zero if K1=0) (y=Ax+B), B -[REAL] coefficient of linear regression, R2B -[REAL] norm of the segment (maximum "distance" measured perpendicular to the regression line)
#' 
#' @keywords internal
#' 
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


#' @export
#' @title Computing linear segments for a specified error norm value.
#' @param eps [real] error norm
#' @param x [real] input x-axis array, should be an increasing function of index
#' @param y [real] input y-axis array
#' @return [integer] array A of indices giving data segments.
#' A[i] start of interval; A[i+1] end of interval, for any i<length(A)
#' @description Segments the data in x,y into the intervals given in the output array A.
#' The data in each interval can be linearly fitted within an error, given by r2b(), less than eps.
#' 
#' @keywords internal

# The dynamic arrays in R remove the need to know the number of data points, n, which is just
# the length of x and y. Similarly Nr is no longer needed and is length(Ni)-1, one less than the length
# of the output array.

s_m_p = function(eps,x,y) {
  # This code generates Nr intervals in vector Ni
  #if(is.unsorted(x)==TRUE) stop("x is not an increasing vector")
  #  Nr=2
  #      m=NINT(FLOAT(N)/FLOAT(Nr))
  #        ni(1)=1
  #      DO i=2,Nr
  #        Ni(i)=m*(i-1)+1
  #      end DO
  #      Ni(Nr+1)=N+1 !{last interval}
  
  # But N4 is fixed at two so we end up with 3 elements in Ni:
  # Ni = {1,m+1,n+1}
  # Clearly, it once spread Nr intervals out uniformly over all the data.
  # We will short-circuit it instead:
  ni = c( 1, round(length(x)/2)+1, length(x)+1 )
  
  # If any interval does not meet the r2b<eps test then split it into two with split_interval().
  # Note that ni will grow in length as this process proceeds.
  i = 1
  while( i<length(ni) ) {   # Rinse, repeat over all intervals in ni.
    k1=ni[i]
    k2=ni[i+1]
    if(r2b(k1,k2,x,y) > eps ) {
      # N.B. We split an interval here so ni gets one element longer
      # N.B. If an interval is added we will have to test the first
      # of the two new intervals so we don't increment i.
      ni = split_interval(ni,i)
      changed = TRUE
    }
    else {
      # Prior intervals all meet eps test, go to next one.
      i = i + 1
    }
  }
  
  changed = TRUE   # Keep track of whether any intervals were altered...
  while(changed) {  # ... because we are not finished until none were.
    changed = FALSE  # Continue the loop only if something changed.
    
    # All intervals now meet the test but it is possible that there are too many
    # in the sense that there may now be adjacent intervals, which if merged into one,
    # will then still meet the test.
    # Scan for such interval pairs and merge them.
    
    # Loop i over all possible "middle" intervals, which we may remove.
    # I am not sure at this point why the last interval is not considered for merging,
    # as it would be if i<=length(ni)-1 , which seems more natural to me.
    
    # Note that ni potentially changes inside the loop.
    i = 2
    while(i <= length(ni)-2) {
      # if( 2 <= length(ni)-2 ) {
      #   # This branch had to be added because the Fortran DO loop will not execute
      #   # if the start value is greater than the limit value but R runs backward.
      #   for( i in 2:(length(ni)-2) ) {
      k1=ni[i-1]
      k2=ni[i+1]
      eps1=r2b( k1=k1, k2=k2, x=x, y=y )
      if( eps1<eps ) {
        if( length(ni)-1 > 2 ) { # Are there two or more intervals in the entire set?
          # Yes, so merge the interval we are looking at.
          ni <- merge_intervals(i, ni)
          changed = TRUE
          # break # exit the loop
        } else {
          # We are here because the last two intervals tested out to merge.
          # So we can just adjust the intervals and bail out entirely because,
          # apparently the entire data set lies on a line within eps!
          # TODO: The original code doesn't seem to do this correctly. It should
          # adjust ni[2] = ni[3] and nr = 1. I think this branch has never been
          # taken in actual operation. Obviously the data set is invalid if it lies
          # entirely on a straight line, right?
          
          #            ni[1]=1  # TODO: Original code, but this should already be the case.
          return(ni)
        }
      }
      # }
      i = i+1
    }
    
    # If a merge was done then changed==TRUE. In this circumstance, the original
    # code then immediately attempts to merge all intervals again from 2 so we
    # do the same. This procedure seems wasteful because all the prior
    # interval pairs have already been tested for merging and don't need it, yet we
    # are going to test them all over again. For the moment we will continue to
    # do it this way and perhaps TODO change it later so that it just restarts
    # with the just-merged interval. Should get the same results faster.
    
    if( changed ) next   # short out the rest of the while(changed) loop.
    
    # In the original code the following is preceded with a comment:
    #  "to avoid couples"
    # I'm not precisely sure what that is supposed to mean but from the original
    # code I infer:
    #
    # If the end index of any interval is only 1 greater than the start index
    # (i.e., ni[i+1] == 1 + ni[i]) then bump the end index up by one. Due to the
    # loop this change propagates up to the last interval so we can't easily
    # replace the loop with nice vectorized R code.
    
    for( i in 1:(length(ni)-1) ) {
      if( ni[i+1] - ni[i] == 1 ) {
        ni[i+1] = ni[i+1] + 1
        changed = TRUE
      }
    }
    
    # TODO: seems like we should restart the while(changed) loop at this point.
    
    # At this point in the original code stands the comment:
    #  "{"R" algorithm: adjusting the endpoint}"
    # I don't know what the "R algorithm" is so I'm just going to have to wing
    # comments.
    
    # Scan all adjacent interval pairs.
    for( i in 2:(length(ni)-1) ) {
      # First of pair (ni[i-1],ni[i]), second (ni[i],ni[i+1])
      k1 = ni[i-1]
      kmid = ni[i]
      k2 = ni[i+1]
      # Find the error on both intervals and take the max.
      epsm = max( r2b(k1,kmid,x,y), r2b(kmid,k2,x,y) ) # TODO: I don't think this is necessary. We'll find it below anyway.
      
      # We are going to move the midpoint and find the split that gives the
      # best error.
      j1 = ni[i]    # Keep track of best splitpoint so far.
      # Scan all alternative splitpoints between these two enpoints.
      for( j in (k1+2):(k2-2) ) {
        epsr = max( r2b(k1,j,x,y), r2b(j,k2,x,y) )  # Calculate max error
        if( epsr < epsm ) {
          epsm = epsr  # This split is the best so far
          j1 = j # Keep track of which index it is at.
        }
      }
      
      if( j1 != ni[i] ) {  # Did we find a better splitpoint?
        ni[i] = j1  # Yes, change the split.
        changed = TRUE
      }
      
      # Here in the original code stands "if (i.eq.2) epsm1=epsm"
      # but epsm1 is never used so we have ignored it.
      
    } # for
    
  } # while(changed)
  
  # The following statement corresponds to one in the original code but seems
  # unnecessary. The code that manipulates ni should really maintain this condition.
  # Mind you, I haven't actually checked carefully that this is true. Maybe the
  # author added it to solve a problem! TODO: maybe take it out and test sometime.
  ni[length(ni)] = length(x)
  
  return(ni) # Q.E.D.
}


#' @export
#' @title Computing linear segments for a specified error norm value.
#' @param nr [integer] number of segments, fixed.
#' @param x [real] input x-axis array, should be an increasing function of index
#' @param y [real] input y-axis array
#' @return [list(eps=eps,ni=ni)] where:
#' eps [real] is the maximum error over all intervals,
#' ni is a [vector of integer, length nr+1] vector Ni of indices giving data segments
#' Ni[i] start of interval; Ni[i+1] end of interval, for any i<length(Ni)
#' @description
#' Subroutine to determine the Linear SEGMENTS for a PREDEFINED NUMBER OF SEGMENTS (NR)
#' (Use this program when you want to predefine the number of segments you want to fit
#' to the data)
#' Segments the data in x,y into the intervals given in the output array A.
#' The data in each interval can be linearly fitted within an error, given by r2b(), less than eps.
#' 
#' @keywords internal

# The dynamic arrays in R remove the need to know the number of data points, n, which is just
# the length of x and y.

s_mN = function(nr,x,y) {
  
  # Initially spread intervals uniformly out over the data
  # taking care that the last interval includes the last point,
  # which otherwise might be lost due to the rounding error.
  ni = rep(0,nr+1)
  m = round(length(x)/nr)
  ni = c( m*(0:(nr-1))+1, length(x)+1 )
  
  #TODO: A more accurate formula might be:
  # ni = round( 1 + (0:nr)*length(x)/nr )
  
  #  Nr=2
  #      m=NINT(FLOAT(N)/FLOAT(Nr))
  #        ni(1)=1
  #      DO i=2,Nr
  #        Ni(i)=m*(i-1)+1
  #      end DO
  #      Ni(Nr+1)=N+1 !{last interval}
  
  changed = TRUE   # Keep track of whether any intervals were altered...
  while(changed) {  # ... because we are not finished until none were.
    changed = FALSE  # Continue the loop only if something changed.
    
    # All intervals now meet the test but it is possible that there are too many
    # in the sense that there may now be adjacent intervals, which if merged into one,
    # will then still meet the test.
    # Scan for such interval pairs and merge them.
    
    # # Loop i over all possible "middle" intervals, which we may remove.
    # # Note that ni potentially changes inside the loop.
    # if( 2 <= length(ni)-2 ) {
    #   # This branch had to be added because the Fortran DO loop will not execute
    #   # if the start value is greater than the limit value but R runs backward.
    #   for( i in 2:(length(ni)-2) ) {
    #     k1=ni[i-1]
    #     k2=ni[i+1]
    #     eps1=r2b( k1=k1, k2=k2, x=x, y=y )
    #     if( eps1<eps ) {
    #       if( length(ni)-1 > 2 ) { # Are there two or more intervals in the entire set?
    #         # Yes, so merge the interval we are looking at.
    #         ni <- merge_intervals(i, ni)
    #         changed = TRUE
    #         break # exit the for loop
    #       } else {
    #         # We are here because the last two intervals tested out to merge.
    #         # So we can just adjust the intervals and bail out entirely because,
    #         # apparently the entire data set lies on a line within eps!
    #         # TODO: The original code doesn't seem to do this correctly. It should
    #         # adjust ni[2] = ni[3] and nr = 1. I think this branch has never been
    #         # taken in actual operation. Obviously the data set is invalid if it lies
    #         # entirely on a straight line, right?
    #
    #         ni[1]=1  # TODO: Original code, but this should already be the case.
    #         return(ni)
    #       }
    #     }
    #   }
    # }
    
    # # If a merge was done then changed==TRUE. In this circumstance, the original
    # # code then immediately attempts to merge all intervals again from 2 so we
    # # do the same. This procedure seems wasteful because all the prior
    # # interval pairs have already been tested for merging and don't need it, yet we
    # # are going to test them all over again. For the moment we will continue to
    # # do it this way and perhaps TODO change it later so that it just restarts
    # # with the just-merged interval. Should get the same results faster.
    #
    # if( changed ) next   # short out the rest of the while(changed) loop.
    
    # In the original code the following is preceded with a comment:
    #  "to avoid couples"
    # I'm not precisely sure what that is supposed to mean but from the original
    # code I infer:
    #
    # If the end index of any interval is only 1 greater than the start index
    # (i.e., ni[i+1] == ni[i]) then bump the end index up by one. Due to the
    # loop this change propagates up to the last interval so we can't easily
    # replace the loop with nice vectorized R code.
    
    for( i in 1:(length(ni)-1) ) {
      if( ni[i+1] - ni[i] == 1 ) {
        ni[i+1] = ni[i+1] + 1
        changed = TRUE
      }
    }
    
    # TODO: seems like we should restart the while(changed) loop at this point.
    
    # At this point in the original code stands the comment:
    #  "{"R" algorithm: adjusting of endpoint}"
    # I don't know what the "R algorithm" is so I'm just going to have to wing
    # comments.
    
    for( i in 2:(length(ni)-1) ) {
      k1 = ni[i-1]
      kmid = ni[i]
      k2 = ni[i+1]
      epsm = max( r2b(k1,kmid,x,y), r2b(kmid,k2,x,y) )
      
      j1 = ni[i]
      for( j in (k1+2):(k2-2) ) {
        epsr = max( r2b(k1,j,x,y), r2b(j,k2,x,y) )
        if( epsr < epsm ) {
          epsm = epsr
          j1 = j
        }
      }
      
      if( j1 != ni[i] ) {
        ni[i] = j1
        changed = TRUE
      }
      
      # Here in the original code stands "if (i.eq.2) epsm1=epsm"
      # but epsm1 is never used so we have ignored it.
      
    } # for
    
  } # while(changed)
  
  # The following statement corresponds to one in the original code but seems
  # unnecessary. The code that manipulates ni should really maintain this condition.
  # Mind you, I haven't actually checked carefully that this is true. Maybe the
  # author added it to solve a problem! TODO: maybe take it out and test sometime.
  ni[length(ni)] = length(x)
  
  return(list(eps=epsm,ni=ni)) # Q.E.D.
}
