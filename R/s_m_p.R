#' @export
#' @title Computing linear segments for a specified error norm value.
#' @param eps [real] error norm
#' @param x [real] input x-axis array, should be an increasing function of index
#' @param y [real] input y-axis array
#' @return [integer] array A of indices giving data segments.
#' A[i] start of interval; A[i+1] end of interval, for any i<length(A)
#' @description Segments the data in x,y into the intervals given in the output array A.
#' The data in each interval can be linearly fitted within an error, given by r2b(), less than eps.
#' Comments from original Fortran code.
#' SUBROUTINE s_m_p(n,eps,x,y,Nr,Ni)
#' C     Main subroutine for determining LINEAR SEGMENTS for a SPECIFIED ERROR NORM VALUE
#' c     (This subroutine determines the linear fit to the data for a specified error norm)
#' C     Input:
#' C          N   -[INTEGER] number of data points;
#' C          EPS -[REAL]    error norm;
#' C          X   -[REAL(N)] input x-axis data array, should be increasing function of index
#' C          Y   -[REAL(N)] input y-axis data array
#' C       Output:
#' C          NR  -[INTEGER] final number of segments;
#' C          NI  -[INTEGER] final array with segment start points
#' C
#' INTEGER NR, NI(n)
#' REAL X(n),Y(n)

# The dynamic arrays in R remove the need to know the number of data points, n, which is just
# the length of x and y. Similarly Nr is no longer needed and is length(Ni)-1, one less than the length
# of the output array.

s_m_p = function(eps,x,y) {
  # This code generates Nr intervals in vector Ni
  if(is.unsorted(x)==TRUE) stop("x is not an increasing vector")
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

  # If any interval does not meet the r2b<eps test then split it into two with spl().
  # Note that ni will grow in length as this process proceeds.
  i = 1
  while( i<length(ni) ) {   # Rinse, repeat over all intervals in ni.
    k1=ni[i]
    k2=ni[i+1]
    if(r2b(k1,k2,x,y) > eps ) {
      # N.B. We split an interval here so ni gets one element longer
      # N.B. If an interval is added we will have to test the first
      # of the two new intervals so we don't increment i.
      ni = spl(ni,i)
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
    # Note that ni potentially changes inside the loop.
    if( 2 <= length(ni)-2 ) {
      # This branch had to be added because the Fortran DO loop will not execute
      # if the start value is greater than the limit value but R runs backward.
      for( i in 2:(length(ni)-2) ) {
        k1=ni[i-1]
        k2=ni[i+1]
        eps1=r2b( k1=k1, k2=k2, x=x, y=y )
        if( eps1<eps ) {
          if( length(ni)-1 > 2 ) { # Are there two or more intervals in the entire set?
            # Yes, so merge the interval we are looking at.
            ni <- zerge(i, ni)
            changed = TRUE
            break # exit the for loop
          } else {
            # We are here because the last two intervals tested out to merge.
            # So we can just adjust the intervals and bail out entirely because,
            # apparently the entire data set lies on a line within eps!
            # TODO: The original code doesn't seem to do this correctly. It should
            # adjust ni[2] = ni[3] and nr = 1. I think this branch has never been
            # taken in actual operation. Obviously the data set is invalid if it lies
            # entirely on a straight line, right?

            ni[1]=1  # TODO: Original code, but this should already be the case.
            return(ni)
          }
        }
      }
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

##################################
### Corresponding FORTRAN CODE ###
##################################
#Nr=2
#m=NINT(FLOAT(N)/FLOAT(Nr))
#ni(1)=1
#DO i=2,Nr
#Ni(i)=m*(i-1)+1
#end DO
#Ni(Nr+1)=N+1 !{last interval}
#c      sss=orma2b(2,20,x,y,a,b)
#
#m=0
#100   change=.false.
#
#c       {step 1: if exceeds norma}
#
#C     lab1:
#  i=1
#101   CONTINUE
#DO while (i.le.Nr)
#  k1=Ni(i)
#k2=Ni(i+1)
#enorma=r2b(k1,k2,x,y,a,b)
#if (enorma.GT.eps) then
#CALL spl(i,Nr,Ni)
#change=.true.
#goto 104
#else
#  i=i+1
#end IF
#104   end DO
#
#C       {step 2: try to merge}
#
#102   CONTINUE
#DO i=2,Nr-1
#k1=Ni(i-1)
#k2=Ni(i+1)
#eps1=r2b(k1,k2,x,y,a,b)
#if (eps1.LE.eps) then
#if (Nr.GT.2) then
#CALL merge_(i,Nr,Ni)
#change=.true.
#goto 102
#else
#  Ni(1)=1
#Nr=2
#RETURN
#end IF
#end IF
#end DO
#c      to avoid couples
#do i=1,nr
#k1=ni(i)
#k2=ni(i+1)
#if (k2-k1.eq.1) then
#change=.true.
#ni(i+1)=ni(i+1)+1
#end if
#end do
#C         {"R" algorithm: adjusting the endpoint}
#DO i=2,Nr
#k1=Ni(i-1)
#k2=Ni(i+1)
#j1=Ni(i)
#eps1=r2b(k1,j1,x,y,a,b)
#eps2=r2b(j1,k2,x,y,a,b)
#if (eps1.GT.eps2) then
#epsm=eps1
#else
#  epsm=eps2
#END IF
#DO j=k1+2,k2-2
#eps1=r2b(k1,j,x,y,a,b)
#eps2=r2b(j,k2,x,y,a,b)
#
#if (eps1.GT.eps2) then
#epsr=eps1
#else
#  epsr=eps2
#END IF
#
#if (epsr.LT.epsm) then
#epsm=epsr
#j1=j
#end IF
#end DO
#if (j1.NE.Ni(i)) then
#Ni(i)=j1
#change=.true.
#end IF
#if (i.eq.2) epsm1=epsm
#end DO
#
#if (change) then
#change=.false.
#goto 102
#end if
#ni(nr+1)=N
#RETURN
#end
