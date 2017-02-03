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
#' Comments from original Fortran code:
#' SUBROUTINE s_mN(n,eps,x,y,Nr,Ni)
#' C       Input:
#' C          N   -[INTEGER] number of points;
#' C          NR  -[INTEGER] number of segments (fixed);
#' C          X   -[REAL(N)] input x data array, should be increasing function of index
#' C          Y   -[REAL(N)] input y data array
#' C       Output:
#' C        EPS -[REAL] maximum error norm
#' C          NI  -[INTEGER] final array with segment start points
#' INTEGER NR, NI(n)
#' REAL X(n),Y(n)

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
    #         ni <- zerge(i, ni)
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

##################################
### Corresponding FORTRAN CODE ###
################################### SUBROUTINE s_mN(n,eps,x,y,Nr,Ni)
# C     Subroutine to determine the Linear SEGMENTS for a PREDEFINED NUMBER OF SEGMENTS (NR)
# c     (Use this program when you want to predefine the number of segments you want to fit
#        c      to the data)
# C       Input:
#   C          N   -[INTEGER] number of points;
# C          NR  -[INTEGER] number of segments (fixed);
# C          X   -[REAL(N)] input x data array, should be increasing function of index
# C          Y   -[REAL(N)] input y data array
# C       Output:
#   C        EPS -[REAL] maximum error norm
# C          NI  -[INTEGER] final array with segment start points
# INTEGER NR, NI(n)
# REAL X(n),Y(n)
# INTEGER i,j,j1,k1,k2 ! Remove warnings from unused variables
# !        INTEGER i,j,j1,k,k1,k2,k3
# integer M
# REAL eps1,eps2,epsr,epsm,a,b
# LOGICAL change
# !      REAL s  ! Remove warning from unused variable
# C       label lab0,lab1,lab2,lab3;
# epsm = 0.0  ! Silence a compiler warning about unitialized use.
# m=NINT(FLOAT(N)/FLOAT(Nr))
# ni(1)=1
# DO i=2,Nr
# Ni(i)=m*(i-1)+1
# end DO
# Ni(Nr+1)=N+1 !{last interval}
# c      sss=orma2b(2,20,x,y,a,b)
#
# m=0
# 102   CONTINUE
# c      to avoid couples
# do i=1,nr
# k1=ni(i)
# k2=ni(i+1)
# if (k2-k1.eq.1) then
# change=.true.
# ni(i+1)=ni(i+1)+1
# end if
# end do
# C         {"R" algorithm: adjusting of endpoint}
# DO i=2,Nr
# k1=Ni(i-1)
# k2=Ni(i+1)
# j1=Ni(i)
# eps1=r2b(k1,j1,x,y,a,b)
# eps2=r2b(j1,k2,x,y,a,b)
# c         epsN0=eps1**2+eps2**2
# if (eps1.GT.eps2) then
# epsm=eps1
# else
#   epsm=eps2
# END IF
# DO j=k1+2,k2-2
# eps1=r2b(k1,j,x,y,a,b)
# eps2=r2b(j,k2,x,y,a,b)
#
# if (eps1.GT.eps2) then
# epsr=eps1
# else
#   epsr=eps2
# END IF
# if (epsr.LT.epsm) then
# epsm=epsr
# j1=j
# end IF
# end DO
# if (j1.NE.Ni(i)) then
# Ni(i)=j1
# change=.true.
# end IF
# if (i.eq.2) epsm1=epsm
# end DO
#
# if (change) then
# change=.false.
# goto 102
# end if
# ni(nr+1)=n
# eps=epsm
# RETURN
# end
#
#
