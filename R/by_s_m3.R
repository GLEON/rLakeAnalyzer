#' Service subroutine for determining Mixed Layer Depth for a SPECIFIED NUMBER OF SEGMENTS
#' 
#' @param nr fixed number of segments
#' @param z0 initial depth: use to omit data above z0
#' @param zmax maximum depth: use to omit data below zmax
#' @param z input x data array, should be increasing function of index
#' @param sigma input y data array
#'
#' @return list(eps=s_mNresults$eps, cline=cline, by_s_m=ss,smz=smz,sms=sms)
#' \itemize{
#' \item eps: the maximum error over all intervals.
#' \item smz: final z array of segmented data
#' \item sms: final sigma array of segmented data
#' \item by_s_m: position of MLD = smz(2); or -99 if something is not right
#' \item cline: Cline depth is defined as the midpoint of the segment connecting inflection points that has the maximum slope
#' }
#'
#' @export
#' @description Service subroutine for determining Mixed Layer Depth for a SPECIFIED ERROR NORM VALUE
#' 
#' @note ForTran description - Input (Calls subroutine S_mN)
##' \itemize{
##' \item{N -[INTEGER] number of points}
##' \item{NR  -[INTEGER] number of segments (fixed)}
##' \item{X   -[REAL(N)] input x data array, should be increasing function of index}
##' \item{Y   -[REAL(N)] input y data array}
##' }
#' @note ForTran description - Output
##' \itemize{
##' \item {NI  -[INTEGER] final array with segment start points}
##' }


by_s_m3 = function(nr,z0,zmax,z,sigma) {
  by_s_m=-99.0 # TODO: why?
  #nn=800 # TODO: why?
  nn=length(z) 

  # finding initial s-level
  i1 = 1 + sum(z<z0)   # Find index of first z[] less than z0
  if(i1==length(z)) return()  # TODO: probably should crash here
  sigma0 = sigma[i1]

  # finding second s-level
  #TODO: look into why by_s_m.R has a different loop. One must be wrong.
  i2 = sum(z<zmax)  # Find index prior to first z[] greater than zmax

  dz = (z[i2]-z[i1])/nn

  # SUBROUTINE GET_XX_norm(anormx,anormy,N,       N1, X0,    DX, X,     Y,         XX,YY)
  # call GET_XX_norm(      ax,    ay,    i2-i1+1, nn, z(i1), dz, z(i1), sigma(i1), XX,YY)
  results = getxxnorm(z[i1:i2],sigma[i1:i2],nn,z[i1],dz)

  s_mNresults = s_mN(nr,results$xx,results$yy)
  ni = s_mNresults$ni

  k=ni[2]
  ax = results$anormx
  ay = results$anormy
  ss=0.5*( results$xx[k]+results$xx[k-1] )*ax + z[i1]

  # nimax = min( 100, length(ni)-1 ) #TODO: why 100?
  smz = rep(0,nr)  # Reserve space
  sms = rep(0,nr)  # Reserve space
  smz[1] = z[i1]
  sms[1] = sigma[i1]
  i = 2:(nr+1)
  k = ni[i]
  smz[i] = 0.5*(results$xx[k]+results$xx[k-1])*ax + z[i1]
  sms[i] = 0.5*(results$yy[k]+results$yy[k-1])*ay + sigma[i1]
  
  ##Thermocline depth is defined as the midpoint of the segment connecting inflection points that has the maximum slope (–dT/dz). Fielder 2010
  cline <- mean(smz[c(which.max(diff(smz)/diff(sms)),which.max(diff(smz)/diff(sms))+1)])

  list(eps=s_mNresults$eps, cline=cline, by_s_m=ss,smz=smz,sms=sms)
}

# real FUNCTION BY_s_m3(n,nimax,thres,z0,z,zmax,sigma,smz,sms)
# C     (This is the service subroutine for the case of a SPECIFIC ERROR NORM)
# c     (Calls subroutine S_mN)
# C       Input:
#   C          N   -[INTEGER] number of points;
# C          NR  -[INTEGER] number of segments (fixed);
# C          X   -[REAL(N)] input x data array, should be increasing function of index
# C          Y   -[REAL(N)] input y data array
# C       Output:
#   C          NI  -[INTEGER] final array with segment start points
# real z(n),sigma(n),smz(n),sms(n)
# real xx(1000),yy(1000)
# integer ni(400)
# by_s_m3=-99.0     ! Compiler is warning could be uninitialized.
# !      by_s_m=-99.0
# nn=800
# c     finding initial s-level
# i=1
# do while (z(i).lt.z0.and.i.lt.n)
#   i=i+1
# end do
# if (i.eq.n) return
# i1=i
# sigma0=sigma(i)
#
# c     finding second s-level
#
# do while (z(i).le.zmax.and.i.lt.n)
#   i=i+1
# end do
# i2=i-1
#
# dz=(z(i2)-z(i1))/nn
# call GET_XX_norm(ax,ay,i2-i1+1,nn,z(i1),dz,z(i1),sigma(i1),XX,YY)
# Nr=nimax
# call s_mN(nn,thres,xx,yy,Nr,Ni)
# k=ni(2)
# ss=0.5*(xx(k)+xx(k-1))*ax+z(i1)
# smz(1)=z(i1)
# sms(1)=sigma(i1)
# do i=2,nimax+1
# k=ni(i)
# smz(i)=0.5*(xx(k)+xx(k-1))*ax+z(i1)
# sms(i)=0.5*(yy(k)+yy(k-1))*ay+sigma(i1)
# end do
# by_s_m3=ss
# return
# end
#
