#' Service subroutine for determining Mixed Layer Depth for a SPECIFIED ERROR NORM VALUE
#'
#' @param thres error norm;
#' @param z0 initial depth: use to omit data above z0
#' @param zmax maximum depth: use to omit data below zmax
#' @param z input x data array, should be increasing function of index
#' @param sigma input y data array
#'
#' @return Buncha stuff
#' @export
#' @description Service subroutine for determining Mixed Layer Depth for a SPECIFIED ERROR NORM VALUE
#' # c     (This subroutine calls the S_M_P subroutine)
#' c       Input:
#' C          N     -[INTEGER] number of points;
#' C          THRES -[REAL]  error norm;
#' C          Z0    -[REAL]  initial depth: use to omit data above z0
#' C          ZMAX  -[REAL]  maximum depth: use to omit data below zmax
#' C
#' C          Z     -[REAL(N)] input x data array, should be increasing function of index
#' C          SIGMA -[REAL(N)] input y data array
#' C       Output:
#' C          NIMAX -[INTEGER] final number of segments;
#' C          SMZ   -[REAL(NIMAX)] final z array of segmented data
#' C          SMS   -[REAL(NIMAX)] final sigma array of segmented data
#' C          VY_S_M-[REAL] position of MLD (=SMZ(2)).
#' C                      return -99 if something is not right.
#' C
#'
by_s_m = function(thres,z0,zmax,z,sigma) {
  by_s_m=-99.0 # TODO: why?
  nn=800 # TODO: why?

  # finding initial s-level
  i1 = 1 + sum(z<z0)   # Find index of first z[] less than z0
  if(i1==length(z)) return()  # TODO: probably should crash here
  sigma0 = sigma(i1)

  # finding second s-level
  i2 = sum(z<=zmax)  # Find index prior to first z[] less than or eq to z0

  dz = (z[i2]-z[i1])/nn

  # SUBROUTINE GET_XX_norm(anormx,anormy,N,       N1, X0,    DX, X,     Y,         XX,YY)
  # call GET_XX_norm(      ax,    ay,    i2-i1+1, nn, z(i1), dz, z(i1), sigma(i1), XX,YY)
  df = getxxnorm(z[i1:i2],sigma[i1:i2],nn,z[i1],dz)

  ni = s_m_p(thres,df$xx,df$yy)
  k=ni[2]
  ax = df$anormx
  ay = df$anormy
  ss=0.5*( xx[k]+xx[k-1] )*ax + z[i1]

  nimax = min(100,length(ni)-1) #TODO: why 100?
  smz = rep(0,nimax)
  sms = rep(0,nimax)
  smz[1] = z[i1]
  sms[1] = sigma[i1]
  i = 2:(nimax+1)
  k = ni[i]
  smz[i] = 0.5*(xx[k]+xx[k-1])*ax + z[i1]
  sms[i] = 0.5*(yy[k]+yy[k-1])*ay + sigma(i1)

  data.frame(nimax=nimax,smz=smz,sms=sms,by_s_m=ss)
}

# Original Fortran code follows.
# REAL FUNCTION BY_S_M(N,NIMAX,THRES,Z0,ZMAX,Z,SIGMA,SMZ,SMS)
# c     Service subroutine for determining Mixed Layer Depth for a SPECIFIED ERROR NORM VALUE
# c     (This subroutine calls the S_M_P subroutine)
# c       Input:
#   C          N     -[INTEGER] number of points;
# C          THRES -[REAL]  error norm;
# C          Z0    -[REAL]  initial depth: use to omit data above z0
# C          ZMAX  -[REAL]  maximum depth: use to omit data below zmax
# C
# C          Z     -[REAL(N)] input x data array, should be increasing function of index
# C          SIGMA -[REAL(N)] input y data array
# C       Output:
#   C          NIMAX -[INTEGER] final number of segments;
# C          SMZ   -[REAL(NIMAX)] final z array of segmented data
# C          SMS   -[REAL(NIMAX)] final sigma array of segmented data
# C          VY_S_M-[REAL] position of MLD (=SMZ(2)).
# C                      return -99 if something is not right.
# C
#
# real z(N),sigma(N),smz(N),sms(N)
# real xx(1000),yy(1000)
# integer ni(400)
# by_s_m=-99.0
# NN=800
# c     finding initial s-level
# i=1
# do while (z(i).lt.z0.and.i.le.n)
#   i=i+1
# end do
# if (i.eq.n) return
# i1=i
# sigma0=sigma(i)
#
# c     finding second s-level
# do while (z(i).le.zmax.and.i.le.n)
#   i=i+1
# end do
# i2=i-1
#
# dz=(z(i2)-z(i1))/nn
# call GET_XX_norm(ax,ay,i2-i1+1,nn,z(i1),dz,z(i1),sigma(i1),XX,YY)
# call s_m_p(nn,thres,xx,yy,Nr,Ni)
# k=ni(2)
# ss=0.5*(xx(k)+xx(k-1))*ax+z(i1)
#
#
# nimax=nr
# smz(1)=z(i1)
# sms(1)=sigma(i1)
# if (nimax.gt.100)nimax=100
# do i=2,nimax+1
# k=ni(i)
# smz(i)=0.5*(xx(k)+xx(k-1))*ax+z(i1)
# sms(i)=0.5*(yy(k)+yy(k-1))*ay+sigma(i1)
# end do
# by_s_m=ss
# return
# end
