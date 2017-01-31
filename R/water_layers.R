#' Modified version for by_s_m.R - for water layer calculation. This function outputs a data.frame rather than a list
#'
#' @param thres error norm;
#' @param z0 initial depth: use to omit data above z0
#' @param zmax maximum depth: use to omit data below zmax
#' @param z input x data array, should be increasing function of index
#' @param sigma input y data array
#'
#' @return data.frame(nimax=nimax,by_s_m=ss)
#' nimax: number of segments
#' by_s_m: position of MLD = smz(2); or -99 if something is not right
#'
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
water_layers = function(thres=thres,z0=z0,zmax=zmax,z=z,sigma=sigma) {
  by_s_m=-99.0 # TODO: this is for an error return, crash instead.
  nn=800 # TODO: why?
  
  # finding initial s-level
  i1 = 1 + sum(z<z0)   # Find index of first z[] less than z0
  if(i1==length(z)) return()  # TODO: probably should crash here
  sigma0 = sigma[i1]
  
  # finding second s-level
  i2 = sum(z<=zmax)  # Find index prior to first z[] greater than zmax
  
  dz = (z[i2]-z[i1])/nn
  
  # SUBROUTINE GET_XX_norm(anormx,anormy,N,       N1, X0,    DX, X,     Y,         XX,YY)
  # call GET_XX_norm(      ax,    ay,    i2-i1+1, nn, z(i1), dz, z(i1), sigma(i1), XX,YY)
  results = getxxnorm(z[i1:i2],sigma[i1:i2],nn,z[i1],dz)
  
  ni = s_m_p(thres,results$xx,results$yy)
  k=ni[2]
  ax = results$anormx
  ay = results$anormy
  ss=0.5*( results$xx[k]+results$xx[k-1] )*ax + z[i1]
  
  nimax = min( 100, length(ni)-1 ) #TODO: why 100?
  smz = rep(0,nimax)  # Reserve space
  sms = rep(0,nimax)  # Reserve space
  smz[1] = z[i1]
  sms[1] = sigma[i1]
  i = 2:(nimax+1)
  k = ni[i]
  smz[i] = 0.5*(results$xx[k]+results$xx[k-1])*ax + z[i1]
  sms[i] = 0.5*(results$yy[k]+results$yy[k-1])*ay + sigma[i1]
  
  #list(nimax=nimax,smz=smz,sms=sms,by_s_m=ss)
  return(data.frame(nimax=nimax, by_s_m=ss))
}