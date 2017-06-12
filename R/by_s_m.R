#' Service subroutine for determining Mixed Layer Depth for a SPECIFIED ERROR NORM VALUE
#'
#' @param thres error norm;
#' @param z0 initial depth: use to omit data above z0
#' @param zmax maximum depth: use to omit data below zmax
#' @param z input x data array, should be increasing function of index
#' @param sigma input y data array
#'
#' @return list(nimax=nimax,by_s_m=ss, cline=cline,smz=smz,sms=sms)
#' \itemize{
#' \item nimax: number of segments
#' \item smz: final z array of segmented data
#' \item sms: final sigma array of segmented data
#' \item by_s_m: position of MLD = smz(2); or -99 if something is not right
#' \item cline: Cline depth is defined as the midpoint of the segment connecting inflection points that has the maximum slope
#' }
#'
#' @export
#' @description Service subroutine for determining Mixed Layer Depth for a SPECIFIED ERROR NORM VALUE
#'
by_s_m = function(thres=thres,z0=z0,zmax=zmax,z=z,sigma=sigma) {
  # by_s_m=-99.0 # TODONE: this is for an error return, crash instead.
  nn=800 # TODO: why?
  #nn=length(z)

  # finding initial s-level
  i1 = 1 + sum(z<z0)   # Find index of first z[] less than z0
  # if(i1==length(z)) return()  # TODONE: probably should crash here
  if(i1==length(z)) {
    stop("Initial depth, z0, excludes all depths in data, z!")
    # return()  # TODONE: probably should crash here
  }

  sigma0 = sigma[i1]

  # finding second s-level
  i2 = sum(z<=zmax)  # Find index prior to first z[] greater than zmax

  dz = (z[i2]-z[i1])/nn

  results = getxxnorm(z[i1:i2],sigma[i1:i2])

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

  list(nimax=nimax,by_s_m=ss, smz=smz,sms=sms)
}


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

by_s_m3 = function(nr,z0,zmax,z,sigma) {
  # by_s_m=-99.0 # TODONE: why?
  nn=800 # TODO: why?
  #nn=length(z)
  
  # finding initial s-level
  i1 = 1 + sum(z<z0)   # Find index of first z[] less than z0
  if(i1==length(z)) {
    stop("Initial depth, z0, excludes all depths in data, z!")
    # return()  # TODO: probably should crash here
  }
  sigma0 = sigma[i1]
  
  # finding second s-level
  #TODO: look into why by_s_m.R has a different loop. One must be wrong.
  i2 = sum(z<zmax)  # Find index prior to first z[] greater than zmax
  
  dz = (z[i2]-z[i1])/nn
  
  results = getxxnorm(z[i1:i2],sigma[i1:i2])
  
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
  
  
  list(eps=s_mNresults$eps, by_s_m=ss,smz=smz,sms=sms)
}

