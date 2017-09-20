# ---Author: Jake Zwart, 2013-04-21 --- 
#translated from Matlab Script - Author Jordan Read, 2009 
## from wedderburnNumber.m in https://github.com/jread-usgs/Lake-Analyzer/



#' Calculates Wedderburn Number for a lake.
#' 
#' Wedderburn Number (Wn) is a dimensionless parameter measuring the balance
#' between wind stress and bouyancy force and is used to estimate the amount of
#' upwelling occuring in a lake.  When Wn is much greater than 1, the bouyancy
#' force is much greater than the wind stress and therefore there is a strong
#' vertical stratification with little horizontal variation in the
#' stratification. When Wn is much less than 1, the wind stress is much greater
#' than the bouyancy force and upwelling is likely occuring at the upwind end
#' of the lake. When Wn is near 1, the bouyance force and wind stress are
#' nearly equal and horizontal mixing is considered important
#' 
#' 
#' @param delta_rho Numeric value for the water density difference between the
#' epilimnion and hypolimnion (kg/m^3)
#' @param metaT Numeric value for the thickness of the water body's surface
#' layer (m)
#' @param uSt Numeric value for the water friction velocity due to wind stress
#' (m/s)
#' @param Ao Numeric value for the water body surface area (m^2) at zero meters
#' depth
#' @param AvHyp_rho Numeric value for the average water density of the
#' hypolimnion layer (kg/m^3)
#' @return The dimensionless numeric value of Wedderburn Number
#' @author Jake Zwart
#' @seealso \code{\link{ts.wedderburn.number}} \code{\link{lake.number}}
#' @references Imberger, J., Patterson, J.C., 1990. \emph{Physical limnology}.
#' Advances in Applied Mechanics 27, 353-370.
#' @keywords arith
#' @examples
#' 
#' delta_rho <- c(3.1,1.5)
#' metaT <- c(5.5,2.4)
#' uSt <- c(0.0028,0.0032)
#' Ao <- c(80300,120000)
#' AvHyp_rho <- c(999.31,999.1)
#' wedderburn.number(delta_rho, metaT, uSt, Ao, AvHyp_rho)
#' 
#' @export
#' 
wedderburn.number <- function(delta_rho,metaT,uSt,Ao,AvHyp_rho){
  
  # Calculates the Wedderburn number for a particular system using the following equation:
  #
  #  W = (g*delta_rho*(h^2))/(pHyp*(uSt^2)*Lo)
  #
  # where
  #   g = force of gravity
  #   delta_rho = density difference between the epilimnion and the hypolimnion
  #   metaT = thickness of the surface layer
  #   uSt = water friction velocity due to wind stress 
  #   Lo = fetch length in the direction of the wind.
  #
  
  #Constants
  g = 9.81                   #force of gravity
  
  Lo = 2 * sqrt(Ao/pi);      #Length at thermocline depth
  
  go = g*delta_rho/AvHyp_rho;
  
  # Calculates W according to formula provided
  W = go*metaT^2/(uSt^2*Lo);
  return(W)
}

