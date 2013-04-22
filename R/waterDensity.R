# T is in ?C
# S is in Pratical Salinity Scale units (dimensionless)
# waterDensity is in grams/Liter

# <<--- Effective range of function: 0-40?C, 0.5-43 Salinity--->>

# -- Author: R. Iestyn. Woolway ----

waterDensity <- function(wtr,S){
  
  # find the number of input arguments
  nargin <- length(as.list(match.call())) -1  

  # If number of input arguments is equal to 1, assume salinity is 0.
  if (nargin == 1){
   S <- wtr*0  
  }
  
  
  # Determine which method we want to use, initially set both methods to false
  MM = FALSE; # Martin & McCutcheon
  UN = FALSE; # UNESCO

  
  # specify temperature and salinity range for the calculations
  Trng <- c(0,40) # temperature range
  Srng <- c(0.5,43) # salinity range
  
  
  # check to see if all values lie within the ranges specified
  if (isTRUE(all(S<Srng[1]))){
    MM <- TRUE # use Martin & McCutcheon
    } else if(!(sum(wtr<Trng[1])||sum(wtr>Trng[2])) && !(sum(S<Srng[1]))||sum(S>Srng[2])){
      UN <- TRUE # use UNESCO
    }
  
  
  # if MM is true we use method of Martin and McCutcheon (1999)
  if (MM){    
    rho <- (1000*(1-(wtr+288.9414)*(wtr-3.9863)^2/(508929.2*(wtr+68.12963))))
  } 
  
  # if UN is true we use method of Martin and McCutcheon (1999)
  if (UN){  
    # -- equation 1:
    rho_0 <- 999.842594 + 6.793952*10^(-2)*wtr - 9.095290*10^(-3)*wtr^2 + 1.001685*10^(-4)*wtr^3 - 1.120083*10^(-6)*wtr^4 + 6.536335e-9*wtr^5
    
    # -- equation 2:
    A <- 8.24493*10^(-1) - 4.0899e-3*wtr + 7.6438*10^(-5)*wtr^2 - 8.2467*10^(-7)*wtr^3 + 5.3875*10^(-9)*wtr^4
    
    # -- equation 3:
    B <- -5.72466*10^(-3) + 1.0227*10^(-4)*wtr - 1.6546*10^(-6)*wtr^2
    
    # -- equation 4:
    C <- 4.8314*10^(-4)
    
    # -- equation 5:
    rho <- rho_0 + A*S + B*S^(3/2) + C*S  
  } 
  
  # if there is a combination of fresh and saline water we need to use a combination of MM and UN
  if (MM == FALSE && UN == FALSE){
    rho <- wtr*0
    for (j in 1:length(rho)){
      rho[j] <- waterDensity(wtr[j],S[j])
    }    
    dim(rho) <- dim(wtr) # ensure same dimension as input array 
  }  
  return(rho) 
}

# -- References
# ?? Martin, J.L., McCutcheon, S.C., 1999. Hydrodynamics and Transport ??
# ?? for Water Quality Modeling. Lewis Publications, Boca              ??
# ?? Raton, FL, 794pp. >>
#
#?? Millero, F.J., Poisson, A., 1981. International one-atmosphere    ??
#?? equation of state of seawater. UNESCO Technical Papers in Marine  ??
#?? Science. No. 36. >>
