
#' @title Estimate hypsography curve
#' 
#' @description Estimates a depth-area curve for a lake using lake surface area, 
#' maximum depth and mean depth. Two methods for estimating the curve are available;
#' 'cone' assumes the lake is shaped as a cone and requires only surface area and 
#' maximum depth; "voldev" uses the volume development (Vd) parameter from Håkanson (1981) 
#' and Johansson et al. (2007). Vd is a dimensionless parameter that describes 
#' lake basin shape in relation to the volume of cone whose base area and height 
#' equal the surface area and maximum lake depth, it is estimated as Vd = Zmean/Zmax (Håkanson et al. 2000). 
#' Method "voldev' requires lake surface area, mean and maximum depth. Depths at 
#' which the area is estimated can be set by as a numeric vector or as a regularly spaced sequence.
#' 
#' @param Zmax a single value of the maxiumum depth of the lake (in m)
#' @param Zmean a single value of the mean depth of the lake (in m)
#' @param lkeArea a sinlge value of the surface area of the lake (in m^2)
#' @param depths a numeric vector of depths (in m) at which areas are estimated. 
#' If not specified depths is regularly spaced sequence of values with the interval set by zinterval. 
#' @param zinterval  a sinlge value defining the depth interval at which volumes should be calculated, default is 1 m.
#' @param method specifies the method used to estimate depth-area relationship, can be "cone"(default) or "voldev". Method "voldev" requires Zmean. See notes for details.
#' 
#' @return a dataframe which defines the lake area for each depth. Columns are depths (m) and Area.at.z (m^2). Area at 0 m should equal the user entered lkeArea.
#' 
#' @references Håkanson, L. (1981). On lake bottom dynamics – the energy– topography factor. Canadian Journal of Earth Sciences, 18, 899–909.
#' Johansson, H., A. A. Brolin, and L. Håkanson. 2007. New approaches to the modelling of lake basin morphometry. Environ. Model. Assess. 12: 213–228.
#'
#' @examples  
#' Voldev.ex = approx.bathy(Zmax = 25, Zmean = 12, lkeArea = 39400000, method = "voldev")
#' Voldevshallow.ex = approx.bathy(Zmax = 25, Zmean = 6, lkeArea = 39400000, method = "voldev")
#' Cone.ex = approx.bathy(Zmax = 25, lkeArea = 39400000, method = "cone")
#' 
#'# plot depth-area curves
#'   plot(Cone.ex$depths ~ Cone.ex$Area.at.z, xlab = "Area (m^3)", ylab = "Depth (m)")
#'   points(Voldev.ex$depths ~ Voldev.ex$Area.at.z, col = "red")
#'   points(Voldevshallow.ex$depths ~ Voldevshallow.ex$Area.at.z, col = "blue")
#'  
#' 
#' @export

approx.bathy <- function(Zmax, lkeArea, Zmean = NULL, method = "cone", zinterval = 1, depths = seq(0, Zmax, by = zinterval)){
  Area = c()
  if(method == "cone"){
      area <- approx(c(0, Zmax), c(lkeArea, 0), depths)$y
    Area = data.frame(depths = depths, Area.at.z = area)
  } #end of "cone"
  
  if(method == "voldev"){
    if(is.null(Zmean)){
      stop("Zmean required for method 'vd'. Use method 'cone' if Zmean is unknown")
    }
      Zrel = depths/Zmax
      Vd = 3*(Zmean/Zmax) 
      fVd = (1.7/Vd) + 2.5 - 2.4*Vd +0.23*Vd^3 # derived from Håkanson, L. (1981) as cited in Johansson et al. 2007
      for (i in 1:length(Zrel)){
        Area.at.z = lkeArea*( (1-Zrel[i]) * (1 + Zrel[i]*sin(sqrt(Zrel[i])) ) )^fVd
        tmp = data.frame(depths = depths[i], Area.at.z = Area.at.z)
        Area = rbind(Area, tmp)
      }# end of for loop
  } #end of "vd"
  return(Area)
}#end of function

