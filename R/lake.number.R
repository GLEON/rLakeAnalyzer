#' @title Calculate Lake Number
#' 
#' @description The Lake Number, defined by Imberger and Patterson (1990), has been used to
#' describe processes relevant to the internal mixing of lakes induced by wind
#' forcings. Lower values of Lake Number represent a higher potential for
#' increased diapycnal mixing, which increases the vertical flux of mass and
#' energy across the metalimnion through the action of non-linear internal
#' waves. Lake Number is a dimensionless index.
#' 
#' Lake Number has been used, for example, to estimate the flux of oxygen
#' across the thermocline in a small lake (Robertson and Imberger, 1994), and
#' to explain the magnitude of the vertical flux of ammonium in a lake (Romero
#' et al., 1998).
#' 
#' @param bthA a numeric vector of cross sectional areas (m2) corresponding to
#' bthD depths, hypsographic areas
#' @param bthD a numeric vector of depths (m) which correspond to areal
#' measures in bthA, hypsographic depths
#' @param uStar a numeric array of u* (m/s), water friction velocity due to
#' wind stress
#' @param St a numeric array of Schmidt stability (J/m2), as defined by Idso
#' 1973
#' @param metaT a numeric array of the top of the metalimnion depth (m from the
#' surface)
#' @param metaB a numeric array of the bottom of the metalimnion depth (m from
#' the surface)
#' @param averageHypoDense a numeric array of the average density of the
#' hypolimnion (kg/m3)
#' @return A numeric vector of Lake Number [dimensionless]
#' @seealso \code{\link{ts.lake.number}} \code{\link{wedderburn.number}}
#' @references Imberger, J., Patterson, J.C., 1990. \emph{Physical limnology}.
#' Advances in Applied Mechanics 27, 303-475.
#' 
#' Idso, S.B., 1973. \emph{On the concept of lake stability}. Limnology and
#' Oceanography 18, 681-683.
#' @keywords manip
#' @examples
#' 
#' 	bthA	<-	c(1000,900,864,820,200,10)
#' 	bthD	<-	c(0,2.3,2.5,4.2,5.8,7)
#' 	uStar	<-	c(0.0032,0.0024)
#' 	St	<-	c(140,153)
#' 	metaT	<-	c(1.34,1.54)
#' 	metaB	<-	c(4.32,4.33)
#' 	averageHypoDense	<-	c(999.3,999.32)
#' 	cat('Lake Number for input vector is: ')
#' 	cat(lake.number( bthA, bthD, uStar, St, metaT, metaB, averageHypoDense) )
#' 
#' @export
lake.number <- function(bthA,bthD,uStar,St,metaT,metaB,averageHypoDense){
	g	<-	9.81
	dz	<-	0.1
	# if bathymetry has negative values, remove.
	# intepolate area and depth to 0
	
	# - implement here -, return proper format NOT DONE


	Ao	<-	bthA[1]
	Zo	<-	bthD[1]
	if (Ao==0){stop('Surface area cannot be zero, check *.bth file')}
	    
	#interpolates the bathymetry data
	layerD	<-	seq(Zo,max(bthD),dz)
	layerA	<-	stats::approx(bthD,bthA,layerD)$y


	#find depth to the center of volume
	Zv = layerD*layerA*dz                    
	Zcv = sum(Zv)/sum(layerA)/dz
	St_uC = St*Ao/g
	# Calculates the Lake Number according to the formula provided
	Ln = g*St_uC*(metaT+metaB)/(2*averageHypoDense*uStar^2*Ao^(3/2)*Zcv)
	return(Ln)
}

