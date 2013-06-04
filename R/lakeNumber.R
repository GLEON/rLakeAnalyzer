#----Author: Jordan S Read 2009 ----
#----converted to R 2013-04-21 ----

lakeNumber <- function(bthA,bthD,uStar,St,metaT,metaB,averageHypoDense){
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
	layerA	<-	approx(bthD,bthA,layerD)$y


	#find depth to the center of volume
	Zv = layerD*layerA*dz                    
	Zcv = sum(Zv)/sum(layerA)/dz
	St_uC = St*Ao/g
	# Calculates the Lake Number according to the formula provided
	Ln = g*St_uC*(metaT+metaB)/(2*averageHypoDense*uStar^2*Ao^(3/2)*Zcv)
<<<<<<< HEAD
=======
	return(Ln)
>>>>>>> 8ab15ecb1076eecbb2d74836aa8051792ecfce33
}

# -- References:
#°° Imberger, Jorg, and John C. Patterson. "Physical Limnology." Advances in °°
#°° Applied Mechanics 27 (1990): 314-317.