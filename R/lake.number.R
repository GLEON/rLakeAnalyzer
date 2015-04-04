#----Author: Jordan S Read 2009 ----
#----converted to R 2013-04-21 ----

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
	layerA	<-	approx(bthD,bthA,layerD)$y


	#find depth to the center of volume
	Zv = layerD*layerA*dz                    
	Zcv = sum(Zv)/sum(layerA)/dz
	St_uC = St*Ao/g
	# Calculates the Lake Number according to the formula provided
	Ln = g*St_uC*(metaT+metaB)/(2*averageHypoDense*uStar^2*Ao^(3/2)*Zcv)
	return(Ln)
}

