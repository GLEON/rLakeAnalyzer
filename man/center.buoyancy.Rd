\name{center.buoyancy}
\alias{center.buoyancy}

\title{
  Calculates the center of buoyancy.
}
\description{
	Calculate the center of buoyancy using buoyancy frequency with a 
  center of mass analysis. Brunt-Vaisala frequency is used for a temperature profile. 
  Negative values for N2 are set to 0 (as they represent transient instabilities or 
  sensor calibration issues) for this calculation.
}

\usage{
	center.buoyancy(wtr, depths)
}

\arguments{
	\item{wtr}{
	a numeric vector of water temperature in degrees C
	}
	  \item{depths}{
	a numeric vector corresponding to the depths (in m) of the wtr measurements
	}
}

\value{
	Returns a value for the center of buoyancy.
}
  
\author{
	Jordan S Read
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{buoyancy.freq}, \code{ts.buoyancy.freq}, \code{center.buoyancy}
}
\examples{

	# A vector of water temperatures
	wtr = c(22.51, 22.42, 22.4, 22.4, 22.4, 22.36, 22.3, 22.21, 22.11, 21.23, 16.42, 
		15.15, 14.24, 13.35, 10.94, 10.43, 10.36, 9.94, 9.45, 9.1, 8.91, 8.58, 8.43)

	#A vector defining the depths
	depths = c(0, 0.5, 1, 1.5, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 
		17, 18, 19, 20)
	
	c.b = center.buoyancy(wtr, depths)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{arith}