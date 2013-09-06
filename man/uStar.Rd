\name{uStar}
\alias{uStar}

\title{Calculates the water friction velocity, uStar}
\description{uStar is the water friction velocity due to wind stress at the lake surface, it is calculated following the methods of Imberger (1985) as a function of the shear stress of air (Fischer et al., 1979), drag coefficient for momentum (Hicks, 1972), and a dimensionless constant (von Karman constant) that decribes the logarithmic velocity profile at the air-water interface}
\usage{
uStar(wndSpeed, wndHeight, averageEpiDense)
}

\arguments{
  \item{wndSpeed}{a numeric vector of wind speed in m s-1}
  \item{wndHeight}{a numeric vector of wind measurement height in m}
  \item{averageEpiDense}{a numeric vector of epilimnion density in kg m-3}
}

\value{a numeric vector of uStar}
\references{
  
  Hicks, B.B., 1972. \emph{A procedure for the formulation of bulk transfer coefficients over water bodies of different sizes}. Boundary-Layer Meterology 3: 201-213.
  
  Amorocho, J., DeVries, J.J., 1980. \emph{A new evaluation of the wind stress coefficient over water surfaces}. Journal of Geophysical Research 85: 433-442.
  
  Fischer, H.B., List, E.J., Koh, R.C.Y., Imberger, J., Brooks, N.H., 1979. \emph{Mixing in inland and coastal waters}. Academic Press.
  
  Imberger, J., 1985. \emph{The diurnal mixed layer}. Limnology and Oceanography 30: 737-770.
}

\author{R. Iestyn. Woolway}

\seealso{\code{layer.density}}

\examples{

  wndSpeed  <- c(5.1,6.3,6.3,5.2,7,7.2)
  wndHeight	<-	2
  averageEpiDense	<- c(14,15,14.2,13,12,12)

  cat('uStar for input vector is: ')
  cat(uStar(wndSpeed,wndHeight,averageEpiDense))
}

\keyword{manip}
