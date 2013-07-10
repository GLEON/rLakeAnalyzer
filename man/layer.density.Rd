\name{layer.density}
\alias{layer.density}
\title{
  Returns the average density of a layer between two depths.
}
\description{ 
This function calculates the average density of a layer of water between two depths.
}
\usage{
layer.density(top, bottom, wtr, depths, bthA, bthD, sal)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{top}{ Numeric value of the depth (m) of the top of the layer from the water surface}
  \item{bottom}{ Numeric value of the depth (m) of the bottom of the layer from the water surface}
  \item{wtr}{ Numeric vector of water temperature in °C}
  \item{depths}{ Numeric vector of depths (m) corresponding to water temperature vector}
  \item{bthA}{ Numeric vector of water body cross sectional area (m2) corresponding to bthD depths}
  \item{bthD}{ Numeric vector of water body bathymetric depths (m) corresponding to areal bthA values}
  \item{sal}{ Optional numeric vector of salinity in Practical Salinity Units corresponding to water temperature vector. If left blank, salinity is set to be zero}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Numeric value of average water density for bounded layer in kg/m^3
}
\references{
  %% ~put references to the literature/web site here ~
}
\author{
Jake Zwart
}
\note{
water.density function is required to run the layer.density function
}
%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{water.density}
}
\examples{
top     <- 2
bottom  <- 6
wtr     <- c(25.2,25.1,24.1,22.0,19.8,15.3,12.0,11.1)
depths  <- c(0,1,2,3,4,5,6,7) 
bthA    <- c(10000,8900,5000,3500,2000,1000,300,10)
bthD    <- c(0,1,2,3,4,5,6,7)
layer.density(top,bottom,wtr,depths,bthA,bthD)
}

\keyword{limnology}
\keyword{GLEON}