\name{water.density}
\alias{water.density}
\title{Estimate Water Density}
\description{Density of water from temperature and salinity}
\usage{
water.density(wtr, sal = wtr*0)
}
\arguments{
\item{wtr}{ a numeric vector of water temperature in °C}
\item{sal}{ a numeric vector of salinity in Practical Salinity Scale units}
}
\details{

}
\value{
A numeric vector of water densities in kg/m^3.
}
\author{
R. Iestyn. Woolway, Luke Winslow
}
\examples{
#Plot water density for water between 1 and 30 deg C
dens = water.density(1:30)
plot(1:30, dens, xlab="Temp(deg C)", ylab="Density(kg/m^3)")
}

\references{
Martin, J.L., McCutcheon, S.C., 1999. \emph{Hydrodynamics and Transport for Water Quality Modeling.} Lewis Publications, Boca Raton, FL, 794pp.

Millero, F.J., Poisson, A., 1981. \emph{International one-atmosphere equation of state of seawater.} UNESCO Technical Papers in Marine Science. No. 36.
}

\keyword{limnology}