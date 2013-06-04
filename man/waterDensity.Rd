\name{waterDensity}
\alias{waterDensity}
\title{Water Density}
\description{Density of water from temperature and salinity}
\usage{
waterDensity(temp, ...)
\method{waterDensity}{default}(temp, sal, ...)
}
\arguments{
\item{temp}{ a numeric design matrix for the model. }
\item{sal}{ a numeric vector of responses. }
\item{\dots}{ not used. }
}
\value{
An object of class \code{logreg}, basically a list including elements
\item{coefficients}{ a named vector of coefficients }
\item{vcov}{ covariance matrix of coefficients }
\item{fitted.values}{ fitted values }
\item{residuals}{ residuals }
}
\author{Friedrich Leisch}
\examples{
data(cats, package="MASS")
mod1 <- linmod(Hwt~Bwt*Sex, data=cats)
mod1
summary(mod1)
}
\keyword{regression}