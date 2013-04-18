\name{linmod}
\alias{linmod}
\alias{linmod.default}
\alias{linmod.formula}
\alias{print.linmod}
\alias{predict.linmod}
\alias{summary.linmod}
\alias{print.summary.linmod}
\title{Linear Regression}
\description{Fit a linear regression model.}
\usage{
linmod(x, ...)
\method{linmod}{default}(x, y, ...)
\method{linmod}{formula}(formula, data = list(), ...)
\method{print}{linmod}(x, ...)
\method{summary}{linmod}(object, ...)
\method{predict}{linmod}(object, newdata=NULL, ...)
}
\arguments{
\item{x}{ a numeric design matrix for the model. }
\item{y}{ a numeric vector of responses. }
\item{formula}{ a symbolic description of the model to be fit. }
\item{data}{ an optional data frame containing the variables in the model. }
\item{object}{ an object of class \code{"linmod"}, i.e., a fitted model. }
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