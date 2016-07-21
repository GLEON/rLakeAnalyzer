\name{ts.center.buoyancy}
\alias{ts.center.buoyancy}

\title{
	Calculates the center of buoyancy for multiple temperature profiles.
}
\description{
Function for simplifying the calculation of the center of buoyancy. Can usually be called directly on data loaded directly using \code{\link{load.ts}} and
  \code{\link{load.bathy}}.
}

\usage{
ts.center.buoyancy(wtr, na.rm=FALSE)
}

\arguments{
  \item{wtr}{
    A data frame of water temperatures (in Celsius). Loaded using \code{\link{load.ts}}
  }
  \item{na.rm}{
  	Boolean indicated if step-by-step removal of NA's should be tried. If false, a timestep with any NA values 
	  will return an NA value. If true, best effort will be made to calculate indices despite NA values.
  }
}

\value{Returns a data frame with the timeseries of the center of buoyancy frequency. Includes a \sQuote{datetime} column.}
  
\references{
Imberger, J., Patterson, J.C., 1990. \emph{Physical limnology}. Advances in Applied
Mechanics 27, 353-370.
}
\author{
	Jordan S Read
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{center.buoyancy}, \code{load.bathy}, \code{load.ts}
}

\examples{

  #Get the path for the package example file included
  wtr.path <- system.file('extdata', 'Sparkling.daily.wtr', package="rLakeAnalyzer")
	
  #Load data for example lake, Sparkilng Lake, Wisconsin.
  sp.wtr = load.ts(wtr.path)
	 
  #calculate and plot the thermocline depth
  t.d = ts.thermo.depth(sp.wtr)
  
  center.N2 = ts.center.buoyancy(sp.wtr)
	
  plot(center.N2, type='l', ylab='Depth (m)', xlab='Date', ylim=c(19,0), lwd = 1.5)
  lines(t.d, type='l', col='red', lwd = 1.5)
  legend(x = t.d[3,1], y = .25,
     c('center of buoyancy','thermocline depth'),
     lty=c(1,1),
     lwd=c(1.5,1.5),col=c("black","red"), bty = "n")
	
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{arith}
