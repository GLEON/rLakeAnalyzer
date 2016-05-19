\name{ts.buoyancy.freq}
\alias{ts.buoyancy.freq}

\title{
	Calculate the buoyancy (Brunt-Vaisala) frequency for a temperature profile.
}
\description{
Function for simplifying the calculation of buoyancy frequency. Can usually be called directly on data loaded directly using \code{\link{load.ts}} and
  \code{\link{load.bathy}}.
}


\usage{
ts.buoyancy.freq(wtr, at.thermo=TRUE, na.rm=FALSE, ...)
}

\arguments{
  \item{wtr}{
    A data frame of water temperatures (in Celsius). Loaded using \code{\link{load.ts}}
  }
  \item{at.thermo}{
    Boolean indicating if only buoyancy frequency at the thermocline should be returned. If false, full profile is returned. 
  }
  \item{na.rm}{
  	Boolean indicated if step-by-step removal of NA's should be tried. If false, a timestep with any NA values 
		will likely return an NA value. If true, best effort will be made to calculate indices despite NA values.
  }
  \item{...}{
    Additional parameters will be passed on to \code{thermo.depth} function when extracting buoyancy frequency at only the thermocline. 
    Common parameters to supply would be \code{seasonal} and \code{slope}.
  }
}

\value{Returns a data frame with the timeseries of buoyancy frequency in units \code{sec^-2}. Includes a \sQuote{datetime} column.}
  
\references{
Imberger, J., Patterson, J.C., 1990. \emph{Physical limnology}. Advances in Applied
Mechanics 27, 353-370.
}
\author{
	Luke Winslow
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{buoyancy.freq}
}

\examples{

	#Get the path for the package example file included
	wtr.path <- system.file('extdata', 'Sparkling.daily.wtr', package="rLakeAnalyzer")
	
	#Load data for example lake, Sparkilng Lake, Wisconsin.
	sp.wtr = load.ts(wtr.path)
	
	N2 = ts.buoyancy.freq(sp.wtr, seasonal=FALSE)
	SN2 = ts.buoyancy.freq(sp.wtr, seasonal=TRUE)
	
	plot(N2, type='l', ylab='Buoyancy Frequency', xlab='Date')
	lines(SN2, col='red')
	
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{arith}
