\name{ts.layer.temperature}
\alias{ts.layer.temperature}

\title{
	Calculate volume-weighted average water temperature across a range of depths for a timeseries.
}
\description{
Function for simplifying the calculation of Wedderburn Number. Can usually be called directly on data loaded directly using \code{\link{load.ts}} and
  \code{\link{load.bathy}}.
}


\usage{
	ts.layer.temperature(wtr, top, bottom, bathy, na.rm=FALSE)
}

\arguments{
  \item{wtr}{
    A data frame of water temperatures (in Celsius). Loaded using \code{\link{load.ts}}
  }
  \item{top}{
	Either a single numeric depth value to be used across the entire timeseries, or a vector of same 
	length as the timeseries (e.g., \code{nrow(wtr)}). This is useful when calculating a time-varying layer 
	average, like average epilimnion temperature.
  }
  \item{bottom}{
	Either a single numeric depth value to be used across the entire timeseries, or a vector of same 
	length as the timeseries (e.g., \code{nrow(wtr)}). This is useful when calculating a time-varying layer 
	average, like average epilimnion temperature.
  }
  \item{bathy}{
    A data frame containing hypsometric data. Loaded using \code{\link{load.bathy}}
  }
  \item{na.rm}{
	Boolean indicated if step-by-step removal of NA's should be tried. If false, a timestep with any NA values 
	will return an NA value. If true, best effort will be made to calculate indices despite NA values.
  }
}

\value{Returns a data frame with the timeseries of the average layer temperature. Includes 
  \sQuote{datetime} and \sQuote{layer.temp} columns.}
  
\author{
	Luke Winslow
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{layer.temperature}
}
\examples{

	#Get the path for the package example file included
	wtr.path <- system.file('extdata', 'Sparkling.daily.wtr', package="rLakeAnalyzer")
	bathy.path <- system.file('extdata', 'Sparkling.bth', package="rLakeAnalyzer")
	
	#Load data for example lake, Sparkilng lake, in Wisconsin.
	sp.wtr = load.ts(wtr.path)
	sp.bathy = load.bathy(bathy.path)
	
	
    l.t = ts.layer.temperature(sp.wtr, 0, 18, sp.bathy)
	plot(l.t$datetime, l.t$layer.temp, type='l', 
        ylab='Volumetrically averaged lake temperature', xlab='Date')
	
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{arith}
