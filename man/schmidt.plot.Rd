\name{schmidt.plot}
\alias{schmidt.plot}
\title{ Creates a time series plot of Schmidt's stability
}
\description{ Generates a time series of Schmidt's stability where each value represents water column stability for each time step of data. See \code{\link{schmidt.stability}} for more details and reference. 
}
\usage{
schmidt.plot(wtr, bth)
}
\arguments{
	\item{wtr}{
		Data frame of water temperature loaded with \code{\link{load.ts}}
	}
	\item{bth}{
		A data frame containing hypsometric data. Loaded using \code{\link{load.bathy}}
	}
}

\references{
	See \code{\link{schmidt.stability}}
}

\author{ 
	Taylor Leach, Jennifer Brentrup
}
\seealso{
	\code{\link{schmidt.stability}}
}
\examples{
	# Get system data file paths 
  wtr.path <- system.file('extdata', 'Sparkling.wtr', package="rLakeAnalyzer")
	bth.path <- system.file('extdata', 'Sparkling.bth', package="rLakeAnalyzer")

	# Load data for example lake, Sparkilng Lake, Wisconsin.
	wtr = load.ts(wtr.path)
	bth = load.bathy(bth.path)
	
	\dontrun{
  # Generate default plot
  schmidt.plot(wtr,bth)
  }
}
\keyword{ hplot }
