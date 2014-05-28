\name{lake.number.plot}
\alias{lake.number.plot}

\title{ 
	Plots time series of Lake Number
}
\description{ 
	Generates a time series plot of lake number for appropriately formatted data
}
\usage{
	lake.number.plot(wtr, wnd, wh, bth)
}
\arguments{
	\item{wtr}{
		Data frame of water temperature loaded with \code{\link{load.ts}}
	}
	\item{wnd}{
		A data frame containing hypsometric data. Loaded using \code{\link{load.bathy}}
	}
	\item{wh}{
		A value indicating the height of the anemometer above lake surface 
	}
	\item{bth}{
		A data frame containing hypsometric data. Loaded using \code{\link{load.bathy}}
	}
}
\author{ 
	Jennifer Brentrup, Taylor Leach
}
\seealso{ 
	\code{\link{wtr.lineseries}}
}
\examples{
	#Get system data file paths 
	wtr.path <- system.file('extdata', 'Sparkling.wtr', package="rLakeAnalyzer")
	bth.path <- system.file('extdata', 'Sparkling.bth', package="rLakeAnalyzer")
	wnd.path <- system.file('extdata', 'Sparkling.wnd', package="rLakeAnalyzer")

	#Load data for example lake, Sparkilng Lake, Wisconsin.
	wtr = load.ts(wtr.path)
	wnd = load.ts(wnd.path)
	bth = load.bathy(bth.path)
	wh = 1 ##Wind height for height of anemometer above lake surface in meters user specified, here as 1 m.

	lake.number.plot(wtr,wnd,wh,bth)
}

\keyword{ hplot }
