\name{ts.meta.depths}
\alias{ts.meta.depths}
\alias{ts.thermo.depth}
\alias{ts.schmidt.stability}
\alias{ts.lake.number}

\title{
  Calculate physical indices for a timeseries.
}
\description{
  Functions for simplifying the calculation of physical indices for a timeseries of observation data. 
  Can usually be called directly on data loaded directly using \code{\link{load.ts}} and
  \code{\link{load.bathy}}.
}
\usage{
  ts.meta.depths(wtr, slope = 0.1)

  ts.thermo.depth(wtr, Smin = 0.1, seasonal=TRUE)

  ts.schmidt.stability(wtr, bathy)

  ts.lake.number(wtr, wnd, wnd.height, bathy)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{wtr}{
    A data frame of water temperatures (in Celsius). Loaded using \code{\link{load.ts}}
  }
  \item{slope}{
    The minimum density gradient (kg/m^3/m) that can be called the thermocline
  }
  \item{Smin}{
    The minimum density gradient cutoff (kg/m^3/m) defining the metalimion
  }
  \item{bathy}{
    A data frame containing hypsometric data. Loaded using \code{\link{load.bathy}}
  }
  \item{wnd}{
    A data frame of wind speeds (in m/s). Loaded using \code{\link{load.ts}}
  }
  \item{wnd.height}{
    Height of the anemometer above the lake surface in meters
  }
  \item{seasonal}{
    Boolean indicating if seasonal thermocline should be used in calculation.
  }
}
\details{
  These are wrapper functions that accept a timeseries of data and call the core
  physical metric functions (like \code{\link{schmidt.stability}}) on each timestep.
  
}
\value{
  Returns a data frame with the timeseries of calculated derivatives. All include 
  a \sQuote{datetime} column, but derivative columns differ between functions.
}

\author{
  Luke Winslow
}

\seealso{
  For loading input data \code{\link{load.ts}}, \code{\link{load.bathy}}.
  
  For the underlying functions operating at each timestep \code{\link{meta.depths}}, 
  \code{\link{thermo.depth}}, \code{\link{schmidt.stability}}, \code{\link{lake.number}}.
}

\examples{
	#Get the path for the package example file included
	exampleFilePath <- system.file('extdata', 'Sparkling.wtr', package="rLakeAnalyzer")
	
	#Load
	sparkling.temp = load.ts(exampleFilePath)
	
	#calculate and plot the metalimnion depths
	m.d = ts.meta.depths(sparkling.temp)
	
	plot(m.d$datetime, m.d$top, type='l', ylab='Meta Depths (m)', xlab='Date', col='blue')
	lines(m.d$datetime, m.d$bottom, col='red')
  
  
  #calculate and plot the thermocline depth
	t.d = ts.thermo.depth(sparkling.temp)
	
	plot(t.d$datetime, t.d$thermo.depth, type='l', ylab='Thermocline Depth (m)', xlab='Date')
  
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}

