\name{ts.wedderburn.number}
\alias{ts.wedderburn.number}

\title{
	Calculate Wedderburn number for a timeseries.
}
\description{
Function for simplifying the calculation of Wedderburn Number. Can usually be called directly on data loaded directly using \code{\link{load.ts}} and
  \code{\link{load.bathy}}.
}


\usage{
ts.wedderburn.number(wtr, wnd, wnd.height, bathy, Ao)
}

\arguments{
  \item{wtr}{
    A data frame of water temperatures (in Celsius). Loaded using \code{\link{load.ts}}
  }
  \item{wnd}{
    A data frame of wind speeds (in m/s). Loaded using \code{\link{load.ts}}
  }
  \item{wnd.height}{
    Height of the anemometer above the lake surface in meters
  }
  \item{bathy}{
    A data frame containing hypsometric data. Loaded using \code{\link{load.bathy}}
  }
  \item{Ao}{ 
    Numeric value for the water body surface area (m^2) at zero meters depth
  }
  
}

\value{Returns a data frame with the timeseries of Wedderburn number. Includes 
  a \sQuote{datetime} column.}
  
\references{
Imberger, J., Patterson, J.C., 1990. \emph{Physical limnology}. Advances in Applied
Mechanics 27, 353-370.
}
\author{
	Luke Winslow
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{wedderburn.number},\code{ts.lake.number}
}
\examples{

	#Get the path for the package example file included
	wtr.path <- system.file('extdata', 'Sparkling.wtr', package="rLakeAnalyzer")
	wnd.path <- system.file('extdata', 'Sparkling.wnd', package="rLakeAnalyzer")
	bathy.path <- system.file('extdata', 'Sparkling.bth', package="rLakeAnalyzer")
	
	#Load data for example lake, Sparkilng lake, in Wisconsin.
	sp.wtr = load.ts(wtr.path)
	sp.wnd = load.ts(wnd.path)
	sp.bathy = load.bathy(bathy.path)
	
	sp.area = 64e4  #Area of Sparkling lake in m^2
	wnd.height = 2  #Height of Sparkling lake anemometer
	
	w.n = ts.wedderburn.number(sp.wtr, sp.wnd, wnd.height, sp.bathy, sp.area)
	plot(w.n$datetime, w.n$wedderburn.number, type='l', ylab='Wedderburn Number', xlab='Date')
	
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{arith}
