\name{wtr.plot.temp}
\alias{wtr.plot.temp}
\title{ Creates a time series plot of the thermocline and top and bottom of the metalimnion
%%  ~~function to do ... ~~
}
\description{ A line based plot of calculated depths of the thermocline, and top and bottom of the metalimnion from a temperature profile time series. 
}
\usage{
wtr.plot.temp(wtr, ylab = " ")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{wtr}{
  	Data frame of water temperature loaded with \code{\link{load.ts}}.
}
  \item{ylab}{ y axis label
}
}
\author{ Jennifer Brentrup, Taylor Leach
}
\seealso{ \code{\link{load.ts}} and
 \code{\link{wtr.lineseries}}}

\examples{
library(rLakeAnalyzer)
#Get system data file paths 
wtr.path <- system.file('extdata', 'Sparkling.wtr', package="rLakeAnalyzer")

#Load data for example lake, Sparkilng Lake, Wisconsin.
wtr = load.ts(wtr.path)
\dontrun{
wtr.plot.temp(wtr)
}

}
\keyword{ hplot }
