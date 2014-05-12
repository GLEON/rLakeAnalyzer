\name{wtr.lineseries}
\alias{wtr.lineseries}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
 Creates a line based plot of temperature profile time series
}
\description{
A non-heat map approach to visualizing a water temperature profile useful for 
identify temperature trends over time at discrete depths and diagnosing issues with data.
}
\usage{
wtr.lineseries(wtr, ylab = "Temperature C", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{wtr}{
Data frame of water temperature loaded with \code{\link{load.ts}}.
}
  \item{ylab}{
y axis title 
}
  \item{\dots}{
Additional parameters supplied to the plot function
}
}

\author{
Taylor Leach, Luke Winslow
}

\seealso{ See
\code{\link{load.ts}} and 
\code{\link{wtr.heat.map}}
}
\examples{
exampleFilePath <- system.file('extdata', 'Sparkling.wtr', package="rLakeAnalyzer")
wtr= load.ts(exampleFilePath)
wtr.lineseries(wtr, ylab = "Temperature C")

}

\keyword{ hplot }
