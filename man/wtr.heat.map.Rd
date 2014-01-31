\name{wtr.heat.map}
\alias{wtr.heat.map}

\title{
	Plots a heat-map of water temperature.
}
\description{
	This creates a simple, default heatmap of water temperature.
}
\usage{
	wtr.heat.map(wtr, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{wtr}{
     Data frame of water temperature loaded with \code{\link{load.ts}}.
  }
  \item{...}{
	Additional parameters supplied to \code{\link{filled.contour}} to modify defaults. Common examples
	include \code{zlim} and \code{plot.title}.
  }
}

\author{
	Jennifer Brentrup, Luke Winslow
}

\seealso{
	\code{\link{load.ts}}
}

\examples{
	#Get the path for the package example file included
	wtr.path <- system.file('extdata', 'Sparkling.wtr', package="rLakeAnalyzer")
	
	#Load data for example lake, Sparkilng Lake, Wisconsin.
	sp.wtr = load.ts(wtr.path)
	
	#Plot default figure
	wtr.heat.map(sp.wtr)

	#Change defaults supplied to filled.contour
	wtr.heat.map(sp.wtr, zlim=c(0,15), plot.title="Sparkling Water Temp (C)")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{hplot}

