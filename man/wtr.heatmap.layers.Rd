\name{wtr.heatmap.layers}
\alias{wtr.heatmap.layers}
\title{
  Plots water temperature heatmap with major limnetic layers indicated
}
\description{ 
  This creates a heat map of water temperature similar to \code{\link{wtr.heat.map}} 
  with additional lines drawn to denote the thermocline, and the top and bottom of 
  the metalimnion as calculated using \code{\link{ts.meta.depths}} 
  and \code{\link{thermo.depth}}.
}
\usage{
wtr.heatmap.layers(wtr, ...)
}

\arguments{
  \item{wtr}{
    Data frame of water temperature loaded with \code{\link{load.ts}}.
    }
  \item{\dots}{
    Additional parameters supplied to \code{\link{filled.contour}} to modify defaults.
    Common examples include \code{zlim} and \code{plot.title}.
    }
}

\author{ 
  Jennifer Brentrup, Luke Winslow and Taylor Leach
}

\note{
  This plot cannot be used in customized multi-panel figures using\code{\link{layout}} as layout is       
  already used in the filled.contour plotting function. 
}

\seealso{
  \code{\link{wtr.heat.map}}
  \code{\link{load.ts}}
  \code{\link{ts.meta.depths}}
  \code{\link{ts.thermo.depth}}
}
\examples{
  #Get the path for the package example file included 
  wtr.path <- system.file('extdata', 'Sparkling.wtr', package="rLakeAnalyzer")

  #Load data for example lake, Sparkilng Lake, Wisconsin.
  wtr = load.ts(wtr.path)
  
  # generate default plot
  \dontrun{
  wtr.heatmap.layers(wtr)
  }
}
\keyword{ hplot }
