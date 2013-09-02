\name{load.bathy}
\alias{load.bathy}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Import lake bathmynetry data.
}
\description{
  Imports lake bathymetry data. Bathymetric data file must be a 2 column array 
  where depth (in meters) and area (in meters^2) information are provided in columns 
  with headers containing the words "depth" and "area" respectively.
}
\usage{
  load.bathy(fPath)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fPath}{
    File path to the bathymetry file.
  }
}
\details{
  
}
\value{
  data.frame of depth and area for given lake.
}
\references{
  %% ~put references to the literature/web site here ~
}
\author{
  Luke Winslow
}

%% ~Make other sections like Warning with \section{Warning }{....} ~
  
  \seealso{
    \code{\link{load.ts}}
    
  }
\examples{
  #Get the path for the package example file included
  exampleFilePath <- system.file('extdata', 'Sparkling.bth', package="rLakeAnalyzer")
  
  #Load and plot the hypsometric curve
  sparkling.bathy = load.bathy(exampleFilePath)
  
  #If successful, there will be two colums. "depths", and "areas".
  plot(sparkling.bathy$areas, sparkling.bathy$depths, type='l', ylim=c(20,0), ylab='Depths (m)', xlab='Areas (m^2)')
  
}
  
\keyword{file}

