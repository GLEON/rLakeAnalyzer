\name{load.bathy}
\alias{load.bathy}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Import lake bathmynetry data.
}
\description{
  Imports lake bathymetry data. Bathymetric data file must be a 2 column array where depth and area information are provided in column 1 and 2 respectively.
}
\usage{
  load.bathy(fPath)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fPath}{
    User provides the file path (fPath) where the bathymetric data file is stored
  }
}
\details{
  %%  ~~ If necessary, more details than the description above ~~
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
\note{
  %%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~
  
  \seealso{
    load.ts
    %% ~~objects to See Also as \code{\link{help}}, ~~~
  }
\examples{
  fPath <- "Sparkling.bth"
  load.bathy(fPath)
  
  ## The function is currently defined as
  function (fPath) 
  {
    d = read.table(fPath, sep = c(","), header = TRUE)
    if (ncol(d) < 2) {
      d = read.table(fPath, sep = c("\t"), header = TRUE)
    }
    if (ncol(d) < 2) {
      stop("Error loading bathymetry file, check that it is a tab delimited file two columns.")
    }
    headNames = tolower(names(d))
    dI = grep("depths", headNames)
    aI = grep("areas", headNames)
    if (length(aI) < 1 || length(dI) < 1) {
      stop("Bathymetry header must be labeled with \"areas\" and \"depths\".")
    }
    names(d)[dI] = "depths"
    names(d)[aI] = "areas"
    return(d)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{limnology}
\keyword{bathymetry}% __ONLY ONE__ keyword per line
