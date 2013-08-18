\name{load.ts}
\alias{load.ts}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Load time series data
}
\description{
  Loads tab delimited time series data file into R session. DateTime information must be provided in first column with the corresponding data in the remaining columns. 
}
\usage{
  load.ts(fPath)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fPath}{
    User provides the file path (fPath) where the data file is stored
  }
}
\details{
 
}
\value{
  data.frame of DateTime and corresponding data for given lake. 
}
\references{
  %% ~put references to the literature/web site here ~
}
\author{
  %%  ~~who you are~~
}
\note{
  %%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~
  
  \seealso{
    %% ~~objects to See Also as \code{\link{help}}, ~~~
  }
\examples{
  
  fPath <- "Sparkling.wtr"
  load.bathy(fPath)
  
  ## The function is currently defined as
  function (fPath) 
  {
    d = read.table(fPath, sep = "\t", header = TRUE)
    names(d) = tolower(names(d))
    d$datetime = as.POSIXct(d$datetime)
    return(d)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{time_series}
