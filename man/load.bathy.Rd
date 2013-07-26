\name{load.bathy}
\alias{load.bathy}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
load.bathy(fPath)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fPath}{
%%     ~~Describe \code{fPath} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
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
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

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
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
