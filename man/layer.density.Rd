\name{layer.density}
\alias{layer.density}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Returns the average density of a layer between two depths.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
layer.density(top, bottom, wtr, depths, bthA, bthD, sal)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{top}{
%%     ~~Describe \code{top} here~~
}
  \item{bottom}{
%%     ~~Describe \code{bottom} here~~
}
  \item{wtr}{
%%     ~~Describe \code{wtr} here~~
}
  \item{depths}{
%%     ~~Describe \code{depths} here~~
}
  \item{bthA}{
%%     ~~Describe \code{bthA} here~~
}
  \item{bthD}{
%%     ~~Describe \code{bthD} here~~
}
  \item{sal}{
%%     ~~Describe \code{sal} here~~
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
function (top, bottom, wtr, depths, bthA, bthD, sal) 
{
    nargin <- length(as.list(match.call())) - 1
    if (nargin < 7) {
        sal <- wtr * 0
    }
    if (top > bottom) {
        stop("bottom depth must be greater than top")
    }
    else if (length(wtr) != length(depths)) {
        stop("water temperature vector must be same length as depth vector")
    }
    else if (nargin < 4) {
        stop("not enough input arguments")
    }
    else if (any(is.na(wtr), is.na(depths), is.na(bthA), is.na(bthD))) {
        stop("input arguments must be numbers")
    }
    if (min(bthD) < 0) {
        useI <- bthD >= 0
        if (!any(bthD == 0)) {
            depT <- c(0, bthD[useI])
        }
        else {
            depT <- bthD[useI]
        }
        bthA <- approx(bthD, bthA, depT)$y
        bthD <- depT
    }
    dz <- 0.1
    numD <- length(wtr)
    if (max(bthD) > depths[numD]) {
        wtr[numD + 1] <- wtr[numD]
        sal[numD + 1] <- sal[numD]
        depths[numD + 1] <- max(bthD)
    }
    else if (max(bthD) < depths[numD]) {
        bthD <- c(bthD, depths[numD])
        bthA <- c(bthA, 0)
    }
    if (min(bthD) < depths[1]) {
        wtr <- c(wtr[1], wtr)
        sal <- c(sal[1], sal)
        depths <- c(min(bthD), depths)
    }
    Io <- grep(min(depths), depths)
    Ao <- bthA[Io]
    if (Ao[1] == 0) {
        stop("surface area cannot be zero, check bathymetry file")
    }
    layerD <- seq(top, bottom, dz)
    layerT <- approx(depths, wtr, layerD)$y
    layerS <- approx(depths, sal, layerD)$y
    layerA <- approx(bthD, bthA, layerD)$y
    layerP <- water.density(layerT, layerS)
    mass <- layerA * layerP * dz
    aveDensity <- sum(mass)/(sum(layerA))/dz
    return(aveDensity)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
