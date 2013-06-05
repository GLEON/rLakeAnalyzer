\name{lake.number}
\alias{lake.number}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculates Lake Number for a lake
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
lake.number(bthA, bthD, uStar, St, metaT, metaB, averageHypoDense)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{bthA}{
%%     ~~Describe \code{bthA} here~~
}
  \item{bthD}{
%%     ~~Describe \code{bthD} here~~
}
  \item{uStar}{
%%     ~~Describe \code{uStar} here~~
}
  \item{St}{
%%     ~~Describe \code{St} here~~
}
  \item{metaT}{
%%     ~~Describe \code{metaT} here~~
}
  \item{metaB}{
%%     ~~Describe \code{metaB} here~~
}
  \item{averageHypoDense}{
%%     ~~Describe \code{averageHypoDense} here~~
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
function (bthA, bthD, uStar, St, metaT, metaB, averageHypoDense) 
{
    g <- 9.81
    dz <- 0.1
    Ao <- bthA[1]
    Zo <- bthD[1]
    if (Ao == 0) {
        stop("Surface area cannot be zero, check *.bth file")
    }
    layerD <- seq(Zo, max(bthD), dz)
    layerA <- approx(bthD, bthA, layerD)$y
    Zv = layerD * layerA * dz
    Zcv = sum(Zv)/sum(layerA)/dz
    St_uC = St * Ao/g
    Ln = g * St_uC * (metaT + metaB)/(2 * averageHypoDense * 
        uStar^2 * Ao^(3/2) * Zcv)
    return(Ln)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
