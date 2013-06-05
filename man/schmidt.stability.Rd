\name{schmidt.stability}
\alias{schmidt.stability}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculates the schmidt stability of a lake.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
schmidt.stability(wtr, depths, bthA, bthD, sal = wtr * 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
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
function (wtr, depths, bthA, bthD, sal = wtr * 0) 
{
    if (length(wtr) != length(depths)) {
        stop("water temperature array must be the same length as the depth array")
    }
    g = 9.81
    dz = 0.1
    if (min(bthD) < 0) {
        useI = bthD >= 0
        if (any(bthD == 0)) {
            depT = bthD[useI]
        }
        else {
            depT = c(0, bathD[useI])
        }
        bthA = approx(bthD, bthA, depT)$y
        bthD = depT
    }
    numD = length(wtr)
    if (max(bthD) > depths[numD]) {
        wtr[numD + 1] = wtr[numD]
        sal[numD + 1] = sal[numD + 1]
        depths[numD + 1] = max(bthD)
    }
    else if (max(bthD) < depths[numD]) {
        bthD = c(bthD, depths[numD])
        bthA = c(bthA, 0)
    }
    if (min(bthD) < depths[1]) {
        wtr = c(wtr[1], wtr)
        sal = c(sal[1], sal)
        depths = c(min(bthD), depths)
    }
    Zo = min(depths)
    Io = which.min(depths)
    Ao = bthA[Io]
    if (Ao == 0) {
        stop("Surface area cannot be zero, check *.bth file")
    }
    rhoL = water.density(wtr, sal)
    layerD = seq(min(depths), max(depths), by = dz)
    layerP = approx(depths, rhoL, layerD)$y
    layerA = approx(bthD, bthA, layerD)$y
    Zv = layerD * layerA * dz
    Zcv = sum(Zv)/sum(layerA)/dz
    numInt = length(layerA)
    st = layerA * NaN
    for (i in 1:numInt) {
        z = layerD[i]
        A = layerA[i]
        st[i] = -(Zcv - z) * layerP[i] * A * dz
    }
    St = g/Ao * sum(st)
    return(St)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
