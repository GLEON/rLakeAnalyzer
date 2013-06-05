\name{uStar}
\alias{uStar}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculates uStar from wind speed and epilimnion density.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
uStar(wndSpeed, wndHeight, averageEpiDense)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{wndSpeed}{
%%     ~~Describe \code{wndSpeed} here~~
}
  \item{wndHeight}{
%%     ~~Describe \code{wndHeight} here~~
}
  \item{averageEpiDense}{
%%     ~~Describe \code{averageEpiDense} here~~
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
function (wndSpeed, wndHeight, averageEpiDense) 
{
    rhoAir <- 1.2
    vonK <- 0.4
    if (wndSpeed < 5) {
        Cd <- 0.001
    }
    else {
        Cd <- 0.0015
    }
    if (wndHeight != 10) {
        wndSpeed <- wndSpeed/(1 - sqrt(Cd)/vonK * log(10/wndHeight))
    }
    tau <- Cd * rhoAir * wndSpeed^2
    uStar <- sqrt(tau/averageEpiDense)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
