\name{wedderburn.number}
\alias{wedderburn.number}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculates Wedderburn Number for a lake.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
wedderburn.number(delta_rho, metaT, uSt, Ao, AvHyp_rho)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{delta_rho}{
%%     ~~Describe \code{delta_rho} here~~
}
  \item{metaT}{
%%     ~~Describe \code{metaT} here~~
}
  \item{uSt}{
%%     ~~Describe \code{uSt} here~~
}
  \item{Ao}{
%%     ~~Describe \code{Ao} here~~
}
  \item{AvHyp_rho}{
%%     ~~Describe \code{AvHyp_rho} here~~
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
function (delta_rho, metaT, uSt, Ao, AvHyp_rho) 
{
    g = 9.81
    Lo = 2 * sqrt(Ao/pi)
    go = g * delta_rho/AvHyp_rho
    W = go * metaT^2/(uSt^2 * Lo)
    return(W)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
