\name{ts.meta.depths}
\alias{ts.meta.depths}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ts.meta.depths(wtr, slope = 0.1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{wtr}{
%%     ~~Describe \code{wtr} here~~
}
  \item{slope}{
%%     ~~Describe \code{slope} here~~
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
function (wtr, slope = 0.1) 
{
    depths = get.offsets(wtr)
    n = nrow(wtr)
    wtr.mat = as.matrix(wtr[, -1])
    m.d = matrix(NA, nrow = n, ncol = 2)
    for (i in 1:n) {
        m.d[i, ] = meta.depths(wtr.mat[i, ], depths, slope)
    }
    return(data.frame(datetime = wtr$datetime, top = m.d[, 1], 
        bottom = m.d[, 2]))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
