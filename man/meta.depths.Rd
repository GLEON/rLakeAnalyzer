\name{meta.depths}
\alias{meta.depths}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculates the top and bottom depth of the metalimnion.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
meta.depths(wtr, thermoD, depths, slope)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{wtr}{
%%     ~~Describe \code{wtr} here~~
}
  \item{thermoD}{
%%     ~~Describe \code{thermoD} here~~
}
  \item{depths}{
%%     ~~Describe \code{depths} here~~
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
function (wtr, thermoD, depths, slope) 
{
    rhoVar = water.density(wtr)
    dRhoPerc = 0.15
    numDepths = length(depths)
    drho_dz = vector(mode = "double", length = numDepths - 1)
    for (i in 1:numDepths - 1) {
        drho_dz[i] = (rhoVar[i + 1] - rhoVar[i])/(depths[i + 
            1] - depths[i])
    }
    metaBot_depth = depths[numDepths]
    Tdepth = vector(mode = "double", length = numDepths - 1) * 
        NaN
    for (i in 1:numDepths - 1) {
        Tdepth[i] = mean(depths[i:(i + 1)])
    }
    tmp = sort.int(c(Tdepth, thermoD + 1e-06), index.return = TRUE)
    sortDepth = tmp$x
    sortInd = tmp$ix
    drho_dz = approx(Tdepth, drho_dz, sortDepth)
    drho_dz = drho_dz$y
    thermo_index = 1
    thermoId = numDepths
    for (i in 1:numDepths) {
        if (thermoId == sortInd[i]) {
            thermo_index = i
            break
        }
    }
    for (i in thermo_index:numDepths) {
        if (drho_dz[i] < slope) {
            metaBot_depth = sortDepth[i]
            break
        }
    }
    if (i - thermo_index > 1 && drho_dz[thermo_index] > slope) {
        metaBot_depth = approx(drho_dz[thermo_index:i], sortDepth[thermo_index:i], 
            slope)
        metaBot_depth = metaBot_depth$y
    }
    if (is.na(metaBot_depth)) {
        metaBot_depth = depths[numDepths]
    }
    for (i in seq(thermo_index, 1)) {
        if (drho_dz[i] < slope) {
            metaTop_depth = sortDepth[i]
            break
        }
    }
    if (thermo_index - i > 1 && drho_dz[thermo_index] > slope) {
        metaTop_depth = approx(drho_dz[i:thermo_index], sortDepth[i:thermo_index], 
            slope)
        metaTop_depth = metaTop_depth$y
    }
    list(botDepth = metaBot_depth, topDepth = metaTop_depth)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
