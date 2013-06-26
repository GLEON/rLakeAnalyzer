\name{thermo.depth}
\alias{thermo.depth}

\title{
Returns the depth of the thermocline from a temperature profile.
}
\description{
This function calculates the location of the thermocline from a temperature profile. 
It uses a special technique to estimate where the thermocline lies even between two temperature measurement depths, 
giving a potentially finer-scale estimate than usual techniques.
}
\usage{
thermo.depth(wtr, depths, Smin = 0.1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{wtr}{
Numeric vector of water temperature in °C
}
  \item{depths}{
Numeric vector of depths. Must be the same length as the wtr parameter
}
  \item{Smin}{
Optional paramter defining minimum density gradient for thermocline
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Depth of thermocline. If no thermocline found, value is max(depths).
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
\code{water.density}
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (wtr, depths, Smin = 0.1) 
{
    rhoVar = water.density(wtr)
    dRhoPerc = 0.15
    numDepths = length(depths)
    drho_dz = vector(mode = "double", length = numDepths - 1)
    for (i in 1:numDepths - 1) {
        drho_dz[i] = (rhoVar[i + 1] - rhoVar[i])/(depths[i + 
            1] - depths[i])
    }
    thermoInd = which.max(drho_dz)
    mDrhoZ = drho_dz[thermoInd]
    thermoD = mean(depths[thermoInd:(thermoInd + 1)])
    if (thermoInd > 1 && thermoInd < numDepths - 1) {
        Sdn = -(depths[thermoInd + 1] - depths[thermoInd])/(drho_dz[thermoInd + 
            1] - drho_dz[thermoInd])
        Sup = (depths[thermoInd] - depths[thermoInd - 1])/(drho_dz[thermoInd] - 
            drho_dz[thermoInd - 1])
        upD = depths[thermoInd]
        dnD = depths[thermoInd + 1]
        if (!is.infinite(Sup) & !is.infinite(Sdn)) {
            thermoD = dnD * (Sdn/(Sdn + Sup)) + upD * (Sup/(Sdn + 
                Sup))
        }
    }
    dRhoCut = max(c(dRhoPerc * mDrhoZ, Smin))
    locs = findPeaks(drho_dz, dRhoCut)
    pks = drho_dz[locs]
    if (length(pks) == 0) {
        SthermoD = thermoD
        SthermoInd = thermoInd
    }
    else {
        mDrhoZ = pks[length(pks)]
        SthermoInd = locs[length(pks)]
        if (SthermoInd > thermoInd + 1) {
            SthermoD = mean(depths[SthermoInd:(SthermoInd + 1)])
            if (SthermoInd > 1 && SthermoInd < numDepths - 1) {
                Sdn = -(depths[SthermoInd + 1] - depths[SthermoInd])/(drho_dz[SthermoInd + 
                  1] - drho_dz[SthermoInd])
                Sup = (depths[SthermoInd] - depths[SthermoInd - 
                  1])/(drho_dz[SthermoInd] - drho_dz[SthermoInd - 
                  1])
                upD = depths[SthermoInd]
                dnD = depths[SthermoInd + 1]
                if (!is.infinite(Sup) & !is.infinite(Sdn)) {
                  SthermoD = dnD * (Sdn/(Sdn + Sup)) + upD * 
                    (Sup/(Sdn + Sup))
                }
            }
        }
        else {
            SthermoD = thermoD
            SthermoInd = thermoInd
        }
    }
    if (SthermoD < thermoD) {
        SthermoD = thermoD
        SthermoInd = thermoInd
    }
    list(thermoD, thermoInd, drho_dz, SthermoD, SthermoInd)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
