\name{wedderburn.number}
\alias{wedderburn.number}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculates Wedderburn Number for a lake.
}
\description{ Wedderburn Number is a dimensionless parameter measuring the effect of wind stress 
on the lake surface. }
\usage{
wedderburn.number(delta_rho, metaT, uSt, Ao, AvHyp_rho)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{delta_rho}{ Numeric value for the water density difference between the epilimnion and hypolimnion (kg/m^3)}
  \item{metaT}{ Numeric value for the thickness of the water body's surface layer (m)}
  \item{uSt}{ Numeric value for the water friction velocity due to wind stress (m/s)}
  \item{Ao}{ Numeric value for the water body surface area (m^2) at zero meters depth}
  \item{AvHyp_rho}{ Numeric value for the average water density of the hypolimnion layer (kg/m^3)}
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
\author{ Jake Zwart}
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

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
