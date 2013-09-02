\name{meta.depths}
\alias{meta.depths}

\title{
Calculate the Top and Bottom Depths of the Metalimnion
}
\description{
Calculates the top and bottom depths of the metalimnion in a stratified lake. The metalimnion is defined as the water stratum in a stratified lake with the steepest thermal gradient and is demarcated by the bottom of the epilimnion and top of the hypolimnion.
}
\usage{
	meta.depths(wtr, depths, slope=0.1, seasonal=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
	  \item{wtr}{
	a numeric vector of water temperature in °C
	}
	  \item{depths}{
	a numeric vector corresponding to the depths (in m) of the wtr measurements
	}
	  \item{slope}{
	a numeric vector corresponding to the minimum slope
	}
	  \item{seasonal}{
	a logical value indicating whether the seasonal thermocline should be returned. This is fed to thermo.depth, which is used as the starting point. The seasonal thermocline is defined as the deepest density gradient found in the profile. If FALSE, the depth of the maximum density gradient is used as the starting point
	}
} %/arguments


\value{
A numeric vector of the top and bottom metalimnion depths in meters. Returns the bottom depth if no distinct metalimion top and bottom found.
} 

\references{
Wetzel, R. G. 2001. Limnology: Lake and River Ecosystems, 3rd ed. Academic Press.
%% ~put references to the literature/web site here ~
}
\author{
Jennifer Brentrup, Luke Winslow
}


\seealso{
\code{\link{schmidt.stability}}, \code{\link{lake.number}}, \code{\link{wedderburn.number}}, \code{\link{thermo.depth}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
	wtr = c(22.51, 22.42, 22.4, 22.4, 22.4, 22.36, 22.3, 22.21, 22.11, 21.23, 16.42, 
		15.15, 14.24, 13.35, 10.94, 10.43, 10.36, 9.94, 9.45, 9.1, 8.91, 8.58, 8.43)
		
	depths = c(0, 0.5, 1, 1.5, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20)

	m.d = meta.depths(wtr, depths, slope=0.1, seasonal=FALSE)
	cat('The top depth of the metalimnion is:', m.d[1])
	cat('The bottom depth of the metalimnion is:', m.d[2])

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}

