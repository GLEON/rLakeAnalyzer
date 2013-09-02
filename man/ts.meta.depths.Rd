\name{ts.meta.depths}
\alias{ts.meta.depths}

\title{
Calculate physical indices for a full timeseries.
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
\code{\link{load.ts}}, \code{\link{ts.thermo.depth}}.
}
\examples{
	#Get the path for the package example file included
	exampleFilePath <- system.file('extdata', 'Sparkling.wtr', package="rLakeAnalyzer")
	
	#Load
	sparkling.temp = load.ts(exampleFilePath)
	
	#calculate and plot the metalimnion depths
	m.d = ts.meta.depths(sparkling.temp)
	
	plot(m.d$datetime, m.d$top, type='l', ylab='Meta Depths (m)', xlab='Date', col='blue')
	lines(m.d$datetime, m.d$bottom, col='red')
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}

