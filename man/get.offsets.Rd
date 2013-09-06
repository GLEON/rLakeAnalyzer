\name{get.offsets}
\alias{get.offsets}

\title{
	Gets depths from data frame containing profile info.
}
\description{
	Extracts the depth information from a data frame containing multi-depth observation data.
	Relies on the format of the header to get information and may fail if your file format is incorrect.
}
\usage{
	get.offsets(data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
     Data frame returned from \code{\link{load.ts}}.
  }
}

\value{
	A numeric vector of depth values. Should be the \code{ncol(data) - 1} 
	in length as the first column contains date/time data.
}

\author{
Luke Winslow
}

\seealso{
	\code{\link{load.ts}}
}

\examples{
	#Get the path for the package example file included
	exampleFilePath <- system.file('extdata', 'Sparkling.wtr', package="rLakeAnalyzer")
	
	#Load
	sparkling.temp = load.ts(exampleFilePath)
	
	#get the lake depths associated with each column
	depths = get.offsets(sparkling.temp)
	
	print(depths)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}

