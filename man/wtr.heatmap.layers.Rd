\name{wtr.heatmap.layers}
\alias{wtr.heatmap.layers}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plots water temp heatmap with major layers highlighted
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
wtr.heatmap.layers(wtr, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{wtr}{
%%     ~~Describe \code{wtr} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
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
function (wtr, ...) 
{
    td = ts.thermo.depth(wtr)
    md = ts.meta.depths(wtr)
    starttime = min(wtr[, 1])
    endtime = max(wtr[, 1])
    sec.endtime = as.numeric(endtime)
    sec.starttime = as.numeric(starttime)
    tt = sec.endtime - sec.starttime
    ttformat = c()
    if (tt < 1.1 * 60) {
        ttformat <- "\%S"
    }
    else if (tt < 1.1 * 60 * 60) {
        ttformat <- "\%M:\%S"
    }
    else if (tt < 60 * 60 * 24 * 2) {
        ttformat <- "\%H:\%M"
    }
    else if (tt < 60 * 60 * 24 * 7) {
        ttformat <- "\%d \%b \%H"
    }
    else if (tt < 60 * 60 * 24 * 7 * 8.9) {
        ttformat <- "\%d \%b \%H:\%M"
    }
    else if (tt < 60 * 60 * 24 * 7 * 4.4 * 12) {
        ttformat <- "\%b"
    }
    else if (tt < 60 * 60 * 24 * 7 * 4.4 * 12 * 1.1) {
        ttformat <- "\%b \%Y"
    }
    xxlab = c()
    if (tt < 1.1 * 60) {
        xxlab <- "Seconds"
    }
    else if (tt < 1.1 * 60 * 60) {
        xxlab <- "Minutes"
    }
    else if (tt < 60 * 60 * 24 * 2) {
        xxlab <- "Hours"
    }
    else if (tt < 60 * 60 * 24 * 7) {
        xxlab <- " "
    }
    else if (tt < 60 * 60 * 24 * 7 * 8.9) {
        xxlab <- " "
    }
    else if (tt < 60 * 60 * 24 * 7 * 4.4 * 12) {
        xxlab <- " "
    }
    else if (tt < 60 * 60 * 24 * 7 * 4.4 * 12 * 1.1) {
        xxlab <- " "
    }
    library(plyr)
    df = list(wtr, td, md)
    wtr.all = join_all(df, by = "datetime")
    nn = ncol(wtr.all) - 3
    depths = get.offsets(wtr.all[, 2:nn])
    wtr.dates = wtr.all$datetime
    datestoshow = pretty(wtr.dates)
    wtr.mat = as.matrix(wtr.all[, 2:nn])
    y = depths
    filled.contour(wtr.dates, y, wtr.mat, ylim = c(max(depths), 
        0), zlim = c(min(wtr.mat, na.rm = TRUE), max(wtr.mat, 
        na.rm = TRUE)), nlevels = 100, color.palette = colorRampPalette(c("violet", 
        "blue", "cyan", "green3", "yellow", "orange", "red"), 
        bias = 1, space = "rgb"), ylab = "Depth (m)", key.title = title((main = "Temperature (°C)"), 
        adj = 0.2, cex.main = 1), plot.axes = {
        lines(x = wtr.dates, y = wtr.all$thermo.depth, col = "black", 
            lwd = 2)
        lines(x = wtr.dates, y = wtr.all$top, col = "gray50", 
            lwd = 2)
        lines(x = wtr.dates, y = wtr.all$bottom, col = "gray80", 
            lwd = 2)
        axis(side = 2)
        axis(side = 3, labels = format(datestoshow, ttformat), 
            at = datestoshow, pos = c(min(depths)), tck = -0.03)
    })
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
