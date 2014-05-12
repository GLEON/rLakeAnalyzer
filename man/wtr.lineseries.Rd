\name{wtr.lineseries}
\alias{wtr.lineseries}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
wtr.lineseries(wtr, ylab = "Temperature C", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{wtr}{
%%     ~~Describe \code{wtr} here~~
}
  \item{ylab}{
%%     ~~Describe \code{ylab} here~~
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
function (wtr, ylab = "Temperature C", ...) 
{
    nn = ncol(wtr)
    mmax = max((wtr[, 2]), na.rm = TRUE)
    mmin = -1 + min((wtr[, nn]), na.rm = TRUE)
    starttime = min(wtr[, 1])
    endtime = max(wtr[, 1])
    colors1 = colorRampPalette(c("red", "orange", "yellow", "green3", 
        "cyan", "blue", "violet"), bias = 1, space = "rgb")
    colors = colors1(n = ncol(wtr))
    wtr.dates = wtr$datetime
    datestoshow = pretty(wtr.dates)
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
    plot(wtr[, 1], wtr[, 2], type = "l", col = colors[1], bty = "n", 
        ylim = c(mmin, mmax), xlim = c(starttime, endtime), tck = -0.03, 
        axes = F, xlab = xxlab, ylab = ylab, ...)
    for (i in 3:ncol(wtr)) {
        lines(wtr[, 1], wtr[, i], type = "l", col = colors[i])
    }
    axis(side = 1, labels = format(datestoshow, ttformat), at = datestoshow, 
        pos = c(mmin), tck = -0.03)
    abline(h = mmin, col = "black", lty = 1)
    axis(side = 2, pos = c(starttime), at = NULL, las = 1)
    abline(v = starttime, col = "black")
    legend("top", col = colors[c(1, nn)], lty = 1, bty = "n", 
        legend = c("Surface", "Bottom"), cex = 0.75, xpd = TRUE, 
        inset = c(0, -0.1), y.intersp = 0.4, horiz = TRUE)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
