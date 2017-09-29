#' @title Plots time series of Lake Number
#' 
#' @description Generates a time series plot of Lake Number for appropriately formatted
#' data.  See \code{\link{lake.number}} for more details on Lake Number and
#' reference.
#' 
#' 
#' @param wtr Data frame of water temperature loaded with \code{\link{load.ts}}
#' @param wnd A data frame containing hypsometric data. Loaded using
#' \code{\link{load.bathy}}
#' @param wh A value indicating the height of the anemometer above lake surface
#' in meters.  This value must be specified, there is no default.
#' @param bth A data frame containing hypsometric data. Loaded using
#' \code{\link{load.bathy}}
#' @seealso \code{\link{wtr.lineseries}}
#' @keywords hplot
#' @examples
#' 
#' 	#Get system data file paths 
#' 	wtr.path <- system.file('extdata', 'Sparkling.wtr', package="rLakeAnalyzer")
#' 	bth.path <- system.file('extdata', 'Sparkling.bth', package="rLakeAnalyzer")
#' 	wnd.path <- system.file('extdata', 'Sparkling.wnd', package="rLakeAnalyzer")
#' 
#' 	#Load data for example lake, Sparkilng Lake, Wisconsin.
#' 	wtr = load.ts(wtr.path)
#' 	wnd = load.ts(wnd.path)
#' 	bth = load.bathy(bth.path)
#' 	wh = 1 # user specified, here as 1 m.
#'   \dontrun{
#'   #generate default plot
#' 	lake.number.plot(wtr,wnd,wh,bth)
#' 	}
#' @export
lake.number.plot = function(wtr,wnd,wh,bth){
  
  ln = ts.lake.number(wtr,wnd,wh,bth)
  
  starttime = min(ln[,1]) #earliest date
  endtime = max(ln[,1]) #latest date
  
  # defining units and labels for x axis
  ln.dates = ln$datetime # turn datetime into vector
  datestoshow = pretty(ln.dates) # pretty vector to specify tick mark location 
  sec.endtime = as.numeric(endtime) # show time as seconds
  sec.starttime = as.numeric(starttime) # show time as seconds
  tt = sec.endtime - sec.starttime # time range of data frame; used to specify time axis
  
  # specify x axis format based upon time range of data 
  ttformat = c()
  if(tt < 1.1*60) { # if time is less than 1 hr units are seconds
    ttformat <- "%S"
  } else if (tt < 1.1*60*60) { # if time is less than 1.1 hours units are min:sec
    ttformat <- "%M:%S"
  } else if (tt < 60*60*24*2) {# if time is less than 2 days units are hour:min
    ttformat <- "%H:%M"
  } else if (tt < 60*60*24*7) { # if time is less than 7 days units are Jul 25 10:15
    ttformat <- "%d %b %H"
  } else if (tt < 60*60*24*7*8.9) {# if time is less than 2 months units are ex. Jul 25 10:15
    ttformat <- "%d %b %H:%M"
  } else if (tt < 60*60*24*7*4.4*12) { # if time is less than 12 months units are Jun, Jul, Aug  
    ttformat <- "%b"
  } else if (tt < 60*60*24*7*4.4*12*1.1){ # if time is more than 12.1 years units are Jul 2013
    ttformat <- "%b %Y"
  }
  
  # specify x axis labels based upon time range of data 
  xxlab = c()
  if(tt < 1.1*60) { # if time is less than 1 minutes units are seconds
    xxlab  <- "Seconds"
  } else if (tt < 1.1*60*60) { # if time is less than 1.1 hours units are min:sec
    xxlab <- "Minutes"
  } else if (tt < 60*60*24*2) {# if time is less than 2 days units are hour:min
    xxlab <- "Hours"
  } else if (tt < 60*60*24*7) { # if time is less than 7 days units are Jul 25 10:15
    xxlab <- " "
  } else if (tt < 60*60*24*7*8.9) {# if time is less than 2 months units are ex. Jul 25 
    xxlab <- " "
  } else if (tt < 60*60*24*7*4.4*12) { # if time is less than 12 months units are Jun, Jul, Aug  
    xxlab <- " "
  } else if (tt < 60*60*24*7*4.4*12*1.1){ # if time is more than 12.1 years units are Jul 2013
    xxlab <- " "
  }
  
  graphics::plot(ln[,2]~ln[,1],
       type='l',
       lwd = 2,
       col="black",
       ylab="Lake Number ",
       xlab=xxlab,
       bty="n",
       xlim=c(starttime,endtime),
       axes=F
  )
  
  # x axis
  graphics::axis(side = 1, labels=format(datestoshow, ttformat), at = datestoshow, pos = c(min(ln[,2],na.rm=TRUE)), tck = -0.03)
  graphics::segments(c(starttime),c(min(ln[,2],na.rm=TRUE)),c(endtime),c(min(ln[,2],na.rm=TRUE)), col = "black", lty = 1)
  
  
  # y axis
  graphics::axis(side  = 2, pos = c(starttime), at = NULL, las = 1)
  graphics::segments(c(starttime),c(min(ln[,2],na.rm=TRUE)),c(starttime),c(max(ln[,2],na.rm=TRUE)), col = "black")
  
}

