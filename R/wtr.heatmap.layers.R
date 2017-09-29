#' @title Plots water temperature heatmap with major limnetic layers indicated
#' 
#' @description This creates a heat map of water temperature similar to
#' \code{\link{wtr.heat.map}} with additional lines drawn to denote the
#' thermocline, and the top and bottom of the metalimnion as calculated using
#' \code{\link{ts.meta.depths}} and \code{\link{thermo.depth}}.
#' 
#' 
#' @param wtr Data frame of water temperature loaded with
#' \code{\link{load.ts}}.
#' @param \dots Additional parameters supplied to \code{\link{filled.contour}}
#' to modify defaults.  Common examples include \code{zlim} and
#' \code{plot.title}.
#' @note This plot cannot be used in customized multi-panel figures
#' using\code{\link{layout}} as layout is already used in the filled.contour
#' plotting function.
#' @seealso \code{\link{wtr.heat.map}} \code{\link{load.ts}}
#' \code{\link{ts.meta.depths}} \code{\link{ts.thermo.depth}}
#' @keywords hplot
#' @examples
#' 
#'   #Get the path for the package example file included 
#'   wtr.path <- system.file('extdata', 'Sparkling.wtr', package="rLakeAnalyzer")
#' 
#'   #Load data for example lake, Sparkilng Lake, Wisconsin.
#'   wtr = load.ts(wtr.path)
#'   
#'   # generate default plot
#'   \dontrun{
#'   wtr.heatmap.layers(wtr)
#'   }
#' @export
wtr.heatmap.layers <- function(wtr, ...){
  
  td = ts.thermo.depth(wtr)
  md = ts.meta.depths(wtr)
  
  starttime = min(wtr[,1]) #earliest date
  endtime = max(wtr[,1]) #latest date
  sec.endtime = as.numeric(endtime) # show time as seconds
  sec.starttime = as.numeric(starttime) # show time as seconds
  tt = sec.endtime - sec.starttime
  
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
  
  df = list(wtr,td,md) ##Create list of data frame to join
  wtr.all = plyr::join_all(df, by="datetime") ##Joins thermodepths, metadepths with temp data
  nn = ncol(wtr.all) -3
  depths = get.offsets(wtr.all[,2:nn])
  
  #n = nrow(wtr.all)
  
  wtr.dates = wtr.all$datetime
  datestoshow = pretty(wtr.dates)
  wtr.mat = as.matrix(wtr.all[,2:nn]) ##Makes matrix of only temp data even though
  ##meta depths and thermo depths in dataframe too
  
  y = depths
  
  graphics::filled.contour(wtr.dates
                 , y
                 , wtr.mat
                 , ylim=c(max(depths),0)
                 , zlim=c(min(wtr.mat,na.rm=TRUE) , max(wtr.mat,na.rm=TRUE))
                 , nlevels=100
                 , color.palette = grDevices::colorRampPalette(c("violet","blue","cyan", "green3", "yellow", "orange", "red")
                                                  , bias = 1
                                                  , space = "rgb")
                 , ylab="Depth (m)"
                 , key.title=graphics::title((main="Temperature (\u00B0C)")
                                   ,adj=0.2, cex.main=1)
                 ,plot.axes = {graphics::lines(x=wtr.dates,y=wtr.all$thermo.depth,col="black",lwd = 2)
                               graphics::lines(x=wtr.dates,y=wtr.all$top, col="gray50", lwd = 2)
                               graphics::lines(x=wtr.dates,y=wtr.all$bottom,col="gray80", lwd = 2)
                               graphics::axis(side = 2)
                               graphics::axis(side = 3, labels=format(datestoshow, ttformat), at = datestoshow, pos = c(min(depths)), tck = -0.03)})
  
  
  
}
