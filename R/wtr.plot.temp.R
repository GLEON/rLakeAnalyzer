# time series plot of thermocline, metalimnion top and bottom depths
wtr.plot.temp = function(wtr, ylab = " ", seasonal=TRUE){
  
  depths = get.offsets(wtr[,-1])
  td = ts.thermo.depth(wtr, seasonal=seasonal)
  md = ts.meta.depths(wtr, seasonal=seasonal)
  
  df = list(wtr,td,md) ##Create list of data frame to join
  wtr.all = join_all(df, by="datetime") ##Joins thermodepths, metadepths with temp data
  
  nn = ncol(wtr) # number of columns in data set
  starttime = min(wtr[,1]) #earliest date
  endtime = max(wtr[,1]) #latest date
  
  # defining units and labels for x axis
  wtr.dates = wtr$datetime # turn datetime into vector
  datestoshow = pretty(wtr.dates) # pretty vector to specify tick mark location 
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
  } else if (tt <= 60*60*24*365) { # if time is less than 12 months units are Jun, Jul, Aug  
    ttformat <- "%b"
  } else if (tt > 60*60*24*365){ # if time is more than 12 months units are Jul 2013
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
  } else if (tt <= 60*60*24*365) { # if time is less than 12 months units are Jun, Jul, Aug
    xxlab <- " "
  } else if (tt > 60*60*24*365){ # if time is more than 12 months units are Jul 2013
    xxlab <- " "
  }
  
  plot(wtr.all[,nn+1]~wtr.all[,1],
       type='l',
       col="black",
       ylab="Depth (m)",
       xlab=xxlab,
       bty="n",
       ylim=c(max(depths),c(min(depths)-1)),
       xlim=c(starttime,endtime),
       axes=F
  )
  lines(wtr.all[,nn+2]~wtr.all[,1], type='l',col="orangered")
  lines(wtr.all[,nn+3]~wtr.all[,1], type='l',col="navy")
  
  # x axis
  axis(side = 3, labels=format(datestoshow, ttformat), at = datestoshow, pos = c(min(depths)), tck = -0.03)
  segments(c(starttime),c(min(depths)),c(endtime),c(min(depths)), col = "black", lty = 1)
  
  # y axis
  axis (side  = 2, pos = c(starttime), at = NULL, las = 1)
  segments(c(starttime),c(min(depths)),c(starttime),c(max(depths)), col = "black")
  
  # generate and place legend
  legend( "bottom"
          , col = c("orangered", "black","navy")
          , lty = 1, lwd = 3, bty = "n"
          , legend = c("Metalimnion Top (m)","Thermocline Depth (m)", "Metalimnion Bottom (m)")
          , cex = 0.75
          , xpd = TRUE
          , #inset=c(0,-0.1)
          , x.intersp = -1.5
          , y.intersp = -1.5
          , adj=c(-0.4,0.5)
          , horiz = TRUE
  )
}
