# time series of lake number
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
  
  plot(ln[,2]~ln[,1],
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
  axis(side = 1, labels=format(datestoshow, ttformat), at = datestoshow, pos = c(min(ln[,2],na.rm=TRUE)), tck = -0.03)
  segments(c(starttime),c(min(ln[,2],na.rm=TRUE)),c(endtime),c(min(ln[,2],na.rm=TRUE)), col = "black", lty = 1)
  
  
  # y axis
  axis (side  = 2, pos = c(starttime), at = NULL, las = 1)
  segments(c(starttime),c(min(ln[,2],na.rm=TRUE)),c(starttime),c(max(ln[,2],na.rm=TRUE)), col = "black")
  
}

