wtr.lineseries = function(wtr, ylab = "Temperature C",...){
  
  nn = ncol(wtr) # number of columns in data set
  mmax = max((wtr[,2]), na.rm = TRUE) # max temp in data set
  mmin = -1 + min((wtr[,nn]), na.rm = TRUE) #  min temp in data set plus a 1 degree buffer
  starttime = min(wtr[,1]) #earliest date
  endtime = max(wtr[,1]) #latest date
  
  colors1 = colorRampPalette(c("red"
                               ,"orange"
                               ,"yellow"
                               ,"green3"
                               ,"cyan"
                               ,"blue"
                               ,"violet")
                             , bias = 1
                             , space = "rgb")
  colors = colors1(n = ncol(wtr))                           
  
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
  } else if (tt < 60*60*24*365) { # if time is less than 12 months units are Jun, Jul, Aug  
  	ttformat <- "%b"
  } else if (tt > 60*60*24*365.25){ # if time is more than 12 months units are Jul 2013
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
  } else if (tt < 60*60*24*365) { # if time is less than 12 months units are Jun, Jul, Aug
  	xxlab <- " "
  } else if (tt > 60*60*24*365.25){ # if time is more than 12 months units are Jul 2013
  	xxlab <- " "
  }
  #tiff('wtf2.tiff',width=1600, height=900, res=300, compression='lzw')
  #plot temp over time; each depth as a unique line
  plot(wtr[,1], 
       wtr[,2] 
       ,type='l' 
       ,col=colors[1]
       ,bty = "n"
       ,ylim = c(mmin,mmax)
       ,xlim = c(starttime, endtime)
       ,tck = -0.03 
       ,axes = F #supress axes
       ,xlab = xxlab
       ,ylab = ylab #"Temperature C"
       , ...
      )
  for( i in 3:ncol(wtr)){
    lines(wtr[,1], wtr[,i], type='l', col=colors[i])
  }
  
  # x axis
  axis(side = 1, labels=format(datestoshow, ttformat), at = datestoshow, pos = c(mmin), tck = -0.03)
  abline(h = mmin, col = "black", lty = 1)
  
  # y axis
  axis (side  = 2, pos = c(starttime), at = NULL, las = 1)
  abline(v = starttime, col = "black")
  
  # generate and place legend
  legend( "top"
          , col = colors[c(1, nn)]
          , lty = 1, bty = "n"
          , legend = c("Surface", "Bottom")
          , cex = 0.75
          , xpd = TRUE
          , inset=c(0,-0.1)
          , y.intersp = 0.4
          , horiz=TRUE)
  
}
