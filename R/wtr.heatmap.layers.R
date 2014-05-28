#heat map with lines for thermocline, meta top and bottom depths
wtr.heatmap.layers <- function(wtr, ...){
  
  library(plyr)
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
  wtr.all = join_all(df, by="datetime") ##Joins thermodepths, metadepths with temp data
  nn = ncol(wtr.all) -3
  depths = get.offsets(wtr.all[,2:nn])
  
  #n = nrow(wtr.all)
  
  wtr.dates = wtr.all$datetime
  datestoshow = pretty(wtr.dates)
  wtr.mat = as.matrix(wtr.all[,2:nn]) ##Makes matrix of only temp data even though
  ##meta depths and thermo depths in dataframe too
  
  y = depths
  
  filled.contour(wtr.dates
                 , y
                 , wtr.mat
                 , ylim=c(max(depths),0)
                 , zlim=c(min(wtr.mat,na.rm=TRUE) , max(wtr.mat,na.rm=TRUE))
                 , nlevels=100
                 , color.palette=colorRampPalette(c("violet","blue","cyan", "green3", "yellow", "orange", "red")
                                                  , bias = 1
                                                  , space = "rgb")
                 , ylab="Depth (m)"
                 , key.title=title((main="Temperature (\u00B0C)")
                                   ,adj=0.2, cex.main=1)
                 ,plot.axes = {lines(x=wtr.dates,y=wtr.all$thermo.depth,col="black",lwd = 2)
                               lines(x=wtr.dates,y=wtr.all$top, col="gray50", lwd = 2)
                               lines(x=wtr.dates,y=wtr.all$bottom,col="gray80", lwd = 2)
                               axis(side = 2)
                               axis(side = 3, labels=format(datestoshow, ttformat), at = datestoshow, pos = c(min(depths)), tck = -0.03)})
  
}
