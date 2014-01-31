wtr.heat.map <- function(wtr, ...){
  
  depths = get.offsets(wtr)
  
  n = nrow(wtr)
  
  wtr.dates = wtr$datetime
  
  wtr.mat = as.matrix(wtr[,-1])
  y = depths

  
  filled.contour(wtr.dates, y, wtr.mat, ylim=c(max(depths),0), nlevels=100,
      color.palette=colorRampPalette(c("violet","blue","cyan", "green3", "yellow", "orange", "red"), 
      bias = 1, space = "rgb"), ylab='Depths (m)', ...) #Sets range and value of color hues
  
}
