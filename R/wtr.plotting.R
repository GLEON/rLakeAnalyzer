#' Plots a heat-map of water temperature.
#' 
#' This creates a simple, default heatmap of water temperature.
#' 
#' 
#' @param wtr Data frame of water temperature loaded with
#' \code{\link{load.ts}}.
#' @param ...  Additional parameters supplied to \code{\link{filled.contour}}
#' to modify defaults. Common examples include \code{zlim} and
#' \code{plot.title}.
#' @author Jennifer Brentrup, Luke Winslow
#' @seealso \code{\link{load.ts}}
#' @keywords hplot
#' @examples
#' 
#' 	#Get the path for the package example file included
#' 	wtr.path <- system.file('extdata', 'Sparkling.daily.wtr', package="rLakeAnalyzer")
#' 	
#' 	#Load data for example lake, Sparkilng Lake, Wisconsin.
#' 	sp.wtr = load.ts(wtr.path)
#' 	
#' 	#Plot default figure
#' 	wtr.heat.map(sp.wtr)
#' 
#' 	#Change defaults supplied to filled.contour
#' 	wtr.heat.map(sp.wtr, zlim=c(0,15), plot.title="Sparkling Water Temp (C)")
#' @export
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
