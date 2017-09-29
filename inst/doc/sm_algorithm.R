## ---- message=FALSE------------------------------------------------------
library(rLakeAnalyzer)
library(knitr)

## ------------------------------------------------------------------------
data("latesummer")
wldf <- wtr.layer(depth = latesummer$depth, measure = latesummer$temper)
knitr::kable(wldf)

## ---- eval = TRUE, echo=TRUE---------------------------------------------
wldf$segments

## ---- fig.show = "hold", fig.width = 8, fig.height = 6-------------------
plot(y = latesummer$depth, x = latesummer$temper, ylim = rev(range(latesummer$depth)))
abline(h = wldf$cline, col='blue')
abline(h = wldf$mld, col='red')
abline(h = wldf$min_depth, col='green')
text(16, wldf$cline+3, "Thermocline", col = 'blue')
text(16, wldf$mld+3, "Mix Layer Depth", col = 'red')
text(16, wldf$min_depth+3, "Minimum Depth", col = 'green')

