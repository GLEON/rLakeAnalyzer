## ---- echo = FALSE-------------------------------------------------------
library(knitr)

## ---- echo = FALSE-------------------------------------------------------
df2 <- data.frame(datetime = c("2008-07-01 01:00","2008-07-01 02:00","2008-07-01 03:00","2008-07-01 04:00"),
                 doobs_0.5= c("8.3","8.2","8.2","8.1"))
kable(df2)

## ---- echo = FALSE-------------------------------------------------------
df1 <- data.frame(datetime = c("2008-07-01 01:00","2008-07-01 02:00","2008-07-01 03:00","2008-07-01 04:00"),
                 wtr_0.5= c("22.3","22.31","22.31","22.32"),
                 wtr_1 = c("22.3","22.31","22.31","22.32"),
                 wtr_2 = rep(21, 4))
kable(df1)

## ---- echo = FALSE-------------------------------------------------------
df <- data.frame(Abbreviation = c("doobs","wtr","wnd","airT","rh"),
                 Variable = c("Dissolved Oxygen Concentration","Water Temperature","Wind Speed",
                              "Air Temperature","Relative Humidity"),
                 `Assumed Units` = c("mg/L ","°C","m/s","°C","%"))
kable(df)

## ---- eval = FALSE-------------------------------------------------------
#  tmp = data.frame()
#  write.table(tmp, "test.wtr", sep='\t', row.names=FALSE)

