
wtr = read.matrix('../../matlabVersion/Data/Sparkling.wtr', sep="\t", header=T ,as.is=T)

wtrDates = as.POSIXct(wtr[,1])
wtr = as.matrix(wtr[,2:ncol(wtr)])



