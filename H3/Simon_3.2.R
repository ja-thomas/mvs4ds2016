library("HSAUR2")
library("MVA")
data(USairpollution, package = "HSAUR2")

USairpollution

cols = colnames(USairpollution)
cols = cols[-1]

par(mfrow=c(4,3))
for(c in cols){
  l = loess(SO2 ~. ,data = USairpollution[,c("SO2", c)])
  s = data.frame(seq(min(USairpollution[,c]), max(USairpollution[,c]), length.out = 100))
  colnames(s) = c
  p = predict(l, newdata = s) 
  plot(unlist(s), p, type = "l")
  points(USairpollution[,c], USairpollution[,"SO2"])
}
