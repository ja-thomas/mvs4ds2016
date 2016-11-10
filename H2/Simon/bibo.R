library("HSAUR2")
library("MVA")
data(USairpollution, package = "HSAUR2")


mlab <- "Manufacturing enterprises with 20 or more workers"
plab <- "Population size (1970 census) in thousands"

outcity <- match(lab <- c("Chicago", "Detroit","Cleveland", "Philadelphia"), rownames(USairpollution))
cols = colnames(USairpollution)
combinations = combn(cols, 2)
for (i in 1:dim(combinations)[2]){
  jpeg(file=paste0(combinations[1,i], combinations[2,i],".pdf"))
  x <- USairpollution[, c(combinations[1,i], combinations[2,i])]  
  bvbox(x, mtitle = "", xlab = combinations[1,i], ylab = combinations[2,i])
  text(x[,combinations[1,i]], x[,combinations[2,i]], labels = rownames(USairpollution), cex = 0.7, pos = c(2, 2, 4, 2, 2))
  dev.off()
}
x <- USairpollution[, c("manu", "popul")]  
bvbox(x, mtitle = "", xlab = mlab, ylab = plab) 
text(x$manu[outcity], x$popul[outcity], labels = lab, cex = 0.7, pos = c(2, 2, 4, 2, 2))

sapply(colnames(USairpollution), function(x){
  sapply(colnames(USairpollution), function(y){
    outcity <- match(lab <- c("Chicago", "Detroit","Cleveland", "Philadelphia"), rownames(USairpollution))
    z <- USairpollution[, c("manu", "popul")]  
    bvbox(z, mtitle = "", xlab = x, ylab = y) 
    text(z$x[outcity], z$y[outcity], labels = lab, cex = 0.7 )
  })
})