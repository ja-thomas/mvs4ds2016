library("HSAUR2")
library("MVA")
data(USairpollution, package = "HSAUR2")

cols = colnames(USairpollution)
combinations = combn(cols, 2)
for (i in 1:dim(combinations)[2]){
  jpeg(file=paste0("bvbox/",combinations[1,i], combinations[2,i],".jpeg"))
  x <- USairpollution[, c(combinations[1,i], combinations[2,i])]  
  bvbox(x, mtitle = "", xlab = combinations[1,i], ylab = combinations[2,i])
  text(x[,combinations[1,i]], x[,combinations[2,i]], labels = rownames(USairpollution), cex = 0.7, pos = c(2, 2, 4, 2, 2))
  dev.off()
  jpeg(file=paste0("chi/",combinations[1,i], combinations[2,i],".jpeg"))
  chiplot(x[,combinations[1,i]],x[,combinations[2,i]])
  dev.off()
}
