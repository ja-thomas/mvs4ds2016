library("HSAUR2")
library("gplots")
library("KernSmooth")
library("MASS")

data(heptathlon, package="HSAUR2")

pairs(heptathlon)
contour(z=heptathlon)
filled.contour(heptathlon)

par(mfrow=c(4,7))
cols = colnames(heptathlon)
combinations = combn(cols,2)
for(i in 1:dim(combinations)[2]){
  print(i)
  oned = kde2d(heptathlon[,combinations[1,i]], heptathlon[,combinations[2,i]])
  contour(oned, xlab = combinations[1,i], ylab = combinations[2,i])
}
