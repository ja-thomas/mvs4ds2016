#library("MVA")
#demo("Ch-Viz")
#library(ggplot2)

library(HSAUR2)
library("MVA")
#library(car)
library("KernSmooth")
library(lattice)
data(USairpollution, package = "HSAUR2")
data(pottery, package = "HSAUR2")
data(quakes)
measure <- read.csv("E:/LMU/[WS16] Multivariant Statistics/Exercise/measure.csv", sep=";")

### Exercise 2.1

# bivariate boxplot on the scatterplot
# combinations= combn(cols, 2)
par(mfrow = c(3, 7))
for (i in 1:(ncol(USairpollution) - 1)) {
    for (j in (i+1):ncol(USairpollution)) {
        x = USairpollution[, c(i,j)]
        bvbox(x, mtitle = "", xlab = colnames(USairpollution)[i], 
              ylab = colnames(USairpollution)[j])
        text(x, cex = 0.6, labels = abbreviate(row.names(USairpollution))) 
    }
}

# cov using all the data
cov(USairpollution)

# cov using data without outliers
outcity = match(c("Chicago", "Detroit", "Cleveland",
                  "Philadelphia", "Miami", "Phoenix",
                  "Albuquerque", "Denver"), 
                rownames(USairpollution))
cov(USairpollution[-outcity,])

### Exercise 2.2

# chi-plot for each pair
par(mfrow = c(3, 7))
for (i in 1:(ncol(USairpollution) - 1)) {
  for (j in (i+1):ncol(USairpollution)) {
    x = USairpollution[, c(i,j)]
    chiplot(x[,1], x[,2], 
            main = paste(colnames(USairpollution)[i], " v.s. ", colnames(USairpollution)[j]))
    #text(x, cex = 0.6, labels = abbreviate(row.names(USairpollution))) 
  }
}

### Exercise 2.3
par(mfrow = c(3, 3))

# construct a scatterplot matrix manually
for(i in 1:3){
  for(j in 1:3){
    if(i == j)
      boxplot(measure[,i], main = colnames(measure)[i])
    else
      bvbox(measure[,c(i,j)],mtitle = "", 
            xlab = colnames(measure)[i], 
            ylab = colnames(measure)[j])
  }
}

pairs(measure, panel = function(x,y) bvbox(cbind(x,y), add = TRUE))

### Exercise 2.4

par(mfrow = c(3, 3))
for(i in 1:3){
  for(j in 1:3){
    
    if(i == j){
      #Boxplot(measure[,i], main = colnames(measure)[i],
              #labels = abbreviate(measure$gender), ylab = NULL)
      
      par(mar = c(0,0,0,0))
      plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
      
      text(x = 0.3, y = 0.5, paste(colnames(measure)[i]), 
           cex = 3.5, col = "black", family="serif", font=2, adj=0.5)
    }
      
    else{

      selected_m = measure[measure$gender == "male", c(i, j)]
      selected_f = measure[measure$gender == "female", c(i, j)]
      
      measured_m = bkde2D(selected_m[, c(1,2)], 
                          bandwidth = sapply(selected_m[, c(1,2)], dpik))
      measured_f = bkde2D(selected_f[, c(1,2)], 
                          bandwidth = sapply(selected_f[, c(1,2)], dpik))
      plot(measure[, c(i, j)],
           xlab = colnames(measure)[i],
           ylab = colnames(measure)[j])
      contour(x = measured_m$x1, y = measured_m$x2,
              z = measured_m$fhat, add = TRUE, col = "blue")
      contour(x = measured_f$x1, y = measured_f$x2,
              z = measured_f$fhat, add = TRUE, col = "red")
      # p[i,j] <- ggplot(measure, aes(x = measure[,i], y = measure[,j], colour = gender)) + stat_density2d()
      text(measure[,i], measure[,j], cex = 0.6,
      labels = abbreviate(measure$gender))
    }
  }
} 


### Exercise 2.5
par(mar = c(1, 1, 1, 1))
n = ncol(pottery) - 1
par(mfrow = c(n, n))
for (i in 1:n) {
  for(j in 1:n){
    
    if(i == j){
      #d = density(pottery[, i])
      #plot(d, main = colnames(pottery)[i])
      #text(d, cex = 0.6,
           #labels = abbreviate(pottery$kiln))
      par(mar = c(0,0,0,0))
      plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
      
      text(x = 0.3, y = 0.5, paste(colnames(pottery)[i]), 
           cex = 1.5, col = "black", family="serif", font=2, adj=0.5)
    }
    else{
      measured = bkde2D(pottery[, c(i, j)], 
                bandwidth = sapply(pottery[, c(i, j)], dpik))
      
      plot(pottery[, c(i, j)],
           xlab = colnames(pottery)[i],
           ylab = colnames(pottery)[j])
      contour(x = measured$x1, y = measured$x2,
              z = measured$fhat, add = TRUE)
      text(pottery[,i], pottery[,j], cex = 0.6,
           labels = abbreviate(pottery$kiln))
      
    }
  }
  
}


### Exercise 2.6
par(mfrow = c(1, 1))
#ylim <- with(quakes, range(long)) * c(0.95, 1)
sub1 = subset(quakes, subset = mag > 4 & mag <= 4.8)
sub2 = subset(quakes, subset = mag > 4.8 & mag <= 5.6)
sub3 = subset(quakes, subset = mag >5.6 & mag <= 6.4)

plot(xyplot(lat ~ long| cut(mag, 3), data = quakes,
            layout = c(3,1), xlab = "Longitude",
            ylab = "Latitude"))
with(sub1, symbols(lat, long, circles = depth, 
                   inches = 0.5, add = TRUE))
with(sub2, symbols(lat, long, squares = depth, 
                   inches = 0.5, add = TRUE))
with(sub3, symbols(lat, long, circles = depth, 
                   inches = 0.5, add = TRUE))