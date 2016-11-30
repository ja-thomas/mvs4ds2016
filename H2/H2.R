library(ggplot2)
library(gridExtra)
library(MVA)

#Ex. 2.1 Use the bivariate boxplot on the scatterplot of each pair of 
#variables in the air pollution data to identify any outliers. 
#Calculate the correlation between each pair of variables 
#using all the data and the data with any identified outliers removed.
#Comment on the results.

data("USairpollution")
i <- 3
j <- 4

dev.new()
par(mfrow=c(3, 7))
for(i in seq(1, 7)){
  for(j in seq(i, 7)) {
    if (i != j) {
      bvbox(USairpollution[, c(i, j)], xlab=names(USairpollution)[i], ylab=names(USairpollution)[j]) 
    }
  }
}
par(mfrow=c(1,1))

#combns <- combn(seq(1, 7), 2)
# par(mfrow=c(3,7))
# for(i in seq(1, ncol(combns))) {
#   bvbox(USairpollution[, c(combns[1, i])], xlab=names(USairpollution)[combns[1, i]], ylab=names(USairpollution)[combns[2, i]])
# }


#Ex. 2.2 Compare the chi-plots with the corresponding scatterplots for each pair of variables
#in the air pollution data. Do you think that there is any advantage in the former?

compareScatChi <- function(data, i, j) {
  dev.new()
  par(mfrow=c(2, 1))
  #scatter plot
  plot(data[[i]], data[[j]], xlab=names(data)[i], ylab=names(data[j]))
  #chi squared plot
  chiplot(data[[i]], data[[j]])
  # cm <- colMeans(data)
  # S <- cov(data)
  # d <- apply(data, 1, function(x) t(x - cm) %*% solve(S) %*% (x - cm))
  # plot(qc <- qchisq((1:nrow(data) - 1/2) / nrow(data), df = 6), sd <- sort(d),
  #      xlab = expression(paste(chi[6]^2, " Quantile")),
  #      ylab = "Ordered distances", xlim = range(qc) * c(1, 1.1))
  # oups <- which(rank(abs(qc - sd), ties = "random") > nrow(data) - 3)
  # text(qc[oups], sd[oups] - 1.5, names(oups))
  # abline(a = 0, b = 1)
}

compareScatChi(USairpollution, 1, 2)

#Ex. 2.3 Construct a scatterplot matrix of the body measurements data that has the appropriate 
# boxplot on the diagonal panels and bivariate boxplots on the other panels. 
# Compare the plot with Figure 2.17, and say which diagram you find more informative about the data.
setwd("/Volumes/HDD/Dropbox/Uni/M.Sc. Data Science/16_17/MVS/Exercises/H")
measures <- read.csv("data/measure.csv", header = TRUE, sep = ";")
measures <- measures[, c(1, 2, 3)]
summary(measures)

par(mfrow=c(ncol(measures), ncol(measures)))
for (i in 1:ncol(measures)) {
  for (j in 1:ncol(measures)) {
    if(i != j) {
      bvbox(measures[, c(i, j)])
    }
    else {
      boxplot(measures[[i]])
    }
  }
}

#Ex. 2.4 Construct a further scatterplot matrix of the body measurements data that labels
# each point in a panel with the gender of the individual, and plot on each scatterplot the 
# separate estimated bivariate densities for men and women.
setwd("/Volumes/HDD/Dropbox/Uni/M.Sc. Data Science/16_17/MVS/Exercises/H")
measures <- read.csv("data/measure.csv", header = TRUE, sep = ";")
summary(measures)

ncols <- 3 # only the first 3 columns are numeric

par(mfrow=c(ncols, ncols))
for (i in 1:ncols) {
  for (j in 1:ncols) {
    plot(measures[, i], measures[, j], xlab = names(measures)[i], ylab=names(measures)[j])
    if(i != j) {
      bvbox(measures[measures$gender == "male", c(i, j)], add=TRUE)
      bvbox(measures[measures$gender == "female", c(i, j)], add=TRUE)
    }
    points(measures[measures$gender == "male", c(i, j)], col="blue")
    points(measures[measures$gender == "female", c(i, j)], col="red")
  }
}


#Ex. 2.5 Construct a scatterplot matrix of the chemical composition of Romano-British pottery
# given in Chapter 1 (Table 1.3), identifying each unit by its kiln number 
# and showing the estimated bivariate density on each panel. What does the resulting diagram tell you?
library(HSAUR2)
data(pottery, package = "HSAUR2")



#Ex. 2.6 Construct a bubble plot of the earthquake data using latitude and longitude as the scatterplot
# and depth as the circles, with greater depths giving smaller circles. 
# In addition, divide the magnitudes into three equal ranges and label the points in your bubble plot
# with a different symbol depending on the magnitude group into which the point falls.

