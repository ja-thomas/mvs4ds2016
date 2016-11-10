library(MVA)

bodydata = read.csv("measure.csv",sep=";")
head(bodydata)

bodydata_without_gender = bodydata[1:3] # remove non numerical values (gender)
pairs(bodydata_without_gender) # you can't use OOB scatterplot matrix here

par(mfrow=c(3,3))
boxplot(bodydata_without_gender$chest, main = "chest")
bvbox(cbind(bodydata_without_gender$waist,bodydata_without_gender$chest),xlab = "waist", ylab = "chest")
bvbox(cbind(bodydata_without_gender$hips,bodydata_without_gender$chest), xlab = "hips",ylab="chest")
bvbox(cbind(bodydata_without_gender$chest,bodydata_without_gender$waist), xlab = "chest",ylab="waist")
boxplot(bodydata_without_gender$waist, main = "waist")
bvbox(cbind(bodydata_without_gender$hips,bodydata_without_gender$waist), xlab = "hips",ylab="waist")
bvbox(cbind(bodydata_without_gender$hips,bodydata_without_gender$chest), xlab = "hips",ylab="chest")
bvbox(cbind(bodydata_without_gender$hips,bodydata_without_gender$waist), xlab = "hips",ylab="waist")
boxplot(bodydata_without_gender$hips, main = "hips")