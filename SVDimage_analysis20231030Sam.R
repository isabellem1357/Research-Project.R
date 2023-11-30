install.packages('imager')
library(imager)
#Read Sam's png photo file into data
#dat <- load.image('/Users/sshen/Desktop/Lectures/SanDiego/2014SDSUJohnson Lecture2014/photos/samshen-AWJ-photo3.png')
setwd('R-Learning+Teaching+Arts/SVD')
getwd()
dat <- load.image('sam.png')

dim(dat)
#[1] 430 460   1   3
#430 rows and 460 columns,  1 photo frame, 3 RGB colors 
#If a video, 1 will become 150 frames or more

dat[1:3, 1:4,1,1]
#Show part of the data

#Plot the photo from the RGB array data
dev.off() #Clear the plotsetting defined earlier
plot(dat, 
     xlim = c(0, 430), ylim = c(0, 460),
     main = 'Sam')

#Generate B/W photo data using grayscale
graydat = grayscale(dat)
dim(graydat)
#[1] 430 460   1   1
#2430 rows and 460 columns,  1 photo frame, 1 grayscale [0, 1]
#plot the gray b/w photo
plot(graydat, 
     xlim = c(0, 430), ylim = c(0, 460),
     main = 'B/W Gray Sam')

#Generate anomaly data for SVD analysis
mean(graydat)
#[1] 0.6732867
#Otherwise the first eigenmode has too much variance due to mean
grayanom = graydat - mean(graydat)
plot(grayanom, 
     xlim = c(0, 430), ylim = c(0, 460),
     main = 'Anomaly Sam')

#Compare the orignal data and anomaly data figures
par(mfrow = c(1, 2))
plot(graydat, 
     xlim = c(0, 430), ylim = c(0, 460),
     main = 'B/W Sam')
plot(grayanom, 
     xlim = c(0, 430), ylim = c(0, 460),
     main = 'Anomaly Sam')
dev.off()

#Compare color and B/W figures
par(mfrow = c(1, 2))
plot(dat, 
     xlim = c(0, 430), ylim = c(0, 460),
     main = 'Sam')
plot(graydat, 
     xlim = c(0, 430), ylim = c(0, 460),
     main = 'B/W Sam')
dev.off()


# More details about the Sam data 
width(dat)
#[1] 430 resolution lines
height(dat)
#[1] 460 resolution lines
depth(dat)
#[1] 1
spectrum(dat)
#[1] 3  three colors


#Plot the data for one of the three colors 
colorDat = apply(dat[,, 1, 1], 1, rev)
image(t(colorDat), main = 'Sam')
#x-y are arranged differently for image and plot

#Compare the color figure and a single color 
par(mfrow = c(1, 2))
plot(dat, 
     xlim = c(0, 430), ylim = c(0, 460),
     main = 'Sam')
image(t(colorDat), main = 'Sam in R Color')
dev.off()


#SVD of the original grayscale data
svdDat = svd(graydat)
SVDd = svdDat$d
percentD = 100*(SVDd^2)/sum(SVDd^2)
cumpercentD = cumsum(percentD)
modeK = 1:length(SVDd)
dev.off()
plot(modeK[1:50], percentD[1:50], 
     type = 'o', col = 'blue',
     xlab = 'Mode number', pch = 16,
     ylab = 'Percentage of mode variance [%]',
     main = 'Scree plot')
#The first eigenmode is about 90% of variance

#SVD analysis of anomaly data
svdAnom = svd(grayanom)
SVDd = svdAnom$d
percentD = 100*(SVDd^2)/sum(SVDd^2)
cumpercentD = cumsum(percentD)
modeK = 1:length(SVDd)
dev.off()
#The scree plot for the first K modes
par(mar = c(4.5, 4.5, 2, 4.5))
K = 100
cumpercentD[K]
#[1] 99.33273 %
plot(modeK[1:K], percentD[1:K], 
     type = 'o', col = 'red', lwd = 1.5,
     xlab = 'Mode number', pch = 16,
     ylab = 'Percentage of mode variance',
     main = 'Scree Plot of Anomaly Sam Data')
legend(10,30, col = c("red"),lty = 1,lwd = 2.0,
       legend = c("Percentage variance"),
       bty = "n",text.font = 2,cex = 1.0)
par(new = TRUE)
plot(modeK[1:50], cumpercentD[1:50],
     type = "o", col = "blue",
     lwd = 1.5, axes = FALSE,
     xlab = "", ylab = "")
legend(10,80, col = c("blue"),lty = 1,lwd = 2.0,
       legend = c("Cumulative variance"),
       bty = "n",text.font = 2,cex = 1.0)
#Suppress the axes and assign the y-axis to side 4
axis(4, col ='red', col.ticks = 'red', col.axis ='red')
mtext("Cumulative variance [%]", col = 'red',
      side = 4,line = 3)
#The first eigenmode accounts about 35% variance

#Reconstructed data from the from K1 to K2 modes of SVD
svdAnom = svd(grayanom)
SVDd = svdAnom$d
D = diag(SVDd)
U = svdAnom$u
V = svdAnom$v
K1 = 1
K2 = 100
reconK1K2 = U[, K1:K2]%*%D[K1:K2, K1:K2]%*%t(V[,K1:K2])
min(reconK1K2)
#[1] -0.5744586
recomIM = apply(reconK1K2, 1, rev) - min(reconK1K2)

#Difference data
diff = grayanom[,,1,1] - reconK1K2
diffIM = apply(diff, 1, rev) - min(diff)

#Plot the reconstructed image
image(t(recomIM), 
      col=grey(seq(0, 1, length = 256)), axes = T,
      main = 'Reconstructed Sam from SVD')

#Plot the difference image
image(t(diffIM), 
      col=grey(seq(0, 1, length = 256)), axes = T,
      main = 'Residual/Difference')

#Compare the original, recon, and difference
dev.off()
par(mfrow = c(1, 3))
plot(grayanom, 
     axes = F,
     main = 'Anomaly Sam')
image(t(recomIM), 
      col=grey(seq(0, 1, length = 256)), axes = F,
      main = 'Reconstructed Sam')
image(t(diffIM), 
      col=grey(seq(0, 1, length = 256)), axes = F,
      main = 'Residual Sam')

