#install.packages('MASS') # To be installed once
#install.packages('kernlab') # To be instaled once
library('MASS')
library("kernlab")

# Setting working directory to the local directory for assignment files
setwd('C:/Vijaya/Course/Deakin/SIT743 Multivariate and Categorical Analysis/Assignment/Assignment 1')


####### Solution 1 #########
the.data<-as.matrix(read.table("BikeShareTabSep.txt"))
my.data <- the.data [sample(1:727,400),c(1:9)]
write.table(my.data,"name-StudentID-BikeShareMyData.txt")

## 1.1
# Registered users histogram

# Registered user is the 9th column
hist(my.data[,9],col='pink',xlab='Registered Users',main='Histogram of Registered Users',
     col.main='blue',
     col.lab='plum4')

# Temperature histogram

# Temperature is the 4th column
hist(my.data[,4],col='pink',xlab='Temperature',main='Histogram of Temperature',
     col.main='blue',
     col.lab='plum4')



## 1.2
# Five number summary and mean value of Casual users (8th column)
summary(my.data[,8])

# Five number summary and mean value of Registered users (9th column)
summary(my.data[,9])

## 1.3
# Parallel Boxplot of casual users (8th column) and registered users (9th column)
boxplot(my.data[,8],my.data[,9],col=c("gold","yellowgreen"),
        names = c("Casual users", "Registered users"),
        xlab='Bike Users',
        ylab='Daily Count',
        main='Boxplot of Bike users',
        col.main='blue',
        col.lab='maroon')



## 1.4

# Scatterplot of Temperature (4th column) and Casual users (8th column)
my.data.200 <- head(my.data,200)
plot(my.data.200[,4],my.data.200[,8],col='violetred',pch=16,
     xlab='Temperature',ylab='Casual users',
     main='Scatter plot: Temperature vs Casual users',
     col.main='blue',
     col.lab='tomato4')

## 1.5

# Linear regression fit for casual users (8th column) based on temperature (4th column)
lm(my.data.200[,8]~my.data.200[,4])
# Linear Equation -> y = 3.093x -12.565

# Linera regression abline on the scatter plot of
# Temperature (4th column) and Casual users (8th column)
abline(lm(my.data.200[,8]~my.data.200[,4]),col='green4')

# Correlation coefficient between temperature (4th column) and casual users (8th column)
my.data.cor <- cor(my.data.200[,4],my.data.200[,8])
my.data.cor

# Coefficient of Determination of temperature and casual users
my.data.cov <- my.data.cor ** 2
my.data.cov

####### Solution 6 #########

# Import Bike Share data and take 200 data points of columns 4 to 9 (Temperature, 
# Feeling Temperature, Humidity, Windspeed, Casual uers, Registered users)
the.data <- as.matrix(read.table("BikeShareTabSep.txt"))
selData <- the.data [sample(1:727,100),c(4:9)]
write.table(selData,"name-StudentID-PCASelData.txt")

## 6.1) 
# PCA on selData
pZ <- prcomp(selData, tol = 0.01, scale = TRUE)
pZ
summary(pZ)
biplot(pZ)

## 6.2)
# Graph of variance vs principal components
plot(pZ)
plot(pZ,type='l')

## 6.3)
#Classical MDS on selData
SelData.x<-as.matrix(selData)
selData.dist<-dist(SelData.x)
selData.mds<-cmdscale(selData.dist)
selData.mds
plot(selData.mds, type = "n",
     main='Classical MDS for Bike sharing data',
     col.main='blue')
text(selData.mds, labels = as.character(1:nrow(SelData.x)))

## 6.4)
# isoMDS on selData with k=2
fit<-isoMDS(selData.dist, k=2)
plot(fit$points, type = "n",
     main='isoMDS plot for Bike sharing data',
     col.main='blue')
text(fit$points, labels = as.character(1:nrow(SelData.x)))

## 6.5)
# Shepherd plot on selData when k=2
selData.sh <-Shepard(selData.dist, fit$points)
plot(selData.sh, pch= ".",
     main='Shepherd plot for isoMDS output',
     col.main='tomato4')
lines(selData.sh$x, selData.sh$yf, type = 'S', col='red')

## 6.6)
# isoMDS and shepherd plot on selData with k=2
fit2<-isoMDS(selData.dist, k=4)
plot(fit$points, type = "n",
     main='isoMDS plot for Bike sharing data with k=4',
     col.main='blue')
text(fit2$points, labels = as.character(1:nrow(SelData.x)))
selData.sh <-Shepard(selData.dist, fit2$points)
plot(selData.sh, pch= ".",
     main='Shepherd plot for isoMDS output with k = 4',
     col.main='tomato4')
lines(selData.sh$x, selData.sh$yf, type = 'S', col='red')

####### Solution 7 #########

# Import data for k-means
zz<-read.table("SITdata2018.txt")
zz<-as.matrix(zz)
zz

## 7.1 a)

# Scater plot before performing kmeans
plot(x=zz[,1],y=zz[,2],xlab="Variable 1",ylab="Variable 2",main="Scatter Plot: SIT data",
     col='violetred',
     col.main='blue',
     col.lab='maroon')

## 7.1 b)
# Number of clusters = 4

## 7.1 c)

# K-means clustering with k=4
(cl  <- kmeans(zz,4,nstart = 25))
plot(zz,col=cl$cluster)
points(cl$centers,col=1:4,pch=22)

## 7.1 d)

# Determining k from scree plot
# kmeans for k value from 2 to 20
totwss= array(,c(20,1))
for (i in 2:20)
{
  print(i)
  totwss[i,1]=(kmeans(x,centers=i))$tot.withinss
  print(totwss[i])
}

# Plot chart showing TOTWSS vs k
plot(totwss, xlab="k",ylab="TOTWSS",main="TOTWSS with different K values",
     col='violetred',
     col.main='blue',
     col.lab='green4')
totwss

# From the scatterplot choose 9 as k
(cl <-kmeans(zz, 9, nstart= 25))
plot(zz, col = cl$cluster)
points(cl$centers, col = 1:9, pch= 8)

## 7.2)

# Read data again for spectral clustering
zz<-read.table("SITdata2018.txt")
zz<-as.matrix(zz)

# Spectral clustering with 4 clusters
sc<-specc(zz, centers=4)
sc
centers(sc)
size(sc)
withinss(sc)

# Scatterplot of the data with spectral clustering
plot(zz, col=sc)
