#____________________________________________HOMEWORK 6__________________________________________________________

#____________________________________Principal Component Analysis________________________________________________

library(GGally)
library(DAAG)

#Read in the data and create a matrix
data <- read.table("uscrime.txt", header = TRUE)
head(data)

#Plotting data to look at correlations
ggpairs(data, columns = c('M', 'Ed', 'Po1','Po2','LF', 'M.F', 'Pop', 'NW', 'U1', 'U2','Wealth', 'Ineq','Prob','Crime'))

#Assign the response to the y variable
y <- data[,16]
head(y)

#Assign the predictor to the x variables
x<- data[,1:15]
head(x)


#Conduct PCA on the data
pca <- prcomp(x, center=TRUE, scale=TRUE)
summary(pca)

#Plot the PCA
screeplot(pca, type='lines', col='blue')


#use only 4 PC
pc <- pca$x[,1:4]
#Combine the predictors of the PCA with the crime response column
crimepc <- as.data.frame(cbind(pc, y))
#Create a linear model 
pcamodel <- lm(y ~ PC1 + PC2 + PC3 + PC4, data = crimepc)
#cross-validate
c <- cv.lm(crimepc, pcamodel ,m=5) 
# total sum of squared differences between data and its mean
SStot <- sum((crimepc$y - mean(crimepc$y))^2)
# mean squared error, times number of data points, gives sum of squared errors
SSres_c <- attr(c,"ms")*nrow(dat) 
#Calculate the r-squared value
print(1 - SSres_c/SStot)
##.106


#use only 5 PC and cross validate
pc <- pca$x[,1:5]
#Combine the predictors of the PCA with the crime response column
crimepc <- as.data.frame(cbind(pc, y))
#Create a linear model 
pcamodel <- lm(y ~ PC1 + PC2 + PC3 + PC4 + PC5, data = crimepc)
#cross-validate
c <- cv.lm(crimepc, pcamodel ,m=5) 
# total sum of squared differences between data and its mean
SStot <- sum((crimepc$y - mean(crimepc$y))^2)
# mean squared error, times number of data points, gives sum of squared errors
SSres_c <- attr(c,"ms")*nrow(dat) 
#Calculate the r-squared value
print(1 - SSres_c/SStot)
##.487


#use only 6 PC and cross validate
pc <- pca$x[,1:6]
#Combine the predictors of the PCA with the crime response column
crimepc <- as.data.frame(cbind(pc, y))
#Create a linear model 
pcamodel <- lm(y ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6, data = crimepc)
#cross-validate
c <- cv.lm(crimepc, pcamodel ,m=5) 
# total sum of squared differences between data and its mean
SStot <- sum((crimepc$y - mean(crimepc$y))^2)
# mean squared error, times number of data points, gives sum of squared errors
SSres_c <- attr(c,"ms")*nrow(dat) 
#Calculate the r-squared value
print(1 - SSres_c/SStot)
##.463


#use 7 PC
pc <- pca$x[,1:7]
#Combine the predictors of the PCA with the crime response column
crimepc <- as.data.frame(cbind(pc, y))
#Create a linear model 
pcamodel <- lm(y ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7, data = crimepc)
#cross-validate
c <- cv.lm(crimepc, pcamodel ,m=5) 
# total sum of squared differences between data and its mean
SStot <- sum((crimepc$y - mean(crimepc$y))^2)
# mean squared error, times number of data points, gives sum of squared errors
SSres_c <- attr(c,"ms")*nrow(dat) 
#Calculate the r-squared value
print(1 - SSres_c/SStot)
##.456


#use 15 PC
pc <- pca$x[,1:15]
#Combine the predictors of the PCA with the crime response column
crimepc <- as.data.frame(cbind(pc, y))
#Create a linear model 
pcamodel <- lm(y ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + PC11 + PC12 + PC13 + PC14 + PC15, data = crimepc)
#cross-validate
c <- cv.lm(crimepc, pcamodel ,m=5) 
# total sum of squared differences between data and its mean
SStot <- sum((crimepc$y - mean(crimepc$y))^2)
# mean squared error, times number of data points, gives sum of squared errors
SSres_c <- attr(c,"ms")*nrow(dat) 
#Calculate the r-squared value
print(1 - SSres_c/SStot)
##.413


#Since using 5 PCs had the best R-squared, I will use that to extract coefficients

#Extract the coefficients and do vecotr multiplication for alphas
pcaintercept <- pcamodel$coefficients[1]
pcaintercept
pcacoefficient <- summary(pcamodel)$coefficients[2:5]
pcacoefficient
eigenvectors <- pca$rotation[,1:4]
eigenvectors
alpha <- (eigenvectors %*% pcacoefficient)
alpha

#Now that we have the alphas we can unscale to make predictions
mean <- pca$center
std <- pca$scale
newAlpha <- mean/std
newAlpha
newintercept <- pcaintercept - sum(alpha*mean/std)
newintercept


prediction <- (1666 + (11.026 * 14) + (9.443 * 10) + (2.860 * 12) + (2.869 * 15.5) + (13.887 * .640) + (33.360 * 94) + 
                 (.962 * 150) + (.983 * 1.1) + (5.295 * .120) + (4.023 * 3.6) + (5.445 * 3200) + (4.863 * 20.1) + (.04 * 2.071) 
                  + (3.753 * 39))

