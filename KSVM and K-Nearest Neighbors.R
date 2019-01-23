# Two #s denotes R output
#-------------------------------------------- Question 3.1 -----------------------------------------------------------------

#----------------Cross Validate Using KNN

# Attach Packages
library(kknn)         # K-nearest neighbors
library(kernlab)      # SVM methodology
library(caret)

#Import data and conver to a matrix
dataf <- read.table(file="C://Users//dcavagnaro//Documents//Training//GT Analytics//Intro to Modeling//HW1//credit_card_data-headers.txt", sep="\t", quote="", comment.char="", header=TRUE)
myMatrix <- data.matrix(dataf, rownames.force = NA)

#The first thing we are going to do is to split the data set into training and testing dataset. This is done with
#the caret library. The createDataPartition function split the data in the ratio of 70:30 and returns a matrix.
set.seed(3033)
intrain <- createDataPartition(y = dataf$R1, p= 0.7, list = FALSE)

#After the partition is created we can then seprate the dataframe into two differnt data sets. One is training and one is test.
training <- dataf[intrain,]
testing <- dataf[-intrain,]

#Check the dimensions of the training and test data to make sure they are split 70:30 with the same number of columns.
dim(training); dim(testing);
## [1] 458  11
## [1] 196  11

#Response column consists of 1 and 0. We should convert this to be categorical
training[["R1"]] = factor(training[["R1"]])


#We can now begin to train the KNN model. The first line of code below uses the caret trainControl to do cross validation. The 
#number is the number of K-folds.
trctrl <- trainControl(method = "cv", number = 10)

#Below is the training of the model using cross validation.
set.seed(3333)
knn_fit <- train(R1 ~., data = training, method = "knn",
                 trControl=trctrl,
                 preProcess = c("center", "scale"),
                 tuneLength = 10)

#Check the results of the cross validation. We can see that the final model users had a k=5. We can also see the results of 
#our 10 models that were set using number=10.
knn_fit

## k-Nearest Neighbors 

## 458 samples
## 10 predictor
## 2 classes: '0', '1' 

## Pre-processing: centered (10), scaled (10) 
## Resampling: Cross-Validated (10 fold) 
## Summary of sample sizes: 413, 412, 412, 412, 412, 412, ... 
## Resampling results across tuning parameters:

## k   Accuracy   Kappa    
## 5  0.8714010  0.7376315
## 7  0.8582126  0.7101613
## 9  0.8582609  0.7079403
## 11  0.8625604  0.7167372
## 13  0.8647343  0.7209529
## 15  0.8648309  0.7212254
## 17  0.8603865  0.7120777
## 19  0.8539130  0.6978934
## 21  0.8604831  0.7120955
## 23  0.8604348  0.7123942

## Accuracy was used to select the optimal model using the largest value.
## The final value used for the model was k = 5."


#We can now apply our model to the test data using a K=5
test_pred <- predict(knn_fit, newdata = testing)
test_pred

#Check the accuracy of the model on the test data set. This returned with an accuracy of .81
sum(test_pred == testing[,11]) / nrow(testing)
## [1] 0.8163265


#----------------Cross Validate Using SVM

#We can use the same code as listed above in the caret package, however, this time we will change the method to be KSM
set.seed(3333)
ksvm_fit <- train(R1 ~., data = training, method = "svmLinear",
                 trControl=trctrl,
                 preProcess = c("center", "scale"),
                 tuneLength = 10)

#Again we print the cross validation results
ksvm_fit

## Support Vector Machines with Linear Kernel 

## 458 samples
## 10 predictor
## 2 classes: '0', '1' 

## Pre-processing: centered (10), scaled (10) 
## Resampling: Cross-Validated (10 fold) 
## Summary of sample sizes: 412, 412, 412, 413, 412, 412, ... 
## Resampling results:
  
## Accuracy   Kappa    
## 0.8687923  0.7411126

## Tuning parameter 'C' was held constant at a value of 1


#We can now apply our model to the test data using a C=1
test_pred <- predict(ksvm_fit, newdata = testing)
test_pred

#Check the accuracy of the model on the test data set. This returned with an accuracy of .81
sum(test_pred == testing[,11]) / nrow(testing)
## [1] 0.8469388


