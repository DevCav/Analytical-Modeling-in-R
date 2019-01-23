#Devin Cavagnaro Homework 1 Submission

# Attach Packages
library(kknn)         # K-nearest neighbors
library(kernlab)      # SVM methodology
library(Matrix)

#Import data and conver to a matrix
dataf <- read.table(file="C://Users//dcavagnaro//Documents//Training//GT Analytics//Intro to Modeling//HW1//credit_card_data-headers.txt", sep="\t", quote="", comment.char="")
data <- dataf.matrix(data, rownames.force = NA)

# Questions 1: call ksvm
model <- ksvm(data[,1:10],data[,11], type="C-svc", kernel="vanilladot", C=100, scaled=TRUE)
# calculate a1.am
a <- colSums(model@xmatrix[[1]] * model@coef[[1]])
# calculate a0
a0 <- model@b
#see what the model predicts
pred <- predict(model,data[,1:10])
pred
# see what fraction of the model's predictions match the actual classification
sum(pred == data[,11]) / nrow(data)


# Question 2: call ksvm
model2 <- ksvm(data[,1:10],data[,11], type="C-svc", kernel="splinedot", C=100, scaled=TRUE)
#see what the model predicts
pred2 <- predict(model2,data[,1:10])
pred2
# see what fraction of the model's predictions match the actual classification
sum(pred2 == data[,11]) / nrow(data)


# Question 3: cll knn
suppressWarnings(suppressMessages(library(kknn)))
kmodel <- train.kknn(V11 ~., data = dataf, kmax = 100, scale=TRUE)
#Optimal number of K is 44
pred3 <- predict(kmodel,dataf[,1:10])
# see what fraction of the model's predictions match the actual classification
sum(pred3 == dataf[,11]) / nrow(data)
