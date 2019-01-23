#____________________________________________HOMEWORK 7__________________________________________________________

#_______________________________________Advanced Regression______________________________________________________

########################################      Tree        ######################################################
#Clean up the workspace
rm(list=ls())

#Import required libraries
library(DAAG)
library(tree)

#Read in the dataset
data <- read.table(file="uscrime.txt", stringsAsFactors = FALSE, header = TRUE)

#Validate structure of the data
head(data)
str(data)

#Set seed for replication
set.seed(1)

#Set tree regression model to the data
tree.data <- tree(Crime~., data = data)

#Print summary data for the tree model
summary(tree.data)


#Print the split of the tree
tree.data$frame

#Plot the tree
plot(tree.data)
text(tree.data)


#Adjust the number of nodes to prune the tree
nodes <- 5
prune.data <- prune.tree(tree.data, best = nodes)
summary(prune.data)

#Plot the prune tree
plot(prune.data)
text(prune.data)

#Determine if pruning the tree will increase performance by cross-validation
cv.data <- cv.tree(tree.data)

#Compare by looking at the sum of squared errors from 7 nodes down to 1 nodes
prune.tree(tree.data)$size
prune.tree(tree.data)$dev
cv.data$dev

#Cross-validation shows that the model didn't do that well
plot(cv.data$size, cv.data$dev, type = "b")

#Build a regression for each node rather than average for each trees leave
prune.data <- prune.tree(tree.data, best = 2)

#Seperate rows of data in each leaf
d1 <- data[which(prune.data$where == 2),]
d2 <- data[which(prune.data$where == 3),]

#Regression on the first leaf
m1 <- lm(Crime~., data = d1)
summary(m1)

#Regression on the second leaf
m2 <- lm(Crime~., data = d2)
summary(m2)


########################################      `rpart        ######################################################
library(rsample)     # data splitting 
library(dplyr)       # data wrangling
library(rpart)       # performing regression trees
library(rpart.plot)  # plotting regression trees
library(ipred)       # bagging
library(caret)       # bagging


#Set the seed and split data 
set.seed(1)
crime_split <- initial_split(data, prop = .7)
crime_train <- training(crime_split)
crime_test  <- testing(crime_split)


#Build a model on the training data
m1 <- rpart(
  formula = Crime~.,
  data    = crime_train
)

#Plot the data
rpart.plot(m1)


#Create a grid search to search for optimal parameters
hyper_tune <- expand.grid(
  minsplit = seq(5, 20, 1),
  maxdepth = seq(8, 15, 1)
)


#For loop to cycle through each of the parameters
models <- list()

for (i in 1:nrow(hyper_tune)) {
  
  # get minsplit, maxdepth values at row i
  minsplit <- hyper_tune$minsplit[i]
  maxdepth <- hyper_tune$maxdepth[i]
  
  # train a model and store in the list
  models[[i]] <- rpart(
    formula = Crime ~ .,
    data    = crime_train,
    method  = "anova",
    control = list(minsplit = minsplit, maxdepth = maxdepth)
  )
}

#Creating a function to find the minimum error with each parameter
# function to get optimal cp
get_cp <- function(x) {
  min    <- which.min(x$cptable[, "xerror"])
  cp <- x$cptable[min, "CP"] 
}

# function to get minimum error
get_min_error <- function(x) {
  min    <- which.min(x$cptable[, "xerror"])
  xerror <- x$cptable[min, "xerror"] 
}

hyper_tune %>%
  mutate(
    cp    = purrr::map_dbl(models, get_cp),
    error = purrr::map_dbl(models, get_min_error)
  ) %>%
  arrange(error) %>%
  top_n(-5, wt = error)


#Adding the minsplit and maxdepth after hypertuning
m2 <- rpart(
  formula = Crime ~ .,
  data    = crime_train,
  method  = "anova", 
  control = list(minsplit = 15, maxdepth = 13)
)

rpart.plot(m2)


pred <- predict(m2, newdata = crime_test)
RMSE(pred = pred, obs = crime_test$Crime)

########################################      Random Forest        ######################################################
library(randomForest) # basic implementation


#Set the seed and split data 
set.seed(1)
crime_split <- initial_split(data, prop = .7)
crime_train <- training(crime_split)
crime_test  <- testing(crime_split)


# default RF model
m1 <- randomForest(
  formula = Crime ~ .,
  data    = crime_train
)

plot(m1)

#Get the number of trees for minimum errror
which.min(m1$mse)

#Make predictions and evaluate the model
pred <- predict(m1, newdata = crime_test)
RMSE(pred = pred, obs = crime_test$Crime)

m1$importance

########################################   Logistic Regression     ######################################################

#Clean up the workspace
rm(list=ls())

#Read in the dataset
germandata <- read.table(file="germancredit.txt", sep = " ")

head(germandata)
str(germandata)


#Convert 1 and 2 to 0 and 1 for response variable
germandata$V21[germandata$V21==1] <- 0
germandata$V21[germandata$V21==2] <- 1


#Split data into training and testing data
nr <- nrow(germandata)
train_samples <- sample(1:nr, size = round(nr*0.7), replace = FALSE)
data_train <- germandata[train_samples,]
data_valid <- germandata[-train_samples,]


#Start building the logistic model
reg <- glm(V21~., family = binomial(link = 'logit'), data = data_train)
summary(reg)

#Start building the second model
reg <- glm(V21 ~ V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+V12+V14+V16+V20, family = binomial(link = 'logit'), data = data_train)
summary(reg)

#Start building the third model
reg <- glm(V21 ~ V1+V2+V3+V4+V6+V8+V9+V10+V14, family = binomial(link = 'logit'), data = data_train)
summary(reg)

#replace variables with binary values
data_train$V1A13[data_train$V1 == 'A13'] <- 1
data_train$V1A13[data_train$V1 != 'A13'] <- 0

data_train$V1A14[data_train$V1 == 'A14'] <- 1
data_train$V1A14[data_train$V1 != 'A14'] <- 0

data_train$V3A34[data_train$V3 == 'A34'] <- 1
data_train$V3A34[data_train$V3 != 'A34'] <- 0

data_train$V4A41[data_train$V4 == 'A41'] <- 1
data_train$V4A41[data_train$V4 != 'A41'] <- 0

data_train$V4A43[data_train$V4 == 'A43'] <- 1
data_train$V4A43[data_train$V4 != 'A43'] <- 0

data_train$V6A64[data_train$V6 == 'A64'] <- 1
data_train$V6A64[data_train$V6 != 'A64'] <- 0

data_train$V6A65[data_train$V6 == 'A65'] <- 1
data_train$V6A65[data_train$V6 != 'A65'] <- 0

data_train$V9A93[data_train$V9 == 'A93'] <- 1
data_train$V9A93[data_train$V9 != 'A93'] <- 0

data_train$V10A102[data_train$V10 == 'A102'] <- 1
data_train$V10A102[data_train$V10 != 'A102'] <- 0

data_train$V10A103[data_train$V10 == 'A103'] <- 1
data_train$V10A103[data_train$V10 != 'A103'] <- 0

data_train$V14A143[data_train$V14 == 'A143'] <- 1
data_train$V14A143[data_train$V14 != 'A143'] <- 0



#Start building the third model
reg <- glm(V21 ~ V1A14+V2+V3A34+V4A41+V4A43+V6A64+V6A65+V8+V9A93+V10A102+V10A103+V14A143, family=binomial(link='logit'), data = data_train)
summary(reg)

#Create validation variables
data_valid$V1A13[data_valid$V1 == 'A13'] <- 1
data_valid$V1A13[data_valid$V1 != 'A13'] <- 0

data_valid$V1A14[data_valid$V1 == 'A14'] <- 1
data_valid$V1A14[data_valid$V1 != 'A14'] <- 0

data_valid$V3A34[data_valid$V3 == 'A34'] <- 1
data_valid$V3A34[data_valid$V3 != 'A34'] <- 0

data_valid$V4A41[data_valid$V4 == 'A41'] <- 1
data_valid$V4A41[data_valid$V4 != 'A41'] <- 0

data_valid$V4A43[data_valid$V4 == 'A43'] <- 1
data_valid$V4A43[data_valid$V4 != 'A43'] <- 0

data_valid$V6A64[data_valid$V6 == 'A64'] <- 1
data_valid$V6A64[data_valid$V6 != 'A64'] <- 0

data_valid$V6A65[data_valid$V6 == 'A65'] <- 1
data_valid$V6A65[data_valid$V6 != 'A65'] <- 0

data_valid$V9A93[data_valid$V9 == 'A93'] <- 1
data_valid$V9A93[data_valid$V9 != 'A93'] <- 0

data_valid$V10A102[data_valid$V10 == 'A102'] <- 1
data_valid$V10A102[data_valid$V10 != 'A102'] <- 0

data_valid$V10A103[data_valid$V10 == 'A103'] <- 1
data_valid$V10A103[data_valid$V10 != 'A103'] <- 0

data_valid$V14A143[data_valid$V14 == 'A143'] <- 1
data_valid$V14A143[data_valid$V14 != 'A143'] <- 0


#Cretae predictions
y_hat <- predict(reg, data_valid, type="response")

#Create a threshold
y_hat_round <- as.integer (y_hat > .9)

#Analysis with confusion matrix
t <- table(y_hat_round, data_valid$V21)
t



