#____________________________________________HOMEWORK 8__________________________________________________________

#_______________________________________ Variable Selection _____________________________________________________


####################################### Stepwise Regression #####################################################
library(DAAG)
library(caret)
library(sme)

#Clean up the workspace
rm(list=ls())


#Read in the dataset
data <- read.table(file="uscrime.txt", stringsAsFactors = FALSE, header = TRUE)


# Set up repeated k-fold cross-validation
ctrl <- trainControl(method = "cv", number = 10)

# Train the model
step.model <- train(Crime ~., data = data,
                    method = "leapSeq", 
                    tuneGrid = data.frame(nvmax = 1:10),
                    trControl = ctrl)
step.model$results
step.model$bestTune


#Create model with best factors
best_step_cv <-train(Crime ~ M + Ed + Po1 + U2 + Ineq + Prob, data = data, method = "lm", trControl = ctrl, metric="RSquared")
#Coefficients of the best model
coef(best_step_cv$finalModel)

AIC_1 <- AIC(lm(Crime ~ M + Ed + Po1 + U2 + Ineq + Prob, data = data))
AIC_1
AICc_1 <- AICc(lm(Crime ~ M + Ed + Po1 + U2 + Ineq + Prob, data = data))
AICc_1
BIC_1 <- BIC(lm(Crime ~ M + Ed + Po1 + U2 + Ineq + Prob, data = data))
BIC_1


####################################### LASSO #####################################################
library(glmnet)
library(ggplot2)
#Assign the response to the y variable
y <- data[c('M','Crime')]
head(y)
#Assign the predictor to the x variables
x<- (data[,-2])
x<- (x[,-15])
x<- scale(x)
head(x)

scaled_data <- as.data.frame(cbind(x, y))

model_lasso <- cv.glmnet(x = as.matrix(scaled_data[,-16]),
                         y = as.matrix(scaled_data[,16]),
                         alpha = 1,
                         nfolds = 10, 
                         type.measure = 'mse',
                         family = 'gaussian',
                         standardize = FALSE)

xmin <- min(model_lasso$lambda)
xmax <- max(model_lasso$lambda)
ymin <- min(model_lasso$cvm)
ymax <- max(model_lasso$cvm)

plot(x = model_lasso$lambda, y = model_lasso$cvm, xlim=c(xmin, xmax), ylim=c(ymin, ymax))
abline(v = model_lasso$lambda.min, col = 'Red', lty = 2)
model_lasso$lambda.min           
dev.off()

plot(x = model_lasso$lambda, y = model_lasso$nzero)
abline(v = model_lasso$lambda.min, col = 'Red', lty = 2)


#Print the coefficients fort the model
coef(model_lasso, s=model_lasso$lambda.min)

#Rerun the regression with the factors that LASSO selected
model_lasso_cv <-train(Crime ~ Ed + Po1 + M.F + NW + U2 + Ineq + Prob, data = data, method = "lm", trControl = ctrl, metric="RSquared")
#Summary of the model
summary(model_lasso_cv)
#Coefficients of the best model
coef(model_lasso_cv$finalModel)

AIC_2 <- AIC(lm(Crime ~ Ed + Po1 + M.F + NW + U2 + Ineq + Prob, data = data))
AIC_2
AICc_2 <- AICc(lm(Crime ~ Ed + Po1 + M.F + NW + U2 + Ineq + Prob, data = data))
AICc_2
BIC_2 <- BIC(lm(Crime ~ Ed + Po1 + M.F + NW + U2 + Ineq + Prob, data = data))
BIC_2

####################################### Elastic Net #####################################################


min_lambda <- rep(1, 10)
min_error <- rep(1, 10)
for (i in 1:11) {
  div <- i / 10
  model_elastic_net <- cv.glmnet(x = as.matrix(scaled_data[,-16]),
                  y = as.matrix(scaled_data[,16]),
                  alpha = div,
                  nfolds = 10, 
                  type.measure = 'mse',
                  family = 'gaussian',
                  standardize = FALSE)
  stats <- cbind(model_elastic_net$lambda, model_elastic_net$cvm)
  min_error[i] = min(stats[,2])
  min_lambda[i] = stats
  }

results <- cbind(min_error, min_lambda)


model_elastic_net <- cv.glmnet(x = as.matrix(scaled_data[,-16]),
                               y = as.matrix(scaled_data[,16]),
                               alpha = 1,
                               nfolds = 10, 
                               type.measure = 'mse',
                               family = 'gaussian',
                               standardize = FALSE)


model_elastic_net 











