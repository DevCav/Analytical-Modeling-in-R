#____________________________________________HOMEWORK 5__________________________________________________________

#____________________________________Crime predictions with Linear Regression____________________________________

#Read in the data and create a matrix
data <- read.table("uscrime.txt", header = TRUE)
crime <- data.matrix(data, rownames.force = NA)

#Check the data
head(data)

#Visualize the data
pairs(data)

#Assign the response to the y variable
y <- crime[,16]
head(y)
  
#Assign the predictor to the x variables
x<- crime[,1:15]
head(x)

#Apply the lm function
relation <- lm(y~x)
print(relation)

#Look at a summary of the relationship
print(summary(relation))

#Based upon the summary I will drop columns and re-run the analysis
d_crime <- crime[,c(1,3,4,11,13,14)]
head(d_crime)

#re-run the linear regression
relation <- lm(y~d_crime)
print(relation)
print(summary(relation))

#Create a dataframe with the provided data to make a prediction on. (I'm only using some of the
#columns in my model) The first thing we create is a confidence interval.
relation <- lm(Crime ~ M + Ed + Po1 + U2 + Ineq + Prob, data = data)
print(relation)
predict(relation, data.frame(M=14, Ed=10, Po1=12, U2=3.6, Ineq=20.1, Prob=.04),interval="confidence")
##       fit      lwr      upr
##    1304.245 1180.741 1427.749

predict(relation, data.frame(M=14, Ed=10, Po1=12, U2=3.6, Ineq=20.1, Prob=.04))
