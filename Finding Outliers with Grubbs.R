#_________________________________________HOMEWORK 3__________________________________________________________

#_______________________Find Outliers using grubbs.test in R outliers package___________________________________

library(ggplot2)
library(outliers)

#Read in my data
dataf <- read.table(file="C://Users//dcavagnaro//Documents//Training//GT Analytics//Intro to Modeling//HW3//uscrime.txt", sep="\t", quote="", comment.char="", header=TRUE)

#Beause the grubbs.test assumes a normal distribution we will first check that the last column is normally distributed. After
#running the code below, we can see that the data is skewed a little to the left but I'm going to assume this is okay for the 
#purpose of the class.
ggplot(dataf, aes(x=Crime)) +
  geom_histogram(binwidth=100, colour="black", fill="white")


#For this portion of the homeowork, I'm going to create a function that labels the data points as ooutliers. 
#This will allow me to create a histogram with the data points still showing but marked in a different color.
#pv is the p-value and this function will use a p-value of 10
X <- dataf[['Crime']]

grubbs.flag <- function(x) {
  outliers <- NULL
  test <- x
  grubbs.result <- grubbs.test(test)
  pv <- grubbs.result$p.value
  while(pv < 0.10) {
    outliers <- c(outliers,as.numeric(strsplit(grubbs.result$alternative," ")[[1]][3]))
    test <- x[!x %in% outliers]
    grubbs.result <- grubbs.test(test)
    pv <- grubbs.result$p.value
  }
  return(data.frame(X=x,Outlier=(x %in% outliers)))
}

#The following code plots the histogram with the outliers marked in a different color. We can continue to change the p-value
#to change the outliers that are different colors. 

p <- ggplot(grubbs.flag(X),aes(x=X,color=Outlier,fill=Outlier))+
  geom_histogram(binwidth=diff(range(X))/30)+
  theme_bw()
p + labs(x = 'Crime')

