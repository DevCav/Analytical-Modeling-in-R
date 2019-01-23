#____________________________________________HOMEWORK 4__________________________________________________________

#______Use exponential smoothing to find the unofficial end of summer has gotten later over the 20 years.________

library(ggplot2)

#Read the data into a dataframe
temps <- read.table("temps.txt", header = TRUE)

#Convert the data into vectors
temps <- as.vector(unlist(temps[2:21]))

#Below we make a time series object from the data
time_series <- ts(temps, start = 1996, frequency = 123)

#Next we can plot the time series
plot(time_series)

#Experimenting with the different Holts functions
h1 <- HoltWinters(time_series)
plot(h1)
plot(h1$fitted)
h1$fitted[,4]

#export seasonality to a matrix
matrix <- matrix(h1$fitted[,4],ncol=123)


#import library to write data to excel
library(xlsx)

#export matrix to excel
write.xlsx(matrix, 'seasonalfactorCUSUM.xlsx')

