# Two #s will indicate R return values.
#---------------------------------- Question 4.2 -------------------------------------------------

#----------------------Create cluster on IRIS data set

#Load the data set and the relevant libraries
library(dplyr)
library(datasets)
library(ggplot2)
data(iris)

# do k-means clustering with 3 centers and exclude the species column 
iris %>% select(-Species) %>% kmeans(centers=3) -> km

#Running km will return each column and their cluster center. We also get the numbered cluster vector that the species
#belongs to
km                

## K-means clustering with 3 clusters of sizes 62, 50, 38

## Cluster means:
##   Sepal.Length Sepal.Width Petal.Length Petal.Width
## 1     5.901613    2.748387     4.393548    1.433871
## 2     5.006000    3.428000     1.462000    0.246000
## 3     6.850000    3.073684     5.742105    2.071053

## Clustering vector:
##  [1] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 1
## [53] 3 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 3 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 3 1 3 3
## [105] 3 3 1 3 3 3 3 3 3 1 1 3 3 3 3 1 3 1 3 1 3 3 1 1 3 3 3 3 3 1 3 3 3 3 1 3 3 3 1 3 3 3 1 3 3 1

## Within cluster sum of squares by cluster:
##   [1] 39.82097 15.15100 23.87947
## (between_SS / total_SS =  88.4 %)

## Available components:
  
##   [1] "cluster"      "centers"      "totss"        "withinss"     "tot.withinss" "betweenss"    "size"        
##   [8] "iter"         "ifault"   


#We can use the below code and see that the algorithm incorrectly classified 14 of the virginica and 2 of the setosa
table(km$cluster, iris$Species)
##      setosa versicolor virginica
## 1      0         48        14
## 2      0          2        36
## 3     50          0         0


#The data is varied in species the most between Petal Length and Petal Width so lets try that.
kmeans(iris[,3:4],centers=3) -> km_again

#Again we can look at the classification. We can see that we have more accurately predicted the specied now. We only 
# misclassifed 6 data points this time

table(km_again$cluster, iris$Species)
##  setosa versicolor virginica
## 1     50          0         0
## 2      0          2        46
## 3      0         48         4



#Plot the clusters that we generated along with the species. The color denotes the cluster and the shape denotes the 
#actual species. We can see that we are pretty accurate.
iris_cluster <- data.frame(iris, cluster=factor(km_again$cluster))
ggplot(iris_cluster, aes(x=Petal.Length, y=Petal.Width, color=cluster, shape=Species)) + geom_point()

##--Image 1 in PDF 

iris_numbered <- select(iris, -Species) # make a data table with only the numeric measurements from iris
summed <- (nrow(iris_numbered)-1)*sum(apply(iris_numbered,2,var))
for (i in 2:15) summed[i] <- sum(kmeans(iris_numbered,
                                     nstart=10,
                                     centers=i)$withinss)
summed_data <- data.frame( centers=1:15, summed)
ggplot(summed_data, aes(x=centers, y=summed)) + geom_point() + geom_line() +
  xlab("Number of Clusters") + ylab("Within groups sum of squares")

##--Image 2 in PDF 




