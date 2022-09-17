library(datasets)
head(iris)

library(ggplot2)
ggplot(iris, aes(Petal.Length, Petal.Width, color = Species)) + geom_point()
set.seed(101)

irisCluster <- kmeans(iris[, 1:4], 3, nstart = 20)
irisCluster

table(irisCluster$cluster, iris$Species)

#visualization 

library(cluster) 
clusplot(iris, irisCluster$cluster, color=TRUE, shade=TRUE, labels=0,lines=0, )
