#K nearest neighbours 
library(ISLR)
library(caTools)
library(ggplot2)
iris <- read.csv('https://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data')
head(iris)

#Data cleaning 
names(iris) <- c('Sepal length','Sepal width', 'Petal length','Petal Width','Species')
sum(is.na(iris))
str(iris)
summary(iris)

#standardize and normalizing the variables. This is not a necessary step for this data
#The variables are occuring in a similar scale.
#it can be done in 3 different ways

standardized.iris <- scale(iris[1:4])

#you can use the variance to check if your standardization worked
var(standardized.iris[,1])
var(standardized.iris[,2])

#OR

#Normalization
#normalize <- function(x) {
#  return ((x - min(x)) / (max(x) - min(x))) }
#standardized.iris <- as.data.frame(lapply(iris[,1:4], normalize))


#OR 

#Species <- Caravan[,5]
#standardized.iris <- scale(iris[,-5])

#Getting our test/train data 
#I will set a seed to reduce variability and for easy replication 


final.data <- cbind(standardized.iris,iris[5])

set.seed(101)

sample <- sample.split(final.data$Species, SplitRatio = .70)
train <- subset(final.data, sample == TRUE)
test <- subset(final.data, sample == FALSE)

#KNN Machine Learning model

install.packages('class')
library(class)

predicted.species <- knn(train[1:4],test[1:4],train$Species,k=1)
predicted.species
mean(test$Species != predicted.species)

predicted.species8 <- knn(train[1:4],test[1:4],train$Species,k=8)
predicted.species8
mean(test$Species != predicted.species8)

#or we can use the square of the number of observations of the independent varibles

#NROW(iris[1:4,])
#square root of 149 is 12.07. So we can use K = 12 and K = 13

predicted.species <- NULL
error.rate <- NULL

for(i in 1:10){
  set.seed(101)
  predicted.species <- knn(train[1:4],test[1:4],train$Species,k=i)
  error.rate[i] <- mean(test$Species != predicted.species)
}

library(ggplot2)
k.values <- 1:10
error.df <- data.frame(error.rate,k.values)
pl <- ggplot(error.df,aes(x=k.values,y=error.rate)) + geom_point()
pl + geom_line(lty="dotted",color='red')

#check for accuracy 
library(caret)

#you also have to pick a K value
#K is lowest at 6 


table(test$Species)
#use the if else function to assign values to each character
# 1 = Iris-setosa 2 = Iris-versicolor 3 = Iris-virginica


tablemod <- table(test$Species, test$predicted.species)