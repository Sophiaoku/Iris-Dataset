library(ISLR)
library(caTools)
df <- iris
head(df)
#install.packages('e1071',repos = 'http://cran.us.r-project.org')
library(e1071)

#df1 <- subset(df, select = -c(Sepal.Length,Sepal.Width) )

#standardized.iris <- scale(iris[1:4])
#final.data <- cbind(standardized.iris,iris[5])

set.seed(101)

sample <- sample.split(df$Species, SplitRatio = .70)
train <- subset(df, sample == TRUE)
test <- subset(df, sample == FALSE)


model <- svm(Species ~ ., data=train, kernel = 'radial', scale = FALSE, cost = 0.2)
summary(model)

#plot(model,train)

predicted.values <- predict(model,test[,-5])
table(predicted.values,test$Species)

#tume
tune.results <- tune(svm,train.x=train[1:4],train.y=train[,5],kernel='radial',ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)))
summary(tune.results)

#- best parameters:
#cost gamma
#1   0.5

tuned.svm <- svm(Species ~ ., data=train, kernel="radial", cost=1, gamma=0.5)
summary(tuned.svm)

tuned.predicted.values <- predict(tuned.svm,test[1:4])
table(tuned.predicted.values,test[,5])
