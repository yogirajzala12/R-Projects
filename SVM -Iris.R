library(datasets)
data(iris)
View(iris)


head(iris, 5)
tail(iris, 5)

summary(iris)
summary(iris$Sepal.Length)

sum(is.na(iris))

install.packages("skimr")
library(skimr)


skim(iris) 

iris %>% 
  dplyr::group_by(Species) %>% 
  skim() 


plot(iris$Sepal.Width, iris$Sepal.Length, col = "red",     # Makes red circles + Adds x and y axis labels
     xlab = "Sepal width", ylab = "Sepal length")
hist(iris$Sepal.Width, col = "red") 

library(caret)
featurePlot(x = iris[,1:4], 
            y = iris$Species, 
            plot = "box",
            strip=strip.custom(par.strip.text=list(cex=.7)),
            scales = list(x = list(relation="free"), 
                          y = list(relation="free")))

set.seed(100)


TrainingIndex <- createDataPartition(iris$Species, p=0.8, list = FALSE)
TrainingSet <- iris[TrainingIndex,] # Training Set
TestingSet <- iris[-TrainingIndex,] # Test Set



Model <- train(Species ~ ., data = TrainingSet,
               method = "svmPoly",
               na.action = na.omit,
               preProcess=c("scale","center"),
               trControl= trainControl(method="none"),
               tuneGrid = data.frame(degree=1,scale=1,C=1)
)


Model.training <-predict(Model, TrainingSet)
Model.testing <-predict(Model, TestingSet) 

Model.training.confusion <-confusionMatrix(Model.training, TrainingSet$Species)
Model.testing.confusion <-confusionMatrix(Model.testing, TestingSet$Species)
