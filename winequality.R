#Assignment 03 
#Course 642
install.packages("class")
install.packages("gmodels")
library(class)
library(gmodels)

dataurl<-"http://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv"
wine<-read.csv(dataurl,header = TRUE,sep = ";")
str(wine)

wine$quality<-factor(wine$quality,ordered = T)
wine$rating <- ifelse(wine$quality < 5, 'low', ifelse(
   wine$quality < 7, 'medium', 'high'))
View(wine) 

which(is.na(wine))
boxplot(wine)


normalize <- function(x) {
   return ((x - min(x)) / (max(x) - min(x))) }
winen <- as.data.frame(lapply(wine[1:11], normalize))
View(winen)

wine_train <- winen[1:65,]
wine_test <- winen[66:100,]
wine_train_labels <- wine[1:65, 1]
wine_test_labels <- wine[66:100, 1]
#knn algorithm
wine_test_pred <- knn(train = wine_train, test = wine_test,cl = wine_train_labels , k=10)
#cross table function for 
CrossTable(x=wine_test_labels, y=wine_test_pred, prop.chisq=FALSE)
