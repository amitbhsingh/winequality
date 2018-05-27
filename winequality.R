# Analysing the fo
#Data Set Information:

#The two datasets are related to red and white variants of the Portuguese "Vinho Verde" wine. For more details, consult: [Web Link] or the reference [Cortez et al., 2009]. Due to privacy and logistic issues, only physicochemical (inputs) and sensory (the output) variables are available (e.g. there is no data about grape types, wine brand, wine selling price, etc.). 

#These datasets can be viewed as classification or regression tasks. The classes are ordered and not balanced (e.g. there are munch more normal wines than excellent or poor ones). Outlier detection algorithms could be used to detect the few excellent or poor wines. Also, we are not sure if all input variables are relevant. So it could be interesting to test feature selection methods. 

# installing the packages required 
install.packages("class")
install.packages("gmodels")
library(class)
library(gmodels)
# importing data 
dataurl<-"http://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv"
wine<-read.csv(dataurl,header = TRUE,sep = ";")
str(wine)

wine$quality<-factor(wine$quality,ordered = T)
wine$rating <- ifelse(wine$quality < 5, 'low', ifelse(
   wine$quality < 7, 'medium', 'high'))
View(wine) 

which(is.na(wine))
boxplot(wine)

#normalizing data 
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
