# 1. Perform Label encoding on IRIS Dataset 


# Using the base R.

data <- iris
data$label = as.numeric(factor(data$Species))

summary(data)




######Using the CatEncoders Package

library('CatEncoders')

data2 = iris

#   Define original Catrgorical Labels
labs = LabelEncoder.fit(data2$Species)

#   Convert the labels to numeric values
data2$label2 = transform(labs, data$Species)

summary(data2)







# 2. Perform One-hot encoding on IRIS Dataset 


#Using dummyvars() function in the caret package.

library(caret)

data3 <- iris

#define the one-hot encoding function

dummy <- dummyVars(" ~ .", df=data3)


#




# 3. Feature scaling or standardization 
# a. Normalization 







# b. Z-scale 







# 4. Find the principal components of IRIS dataset

  ### Using prcomp()


str(iris)


# Create a 2 * 2 graphical layout tofor drawing 4 charts. 
par(mfrow = c(2, 2))

# Create the histogram chart for all four attributes.
hist(iris$Sepal.Length, breaks = 20)
hist(iris$Sepal.Width, breaks = 20)

hist(iris$Petal.Length, breaks = 20)
hist(iris$Petal.Width, breaks = 20)



# Perform a log transform on the first four feature columns.
log.iris <- log(iris[, 1:4])

# Extract the label column. i.e. 5th column.
iris.species <- iris[, 5]


# Create histograms of the log transformed features.
par(mfrow = c(2, 2))

hist(log.iris$Sepal.Length, breaks = 20)
hist(log.iris$Sepal.Width, breaks = 20)

hist(log.iris$Petal.Length, breaks = 20)
hist(log.iris$Petal.Width, breaks = 20)

# Perform principle component analysis of the log transformed 4 feature vectors.
iris.pca <- prcomp(log.iris, center=TRUE, scale = TRUE)
print(iris.pca)

# Plot and visualize the principle components. 
fviz_eig(iris.pca, type = 'lines')

# We can observe that the first principle component is the most significant.

# Summary of Principle components.
summary(ir.pca)





  ### By diagonalizing the covariance matrix:

iris.mat <- as.matrix(log.iris)
cov.mat <- cor(iris.mat)

eigen_mat <- eigen(cov.mat)

eigen_mat



# Here, it is observable that the results obtained form both the above methods are same. 
# Hence, the Principle component analysis of the IRIS data set is performed.





# 5. House rent prediction using linear regression 


library(readr)
library(ggplot2)
library(corrplot)
library(mlbench)
library(Amelia)
library(plotly)
library(reshape2)
library(caret)
library(caTools)
library(dplyr)


library('MASS')
library('ISLR')

summary(BostonHousing)


# housing_dataset <- BostonHousing
# 
# str(housing_dataset)
# 
# summary(housing_dataset)
# 
# head(housing_dataset)
# 
# # View if there is any missing data in the datase using missmap.
# missmap(housing_dataset,col=c('white','black'),y.at=1,y.labels='',legend=TRUE)
# 
# # Draw a correlation plot of all the features in the dataset.
# corrplot(cor(select(housing_dataset, -chas)))



data <- BostonHousing

summary(data)

head(data)

plot(medv~lstat, data)


# It can be noted that lstat and medv are related.


# =========================================

# Packages Used: mlbench and caTools
# install.packages("mlbench") 
# install.packages('caTools') 

#Read Dataset 
library(mlbench) 
data(BostonHousing) 
x=BostonHousing 
str(x) 
names(x) 


#Check NA in dataset 
sum(is.na(x)) 



library(caTools) 
split=sample.split(x,SplitRatio =0.7) 
train_set = subset(x,split==TRUE) 
test_set = subset(x,split==FALSE) 

dim(train_set)
dim(test_set)


# The General Linear regression model in R :
# Univariate Model : model<−lm(y∼x,data)
# Multivariate Model : model<−lm(y∼.,data)
# medv is the target variable, predicted using crim,rm,tax,lstat


model=lm(medv ~ crim + rm + tax + lstat , data = train) 
summary(model) 


#Prediction 
test$predicted_val= predict(model,test) 
print(test$medv) 
print(test$predicted_val) 



# Error and rmse 
error=test$medv-test$predicted.medv 
rmse=sqrt(mean(error)^2) 
cat("RMSE",rmse)




# 6. Medical diagnosis for disease spread pattern Using SVM








# New packages used: e1071, superml
# install.packages('e1071') 


x=read.csv("D:\\Academics\\VIT_Academics\\5th_sem\\Prog for DS\\LAB\\Feature_engineering_and_Model_fitting\\Cancer_Data.csv") 

names(x) 
x=x[-c(1,33)] 

#Check NA in dataset 
sum(is.na(x)) 
colSums(is.na(x)) 

#Label Encoder 
library(superml) 
label=LabelEncoder$new() 
x$diagnosis=label$fit_transform(x$diagnosis) 
head(x) 

#Train-test split 
library(caTools) 
split=sample.split(x$diagnosis,SplitRatio =0.7) 
train=subset(x,split==TRUE) 
test=subset(x,split==FALSE) 

#SVM 
install.packages('e1071') 
library(e1071) 
train[-1]=scale(train[-1]) 
test[-1]=scale(test[-1]) 
names(train) 
classifier = svm(formula = diagnosis ~ ., 
                 data = train, 
                 type = 'C-classification', 
                 kernel = 'linear') 



#Prediction 
Diag_pred = predict(classifier, newdata = test[-1]) 


# Making the Confusion Matrix 
cm = table(test[,1], Diag_pred) 
print(cm)
