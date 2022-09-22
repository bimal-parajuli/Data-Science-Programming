
#
# Model Fitting


# 1. Read mtcars dataset from R and perform following model fitting techniques.
# (Any other datasets also applicable)




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
library(GGally)
library(VGAM)
library(stats4)
library(splines)
library(e1071)

x <- mtcars

str(x)

glimpse(x)

dim(x)






# a. Logistic Regression


# load the diabetes dataset


data(PimaIndiansDiabetes2)

# Handle missing values by Removing any records that may contain a NA. 
Diabetes <- na.omit(PimaIndiansDiabetes2)

# Observe the structure of the dataset and plan for further modeling.
dplyr::glimpse(Diabetes)





# Split the dataset into train and test in the ratio 8:2


split=sample.split(Diabetes,SplitRatio =0.8) 
Diabetes_train= subset(Diabetes,split==TRUE) 
Diabetes_test = subset(Diabetes,split==FALSE) 


dim(Diabetes)
dim(train_set)
dim(test_set)


# Train a logistic regression model
Diabetes_model <- glm (diabetes ~ ., data = train_set, family = "binomial") 



# Perform prediction and test the model on the testing data.

class_predicted <- predict(Diabetes_model, type = "response")


# computes the average prediction for each of the two outcomes

tapply(class_predicted, train_set$diabetes, mean)


# Predicted result.
summary(class_predicted)

# Build confusion matrix with a threshold value of 0.5

threshold_0.5 <- table(train_set$diabetes, class_predicted > 0.5)
threshold_0.5

# Accuracy
accuracy_0.5 <- round(sum(diag(threshold_0.5))/sum(threshold_0.5),2)
sprintf("Accuracy is %s",accuracy_0.5)

# Mis-classification error rate
MC_0.5 <- 1-accuracy_0.5
sprintf("Mis-classification error is %s",MC_0.5)

sensitivity0.5 <- round(118/(83+118),2)
specificity0.5 <- round(333/(333+42),2)
sprintf("Sensitivity at 0.5 threshold: %s", sensitivity0.5)
sprintf("Specificity at 0.5 threshold: %s", specificity0.5)

class(threshold_0.5)






















# b. Decision Tree


# Diabetes classification using the PIMA diabetes dataset.

library(mlbench) # Diabetes dataset
library(rpart) # Decision tree
library(rpart.plot) # Plotting decision tree
library(caret) # Accuracy estimation
library(Metrics) # For diferent model evaluation metrics



# load the diabetes dataset


data(PimaIndiansDiabetes2)

# Handle missing values by Removing any records that may contain a NA. 
Diabetes <- na.omit(PimaIndiansDiabetes2)

# Observe the structure of the dataset and plan for further modeling.
dplyr::glimpse(Diabetes)


# Split the dataset into train and test in the ratio 8:2
split=sample.split(Diabetes,SplitRatio =0.8) 
Diabetes_train= subset(Diabetes,split==TRUE) 
Diabetes_test = subset(Diabetes,split==FALSE) 


dim(Diabetes)
dim(train_set)
dim(test_set)


# Train a decision tree model
Diabetes_model <- rpart(formula = diabetes ~., 
                        data = Diabetes_train, 
                        method = "class")


# Plot the decision Tree graphically.
rpart.plot(x = Diabetes_model, yesno = 2, type = 0, extra = 0)



# Perform prediction and test the model on the testing data.
class_predicted <- predict(object = Diabetes_model,  
                           newdata = Diabetes_test,   
                           type = "class")


# Predicted result.
class_predicted



# Confusion matrix and Sensitivity, Specificity, Precision, Recall, F1 Score
confusionMatrix(data = class_predicted, reference = Diabetes_test$diabetes)$byClass



# Confusion matrix and Accuracy, Kappa statistics
confusionMatrix(class_predicted, Diabetes_test$diabetes, mode = "prec_recall")$overall























# c. Naïve Bayes




# load the diabetes dataset


data(PimaIndiansDiabetes2)

# Handle missing values by Removing any records that may contain a NA. 
Diabetes <- na.omit(PimaIndiansDiabetes2)

# Observe the structure of the dataset and plan for further modeling.
dplyr::glimpse(Diabetes)


# Split the dataset into train and test in the ratio 8:2
split=sample.split(Diabetes,SplitRatio =0.8) 
Diabetes_train= subset(Diabetes,split==TRUE) 
Diabetes_test = subset(Diabetes,split==FALSE) 


dim(Diabetes)
dim(train_set)
dim(test_set)


# e1071 library needed.

# Train a decision tree model
bayes_model <- naiveBayes(diabetes ~ ., data = train_set) 

bayes_model


# Perform prediction and test the model on the testing data.
class_predicted <- predict(object = bayes_model,  
                           newdata = Diabetes_test,   
                           type = "class")


# Predicted result.
class_predicted


# Confusion matrix and Sensitivity, Specificity, Precision, Recall, F1 Score
confusionMatrix(data = class_predicted, reference = Diabetes_test$diabetes)$byClass
# Confusion matrix and Accuracy, Kappa statistics
confusionMatrix(class_predicted, Diabetes_test$diabetes, mode = "prec_recall")$overall




# d. SVM













# e. Random forest

















# 2. Compare each of the above models using the following parameters
# a. Accuracy


# b. Precision


# c. Recall


# d. Sensitivity


# e. Specificity







# 3. Perform k-means clustering in IRIS dataset.

# Loading the necessary dataset:

library(datasets)
library(caTools)
library(superml)
library(dplyr)
library(ggplot2)

# Loading the data.
iris = datasets::iris
head(iris)


#handle null values if present
sum(is.na(iris))
#no such values are present

refinedData <- dplyr::select(iris, -5)
head(refinedData)

# split data into train and test
splits = sample.split(refinedData[, 1], SplitRatio = 0.7)
train <- refinedData[splits == TRUE, ]
test <- refinedData[splits == FALSE, ]


# since iris data has three distinct species
k = 3

kModel <- kmeans(refinedData, centers = 3, nstart = 20)
kModel$centers

#plotting our clusters
library(factoextra)

fviz_cluster(kModel, refinedData,
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)

























# 4. Perform Hierarchical clustering in mtcars dataset.

# Load necessary libraries.

library(datasets)
library(caTools)
library(superml)
library(corrplot)
dev.new()


# Load the dataset.
mtcars = datasets::mtcars

# View the dataset.
head(mtcars)

#handle null values if present
sum(is.na(mtcars))


#create a hierarchical cluster
clusterObj = hclust(dist(mtcars), method="average")
plot(clusterObj,
     main = paste('Cluster of cars'),
     xlab = 'Cards',
     ylab = 'Euclidean distances')



clusterObj


#fitting the cluster
fittedCluster = cutree(clusterObj, h = 5)
str(fittedCluster)

# visualize the cluster
library(cluster)

clusplot(mtcars, fittedCluster, lines = 0, color = TRUE, shade =TRUE, span = TRUE,
         labels = 2,
         main = "Clusters of mtcars")





