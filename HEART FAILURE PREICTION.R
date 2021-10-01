#######The objective of this assignment is to predict the death by heart failure of a patient on the basis of provided 
#clinical data of patient##########


#read the dataset into the R environment 
heart <- read.csv("heart_failure.csv")


#See the structure of the data set, what variables it has and what corresponding values they contain
str(heart)

#the model will expect our target variable to be encoded as a factor, so we use the as.factor for this purpose
heart$DEATH_EVENT<-as.factor(heart$DEATH_EVENT)

#we want to look at the statistical characteristics of the variables like mean, min, max 
summary(heart)

###Code to remove locks from the user directory for package installation
options("install.lock"=FALSE)



#split the dataset into training and test set. we are doing a 70,30 split on or data set i.e. 70% data is for training
#and the remaining 30% is saved for testing the accuracy of our model


heart_train <- heart[1:209,]
heart_test <- heart[210:299, ]


# Since we are applying decision tree (a non parametric model to our dataset, we really dont need the standerdization for the
#data set

# Create our model using rpart
install.packages("rpart")
install.packages("rpart.plot")

library(rpart)
library(rpart.plot)

#create decision tree on our training data that we obtained by the splitting of our observations above
fit<- rpart (DEATH_EVENT ~., data = heart_train, method = 'class')

#An attractive feature of decision trees lies in the explainability of this model, it simply can tell which features
#were important for determining the target variable
rpart.plot(fit)

#We skipped the feature selection step in the beginning as decision trees stand out when it comes to non-parametric models 
#as it makes it decision to make prediction on the importance of features itself i.e. by finding which feature
#leads to less impurity in the successive nodes

#using the model trained on the dataset to make predictions on the unseen data i.e. our test data. As we want to see
#how well our model will do, when it comes across something that it hasn't seen before 
predict_unseen <-predict(fit, heart_test, type = 'class')

#Create a matrix to compare our model's predictions results with actual observations of target variable
table_mat <- table(heart_test$DEATH_EVENT, predict_unseen)
table_mat

#We will get a number of how well the model did 
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)

print(paste('Accuracy for test', accuracy_Test))



accuracy_tune <- function(fit) {
  predict_unseen <- predict(fit, heart_test, type = 'class')
  table_mat <- table(heart_test$DEATH_EVENT, predict_unseen)
  accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
  accuracy_Test
}

#create a variable called control that contains the hyperparameters of our model 
control <- rpart.control(minsplit =4,
                         minbucket = 3,
                         maxdepth = 3,
                         cp = 0)

#Fit a new model with "control" 
tune_fit <- rpart(DEATH_EVENT ~. , data = heart_train, method = 'class', control = control)

#Lets see how our results have changed after fiddling with hyperparamters of our model
accuracy_tune(tune_fit)


#controlling for hyperparameter in our model above lead to an improvement in the accuracy of the model by a good number.
#but lets not stop here, we need a confusion matrix to see if the model's predictions stand correct for all observations

#Lets create the confusion matrix to see how well our model performed by using other performing metrics 



install.packages('caret')

#Import required library
library(caret)

confusionMatrix(heart_test$DEATH_EVENT, predict_unseen)

#the confusion matrix gave us more insights about how well model performed, and the kappa value is not looking good which 
#indicates that there is low agreement between our predicted and actual classes
#our information rate is high that our model is mostly relying majority classes to make predictions.
#This can be further confirmed by looking at the p-value and prevalence.

####We will try to shuffle our dataset, so that we can get rid of all classes lumped together in one place and have more equal
#distribution of classes in both train and test set. if we dont do that, the algorithm will
#mostly see one prevalent class##################################



#First, create an index of observations 
shuffle_index <- sample(1:nrow(heart))

# Use the index to shuffle the observations 
heart <- heart[shuffle_index, ]

#testing if the shuffling has worked or not 
head(heart)

#Lets check the tail of the dataset if the shuffling has really worked.

tail(heart)
#we can confirm by our head function applied on the heart data set that now there more shuffling of classes in the data set

heart_train1 <- heart[1:209,]
heart_test1 <- heart[210:299, ]


#create decision tree on our training data that we obtained by the splitting of our observations above
fit<- rpart (DEATH_EVENT ~., data = heart_train1, method = 'class')

#An attractive feature of decision trees lies in the explainability of this model, it simply can tell which features
#were important for determining the target variable
rpart.plot(fit)

#We skipped the feature selection step in the beginning as decision trees stand out when it comes to non-parametric models 
#as it makes it decision to make prediction on the importance of features itself i.e. by finding which feature
#leads to less impurity in the successive nodes

#using the model trained on the dataset to make predictions on the unseen data i.e. our test data. As we want to see
#how well our model will do, when it comes across something that it hasn't seen before 
predict_unseen <-predict(fit, heart_test1, type = 'class')

#Create a matrix to compare our model's predictions results with actual observations of target variable
table_mat <- table(heart_test1$DEATH_EVENT, predict_unseen)
table_mat

#We will get a number of how well the model did 
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)

print(paste('Accuracy for test', accuracy_Test))

#accuracy has improved for our model after shuffling 


accuracy_tune <- function(fit) {
  predict_unseen <- predict(fit, heart_test1, type = 'class')
  table_mat <- table(heart_test1$DEATH_EVENT, predict_unseen)
  accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
  accuracy_Test
}

#create a variable called control that contains the hyperparameters of our model 
control <- rpart.control(minsplit =4,
                         minbucket = 3,
                         maxdepth = 3,
                         cp = 0)

#Fit a new model with "control" 
tune_fit <- rpart(DEATH_EVENT ~. , data = heart_train1, method = 'class', control = control)

#Lets see how our results have changed after fiddling with hyperparamters of our model
accuracy_tune(tune_fit)


#controlling for hyperparameters in our model above lead to an improvement in the accuracy of the model by a good number.
#but lets not stop here, we need a confusion matrix to see if the model's predictions stand correct for all observations

#Lets create the confusion matrix to see how well our model performed by using other performing metrics 

confusionMatrix(heart_test1$DEATH_EVENT, predict_unseen)
#the shuffling of our observation lead to an improvement of our performance indicators as shown below####


#shuffling the data set helped us to get rid of our problem of having a high information rate i.e. classifier mostly relying 
#on majority class to make predictions and a decrease in p-value and increase in specificity also points towards the same.  


# Now lets try random sampling 
train_sample <- sample(299, 209)

str(train_sample)

# split the data frames
heart_train2 <- heart[train_sample, ]
heart_test2  <- heart[-train_sample, ]   


#create decision tree on our training data that we obtained by the splitting of our observations above
fit<- rpart (DEATH_EVENT ~., data = heart_train2, method = 'class')

#An attractive feature of decision trees lies in the explainability of this model, it simply can tell which features
#were important for determining the target variable
rpart.plot(fit)

#We skipped the feature selection step in the beginning as decision trees stand out when it comes to non-parametric models 
#as it makes it decision to make prediction on the importance of features itself i.e. by finding which feature
#leads to less impurity in the successive nodes

#using the model trained on the dataset to make predictions on the unseen data i.e. our test data. As we want to see
#how well our model will do, when it comes across something that it hasn't seen before 
predict_unseen <-predict(fit, heart_test2, type = 'class')

#Create a matrix to compare our model's predictions results with actual observations of target variable
table_mat <- table(heart_test2$DEATH_EVENT, predict_unseen)
table_mat

#We will get a number of how well the model did 
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)

print(paste('Accuracy for test', accuracy_Test))

#accuracy has improved for our model after shuffling 


accuracy_tune <- function(fit) {
  predict_unseen <- predict(fit, heart_test2, type = 'class')
  table_mat <- table(heart_test2$DEATH_EVENT, predict_unseen)
  accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
  accuracy_Test
}

#create a variable called control that contains the hyperparameters of our model 
control <- rpart.control(minsplit =4,
                         minbucket = 3,
                         maxdepth = 3,
                         cp = 0)

#Fit a new model with "control" 
tune_fit <- rpart(DEATH_EVENT ~. , data = heart_train2, method = 'class', control = control)

#Lets see how our results have changed after fiddling with hyperparamters of our model
accuracy_tune(tune_fit)


#controlling for hyperparameters in our model above lead to an improvement in the accuracy of the model by a good number.
#but lets not stop here, we need a confusion matrix to see if the model's predictions stand correct for all observations

#Lets create the confusion matrix to see how well our model performed by using other performing metrics 

confusionMatrix(heart_test2$DEATH_EVENT, predict_unseen)
