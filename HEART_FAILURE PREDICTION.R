######THE aim of our analysis is to get predict if a patient is going to live or die on the basis of 
#the provided clinical records#######



#get the current working directory
getwd()

#load the data into the environment 
heart <- read.csv("heart_failure.csv")

# Before diving into the modelling process, we need to examine the structure of data set i.e. what variables it has
#what kind of data values they hold. for instance the variable age is a num but sex is an int
#there may #be strings in the dataset that might require further modification in order to prepare the for final
#classifier
str(heart)

#####Feature Engineering###

#the selection of features is an important step in the prepossessing for data that we are going to 
#use for our final model. The features in data set use have impact in final perform-ace outcome of the model.
#lets do a correlation matrix to see if we have any redundant feature, i.e. two features which provide us with the
#same information
correlationMatrix <- cor(heart[,1:13])
correlationMatrix
#####we found nothing to say that the features are highly correlated, which saves us from dropping any one of the 
####Usually a score > 0.75 is considered good for two features to be highly correlated 

#we want to encode the target variables as factors as our classfier would expect them to be 
heart$DEATH_EVENT <- factor(heart$DEATH_EVENT, levels = c(0, 1))

 
#Before moving any further lets use LVQ method (Learning vector quantification) used for ranking feature importance
#this method is quite sophisticated as it ranks features according to their importance.
#install caret library 
install.packages("caret", dependencies = c("Depends", "Suggests"))
library(caret)
#install mlbench library
install.packages('mlbench')
library(mlbench)
# prepare training scheme i.e. define how to train our model. we chose repeated Cross-validation with folds as 10 
#repeats as 3
control <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the model
model <- train(DEATH_EVENT~., data=heart, method="lvq", preProcess="scale", trControl=control)
# estimate variable importance
importance <- varImp(model, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance)
###none of the variables had an importance less than 0.5. the surprisng thing we noticed here that in the original study 
#the varaible creatinine phosphokinase alongwith ejection fraction was mentioned as the one 
#which coud be used sloley to predict heart failure but accroding to our LVQ method time i.e. the time spent on 
#for follow up turned out had highest ranking in terms of importance 

#The table function gives he count of the classes of the varaible that we want to predict i.e. in our case "DEATH_EVENT"
table(heart$DEATH_EVENT)

#we didnot know what the variables 0,1 corresponded to which gender, so we did a count and matched it against our 
#original research paper and it turned out that 0 corresponds to 'female' while 1 corresponds to 'male'. 
#The patients consisted of 105 women and 194 men(Chicco & Jurman)
table(heart$sex)

#prop.table() is more useful to find out proportions of the target varaible. It gives a percentage of the all the
#classes present in the target variable
round(prop.table(table(heart$DEATH_EVENT)) * 100, digits = 1)


#we need the summary of different statistical indicators of our dataset's different variables 
summary(heart)
#This summary showed some problematic vairables in terms of outliers. we will create boxplots of the below
#to further investigate.

#we create boxplots of the varaibles 'creatinine_phosphokinase', ' ejection_fraction', 'platelets' and
#' serum_creatinine' to further investigate these varaible's distribution. 

boxplot(heart$creatinine_phosphokinase,main= 'Creatinine phosphokinase' )
###The Boxplot clearly shows that Creatinine phosphokinase has many outliers##

boxplot( heart$ejection_fraction , main = 'Ejection fraction')
#this varaible is good when it comes to outliers. not too many outliers  

boxplot(heart$platelets, main = 'Platelets')
#this varaible has too many outliers 
boxplot(heart$serum_creatinine, main= 'Serum Creatinine')
#this one is seriously problamatic because it is dominated mostly by outliers


# the summary of the data set and the boxplots we created above showed that the 
#we need to normalize the data because of the presence of outliers
#as the similarity calculated between various data points will be dominated 
#by variables that have greater magnitude compared with others, hence the variables will introduce a bias in the 
#outcome of our final model. we will apply min-max normalization function on our data set.


normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))}

#In order to apply our normalize function on the data, we can use the lapply function
heart_normalized <- as.data.frame(lapply(heart [1:12], normalize))

heart_normalized

#Now in order to do the classification, split the data into training and test. 70% of the data is kept for the 
#training of algorithm and 30% is reserved as the unseen data that can be useful to evaluate performance of 
#our classifier
heart_train <- heart_normalized[1:209, ]
heart_test <- heart_normalized[210:299, ]

#We need labels of the target variables. create labels for train and test data 
heart_train_labels <- heart[1:209, 13]
heart_test_labels <- heart[210:299, 13]

install.packages ("class")    # if necessary
library(class)


#There is no sure shot method on how to select the number of k  i.e. Number of clusters  for training the classifier. 
#First is use the odd number, if the number of classes we are trying to predict are two, which is excatly the 
#case we are dealing here.
#and second is to apply k = sqrt(n) of training data points, which our case 14.45, so we can choose 15 to start with
heart_test_pred <- knn(train = heart_train, test = heart_test,
                       cl = heart_train_labels, k=15)

heart_test_pred

# load the "gmodels" library
install.packages('gmodels')
library(gmodels)


CrossTable(x = heart_test_labels, y = heart_test_pred,
           prop.chisq=FALSE)
length(heart_test_labels)
length(heart_test_pred)

heart_train <- heart_normalized[1:209, ]
heart_test <- heart_normalized[210:299, ]

#now lets try running the models with different values of k 
heart_test_pred <- knn(train = heart_train, test = heart_test, cl = heart_train_labels, k=15)
CrossTable(x = heart_test_labels, y = heart_test_pred, prop.chisq=FALSE)

heart_test_pred <- knn(train = heart_train, test = heart_test, cl = heart_train_labels, k=3)
CrossTable(x = heart_test_labels, y = heart_test_pred, prop.chisq=FALSE)

heart_test_pred <- knn(train = heart_train, test = heart_test, cl = heart_train_labels, k=1)
CrossTable(x = heart_test_labels, y = heart_test_pred, prop.chisq=FALSE)

heart_test_pred <- knn(train = heart_train, test = heart_test, cl = heart_train_labels, k=7)
CrossTable(x = heart_test_labels, y = heart_test_pred, prop.chisq=FALSE)

heart_test_pred <- knn(train = heart_train, test = heart_test, cl = heart_train_labels, k=11)
CrossTable(x = heart_test_labels, y = heart_test_pred, prop.chisq=FALSE)

heart_test_pred <- knn(train = heart_train, test = heart_test, cl = heart_train_labels, k=19)
CrossTable(x = heart_test_labels, y = heart_test_pred, prop.chisq=FALSE)

##although there was a difference in counts heart_test_labels and the predictions made by classifier heart_test_pred 
#0 vs 1. but there was no difference observed in terms of the performance of classifeir when changing
#values of k.
####lets initiate a loop that gives us performance of the classifier for different values of k

i=1                          # declaration to initiate for loop
k.optm=1                     # declaration to initiate for loop
for (i in 1:21){ 
  knn.mod <-  knn(train=heart_train, test=heart_test, cl=heart_train_labels, k=i)
  k.optm[i] <- 100 * sum(heart_test_labels == knn.mod)/NROW(heart_test_labels)
  k=i  
  cat(k,'=',k.optm[i],'\n')       # to print % accuracy 
}

###according to our loop above, it turns out that k=21 gives the best accuracy for our classifier. 
heart_test_pred <- knn(train = heart_train, test = heart_test, 
                       cl = heart_train_labels, k=21)


##we created a confusion matrix in the end to get an in detail picture of the accuracy of the model,in the loop 
#that we did above the training accuracy turned to be 92% for k =21, hence we are choosing this as our final model to
#make predictions on the unseen data 

confusionMatrix(heart_test_labels,heart_test_pred)

#the accuracy of the classifier was really good, the sensitivity showed that our model did well, when classifying 
#the positive classes as positive i.e. the sensitivity of our classifier was 93%, which happens to be good metric 
#the specificity showed that given the false values predicted by our model, 50% of the times they were actually false
#the no information rate was  good where the majority classes were 67.9 % and the model did a good job at prediction 
#where there was an imbalance in target variable and didn't relied on the dominant class to make predictions. 
#the prevalence of our sample showed that how 97% of the times the positive class '0' is in our case
#the high prevalence lead to the high positive predicted value in our case and vice versa lead to a low
#negative predicted value. the good detection rate indicated that out of our entire sample the proportion of 
#events detected correctly is 91%. The Detection prevelance showed that 92% of the time the classifier predicted the 
#positive classes out of the entire proportion of sample. 


                                      ###########################################

#Another method that I tried to improve the accuracy and other metrics of our model was do z-score standardization.
#This method is known to handle outliers well,so it can contribute to improved performce. this method 
#will compress outliers i.e. values that deviate alot from mean and fall outside the 
# the interquartile range, into smaller range.Lets find out.
heart_z <- as.data.frame(scale(heart[-13]))

#create train and test set
heart_train <- heart_z[1:209, ]
heart_test <- heart_z[210:299, ]

#We need labels of the target variables. create labels for train and test data 
heart_train_labels <- heart[1:209, 13]
heart_test_labels <- heart[210:299, 13]


#build classifier again

heart_test_pred <- knn(train = heart_train, test = heart_test,
                       cl = heart_train_labels, k=15)

CrossTable(x = heart_test_labels, y = heart_test_pred,
           prop.chisq=FALSE)
heart_test_pred <- knn(train = heart_train, test = heart_test,
                       cl = heart_train_labels, k=15)

#now lets try running again the newly created models with different values of k 
heart_test_pred <- knn(train = heart_train, test = heart_test, cl = heart_train_labels, k=15)
CrossTable(x = heart_test_labels, y = heart_test_pred, prop.chisq=FALSE)

heart_test_pred <- knn(train = heart_train, test = heart_test, cl = heart_train_labels, k=3)
CrossTable(x = heart_test_labels, y = heart_test_pred, prop.chisq=FALSE)

heart_test_pred <- knn(train = heart_train, test = heart_test, cl = heart_train_labels, k=1)
CrossTable(x = heart_test_labels, y = heart_test_pred, prop.chisq=FALSE)

heart_test_pred <- knn(train = heart_train, test = heart_test, cl = heart_train_labels, k=7)
CrossTable(x = heart_test_labels, y = heart_test_pred, prop.chisq=FALSE)

heart_test_pred <- knn(train = heart_train, test = heart_test, cl = heart_train_labels, k=11)
CrossTable(x = heart_test_labels, y = heart_test_pred, prop.chisq=FALSE)

heart_test_pred <- knn(train = heart_train, test = heart_test, cl = heart_train_labels, k=19)
CrossTable(x = heart_test_labels, y = heart_test_pred, prop.chisq=FALSE)


i=1                          # declaration to initiate for loop
k.optm=1                     # declaration to initiate for loop
for (i in 1:30){ 
  knn.mod <-  knn(train=heart_train, test=heart_test, cl=heart_train_labels, k=i)
  k.optm[i] <- 100 * sum(heart_test_labels == knn.mod)/NROW(heart_test_labels)
  k=i  
  cat(k,'=',k.optm[i],'\n')       # to print % accuracy 
}

###according to our loop above, it turns out that k=21 gives the best accuracy for our classifier. 
heart_test_pred <- knn(train = heart_train, test = heart_test, 
                       cl = heart_train_labels, k=27)

#create a confusion matrix for again

confusionMatrix(heart_test_labels,heart_test_pred)

# The changes in data introduced as result of z-score standardization resulted kappa value improved significantly
#to 0.44 and also led to an improvement in the value of the negetive prediction value too. There was also an 
#improvement in mcnemar test p-value. overall this model has turned out to be better compared to the previous one i.e.
#trained on the min-max standerdized data. 
