#### the Objective of our assignment is conduct exploraltory data analysis through data visualization 
#### by using heart failure data#######

heart = read.csv("heart_failure.csv")
str(heart)

table(heart$DEATH_EVENT)

#Performed some data manipulation to convert our integers into text form for some columns, 
#it would be more helpful for the purpose of visualizaion i.e. It will make our plots 
#more informative and readable 
heart$sex <- c("Female", "Male")[heart$sex+ 1]

heart$DEATH_EVENT <- c("No", "Yes") [heart$DEATH_EVENT + 1]

heart$anaemia <- c("No", "Yes") [heart$anaemia +1]

heart$high_blood_pressure <- c("No", "Yes") [heart$high_blood_pressure + 1]

heart$smoking <- c("No", "Yes") [heart$smoking +1]

heart$diabetes<- c("No", "Yes") [heart$diabetes +1]

install.packages("ggplot2")
library(ggplot2)


#we want to visualize distribution of our variables, 

H <- ggplot(heart, aes(x = age)) +
  geom_histogram()
H

# the distribution shows that age is normally distributed varaible.

#we passed the bin width argument to our code above, 
# the bin width of the variable will be 5 

H <- ggplot(heart, aes(x = age)) +
  geom_histogram(aes(y = ..count..), binwidth = 5)
H

#In our dataset, the variable ejection_fraction corresponds to heart's pumping power.According to research as people 
#age, the heart loses it pumping power. we want to investigate through our data set if there is a positive correlation
#between these two varaibles. 

#Plot a scatter plot between age and ejection_fraction


ggplot(data = heart) + 
  geom_point(mapping = aes(x = age, y = ejection_fraction ))

#Our plot above gave no clear pattern in visualization, meaning there is no correlation between variables
# age and ejection_fraction.


#lets see it more clearly by plotting a line between age and ejection_fraction, Is there an underlying trend for 
# these two variables if the data points are separated on the basis of gender.

# left
ggplot(data = heart) + 
  geom_point(mapping = aes(x = age, y = ejection_fraction))

# right
ggplot(data = heart) + 
  geom_smooth(mapping = aes(x = age, y = ejection_fraction))

#we can check side by side by plot for both male and female genders in order to see how the variable 
#ejection fraction correlates with age for both genders i.e. is there a difference between ejection fraction 
#indicator for male and female subjects

ggplot(data = heart) + 
  geom_point(mapping = aes(x = age, y = ejection_fraction)) + 
  facet_wrap(~ sex, nrow = 2)

#When variable 'sex" segregated the data points for age and ejection fraction, there was no difference seen 
#in trends when seen for plots of both the genders.


install.packages("vcd")
library(vcd)

#Now lets figure out it either more men died from heart failure or women 

mosaic(~ DEATH_EVENT + sex, data = heart)
#More men died from heart failure compared to women 

#Similary we want to see if there is a prevelence of high bood pressure, blood pressure or
#smoking habit among people who died 
#from heart failure
mosaic(~ DEATH_EVENT + high_blood_pressure, data = heart)


mosaic(~ DEATH_EVENT + diabetes, data = heart)

mosaic(~ DEATH_EVENT + smoking, data = heart)

#lets explore the relationship of time varaible with death_event 

boxplot(time ~ DEATH_EVENT, data = heart)

#These boxplots clearly show that people who spent more time in follow up routine with their 
#physicians, nursing care etc didnot die compared to those who did. The distributions are clearly
# different. the distribution for people who didn't die is skewed towards left while for people who died, its 
#skewed towards right

#Two variables are considered especially the predictor's of the heart failure, i.e. creatinine phosphokinase and 
#ejection fraction
#We want to see, how these variables compare in patients who died vs who didn't die

boxplot(creatinine_phosphokinase ~ DEATH_EVENT, data = heart)

#The distribution for variable creatinine phosphokinase are exactly similar for people who died of heart failure 
#versus the patients who did-not die. 

boxplot(ejection_fraction ~ DEATH_EVENT, data = heart)

#The distribution created in the plot show that people who die of heart failure have low ejection fraction 
#compared to those who did not.


#we want to compare the distribution of age for people who died versus people who didnot die 

boxplot(age ~ DEATH_EVENT, data = heart)

#People dying of heart failure have an average age around 65 compared to people who didnot die had a mean 
#age at 60. The people from age of 40 to 95 are included in the study but older people pass away more




