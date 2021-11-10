#####################################################################
####################### Classificaion Case Study ###################
#####################################################################

#In case of a classification problem:  the target variable is a categorical in  nature.
#We'll be doing Logistic Regression and Classification decision tree. 


# Step-1
# Identify the Problem Statement, What are you trying to solve?
#here the ask is:
#"if the titanic ship is once again built, and people are boarded, and the
#journey starts again, then can you help us identify whether the person whom we
#are boarding will survive another crash or not?"


# Step-2
# Identify the Target variable in the data, What value will be predicted?
##spot this "Survived" in the data, might not be present with this name (e.g. Surv_x, sv_x)
#make sure you have the correct target variable

# Step-3
#Loading the raw Data

TitanicData=read.csv('F:/stat+r ajanta maam/TitanicSurvivalData.csv',na.strings=c(""," ","NA","NULL"),stringsAsFactors = T)
View(TitanicData)
dim(TitanicData)
class(TitanicData)
names(TitanicData)


# Step-4
# Exploring the dataset
#gives the descriptive statistics of the data
#what kind of variables are there?
str(TitanicData)
head(TitanicData,10)

#####passenger ID####
#everytime a new passenger comes its ID will be something different. 
#A person having certain ID has no relation to whether that person will 
#survive the crash or not
##if you include this column then ML algo might find some patterns/relations and give
#importance to ID also- that will be a useless model from the business perspective

##ALWAYS THINK FROM THE BUSINESS PERSPECTIVE
#The input I am choosing is that making sense? If no, then reject. 

#####Pclass#####
#People travelling in 1st class might have access to life boats/jackets - they have 
#higher chance of survival.

#####Name#####
#same concept like PassengerID
##you can't say whether a person will survive or not, based on his/her name.

#####Sex#####
#In the movie also, we saw who was over the plank. Here, gender is a very 
#important variable and we'll see in our data exploration also why it's important. 

#####Age#####
#It is directly related to the swimming capacity or harsh condition tolerance

#####ticket#####
##ticket no. has not relation will survival


#####Fare#####
##people paying higher fares might have better access to different facilities in the ship
#and that can increase their chances of survival

#####cabin#####
##in which cabin the people are travelling
##lot of missing values
summary(TitanicData)

##529 missing values out of 714 variables- so we don't choose that column


#####Embarked#####
##docking area where a passenger boards the ship
#if the person didn't board the ship before it crashed, then he/she will
#survive

# Removing useless columns in the data, and explore the rest
UselessColumns=c('PassengerId', 'Name', 'Cabin', 'Ticket')
TitanicData[, UselessColumns]=NULL

head(TitanicData)

str(TitanicData)


###check if all the categorical variables are factor or not

factor_cols=c("Survived","Pclass")

for (cat_cols in factor_cols){
  TitanicData[ , cat_cols]=as.factor(TitanicData[ , cat_cols])
}

str(TitanicData)
############################################################

# Step-5
# Whether it is a Regression problem or Classification?

#target variable - survive or not survive
##survival:Yes/1 or Survival:No/0


# Step-6
# Checking and treating missing values

# Checking missing values
colSums(is.na(TitanicData))

table(TitanicData$Embarked)
TitanicData$Embarked[is.na(TitanicData$Embarked)]="S"

# Checking missing values after treatment
colSums(is.na(TitanicData))

# Step-8
# Explore each "Potential" predictor for distribution and Quality
############################################################

# Exploring MULTIPLE CONTINUOUS features
ColsForHist=c("Age","Fare")

#Splitting the plot window into four parts
par(mfrow=c(2,1))

# library to generate professional colors
library(RColorBrewer) 

# looping to create the histograms for each column
for (ColumnName in ColsForHist){
  hist(TitanicData[,c(ColumnName)], main=paste('Histogram of:', ColumnName), 
       col=brewer.pal(8,"Paired"))
}


############################################################
# Exploring MULTIPLE CATEGORICAL features
ColsForBar=c("Survived","Pclass","Embarked")

#Splitting the plot window into four parts
par(mfrow=c(2,2))

# looping to create the Bar-Plots for each column
for (ColumnName in ColsForBar){
  barplot(table(TitanicData[,c(ColumnName)]), main=paste('Barplot of:', ColumnName), 
          col=brewer.pal(8,"Paired"))
}


############################################################

# Step-9
# Visual Relationship between target variable and predictors

##for classification- dependent:categorical and predictor: categorical/continuous
# Categorical Vs Continuous --- Box Plot
# Categorical Vs Categorical -- Grouped Bar chart

############################################################
# Categorical Vs Continuous Visual analysis: Boxplot

##Age: continuous, Survived: Categorical
par(mfrow=c(1,1))
boxplot(Age~Survived, data = TitanicData, col=brewer.pal(8,"Paired"))

boxplot(Fare~Survived, data = TitanicData, col=brewer.pal(8,"Paired"))

############################################################
############################################################
# Categorical Vs Categorical Visual analysis: Grouped Bar chart

##in table() command, if we use one column, it will give the number of 
##occurances of each value of that column
table(TitanicData$Sex)

##but if we pass 2 columns, it will cross tabulate the values of both the columns
#so here using one command you can understand:
#how many female didn't survive
#how many male didn't survive 
#how many female survived
#how many male survived 
chi_cols=c("Sex")

CrossTabResult=table(TitanicData[,c('Survived',chi_cols)])
CrossTabResult

barplot(CrossTabResult, beside=T, col=c('Red','Green'))
#0: Not survived
#1:survived

##there is huge casualty for males, and for females it's the opposite
##based on the graph itself you can say that here gender actually has a
#relationship with survival
##there is a dependency



# Step-9
# Statistical Relationship between target variable (Categorical) and predictors

# Categorical Vs Continuous --- ANOVA
# Categorical Vs Categorical -- Chi-square test


# Continuous Vs Categorical relationship strength: ANOVA
# Analysis of Variance(ANOVA)
# H0: Variables are NOT correlated
# Small P-Value <5%--> Variables are correlated(H0 is rejected)
# Large P-Value--> Variables are NOT correlated (H0 is accepted)

summary(aov(Age~Survived, data = TitanicData))

summary(aov(Fare~Survived, data = TitanicData))


#### Categorical Vs Categorical relationship strength: Chi-Square test
# H0: Variables are NOT correlated
# Small P-Value--> Variables are correlated(H0 is rejected)
# Large P-Value--> Variables are NOT correlated (H0 is accepted)


##It takes crosstabulation as the input and gives you the result
Chisqcols=c("Pclass","Sex","Embarked")
for(chi_cols in ColsForBar ){
  CrossTabResult=table(TitanicData[,c('Survived',chi_cols)])
  ChiResult=chisq.test(CrossTabResult)
  print(ColsForBar)
  print(ChiResult)
}


#H0:the two columns are not correlated

#p is very low, 
#so we reject the null and conclude that these two columns are correlated


###do the chi-sq test for all other variables - do an automation i.e write a for loop
#and see which of the categorical variables are correlated with Survived column
############################################################



############################################################
# Step-10
InputData=TitanicData

# Specifying the Target Variable
TargetVariableName='Survived'

# Extracting Target and predictor variables from data to create a generic dataset
TargetVariable=InputData[, c(TargetVariableName)]
str(TargetVariable)

# Here I am Selecting all other columns as Predictors apart from target variable
#but based on EDA, you'll have to choose those columns which are important

PredictorVariables=InputData[, !names(InputData) %in% TargetVariableName]
str(PredictorVariables)


DataForML=data.frame(TargetVariable,PredictorVariables)
##make sure you look at the structure before running any classification algo
str(DataForML)
head(DataForML)

# Step-12
#############################################################################################
# Sampling | Splitting data into 70% for training 30% for testing
TrainingSampleIndex=sample(1:nrow(DataForML), size=0.7 * nrow(DataForML) )
DataForMLTrain=DataForML[TrainingSampleIndex, ]
DataForMLTest=DataForML[-TrainingSampleIndex, ]
dim(DataForMLTrain)
dim(DataForMLTest)



#############################################################################################
#############################################################################################
# Creating Predictive models on training data to check the accuracy on test data
###### Logistic Regression #######

##we are predicting TV based on all other variables
##glm() is used for wide variety of modeling activities. Logistic regression
#is one of the models that you can create using glm()
##in order to tell glm() that you have to perform logistic regression,
#you have to say family= 'binomial"
startTime=Sys.time()
LR_Model=glm(TargetVariable ~ . , data=DataForMLTrain, family='binomial')

summary(LR_Model)
endTime=Sys.time()
endTime-startTime

##probabilities will guide you whether to accept or reject a particular column

#LR_Model_2=glm(TargetVariable ~ I(Pclass==2)+I(Pclass==3)+I(Sex=="male")+Age+Embarked, data=DataForMLTrain, family='binomial')
LR_Model_2=glm(TargetVariable ~ .-Fare, data=DataForMLTrain, family='binomial')
summary(LR_Model_2)



#LR_Model_3=glm(TargetVariable ~ I(Pclass==2)+I(Pclass==3)+I(Sex=="male")+Age, data=DataForMLTrain, family='binomial')
LR_Model_3=glm(TargetVariable ~ .-Fare-Embarked, data=DataForMLTrain, family='binomial')
summary(LR_Model_3)


#Deviance is a measure of goodness of fit of a generalized linear model.
#Or rather, it's a measure of badness of fit: higher numbers indicate worse fit.
#The null deviance shows how well the response variable is predicted by a model 
#that includes only the intercept
nrow(DataForMLTrain)
#Including _4_ no. of independent variables the deviance decreased to ___ 


#residual deviance has to be lesser than null deviance to have a good model

##degrees of freedom obtained by R automatically can be interpreted as follows:
#you can see that addition of _4__ (498-494 =4) independent variables
#decreased the deviance to ____ from ___,
#a significant reduction in deviance.


##Unlike R-squared- this AIC value is relative
#as you run different models you see how the AIC value is changing
#lower it is, better is the model

###if you see that AIC has increased then the model won't be effective
##you want the least possible value of AIC
#and finalize the model with least AIC

#odds of surviving: p(Survived=1)/1-p(Survived=1)
#the regression coefficient for Age is -0.03. 
#This indicate that one unit increase in the age will decrease
#the odds of surviving by a factor of exp(-0.03)

#when we say that odds of surviving decreases by a factor of exp(-0.03)
#we can calculate the percentage of decrease
#so, it means if age goes up by 1, then the odds of surviving decreases by 
#(exp(-0.03)-1)*100 percent

#negative sign means, when age goes up by 1 unit, you are less likely to survive

#if you belong to p-class 2 compared to p-class 1, you are less likely to survive
#if you belong to p-class2 compared to p-class1, then the odds of surviving decreases
#by __?___ %


# Checking Accuracy of model on Testing data
PredictionProb=predict(LR_Model_3,DataForMLTest,type = "response")
PredictionProb

##considering a threshold of 0.55

DataForMLTest$Prediction=ifelse(PredictionProb>0.55, 1, 0)
DataForMLTest$Prediction=as.factor(DataForMLTest$Prediction)
head(DataForMLTest)

# Creating the Confusion Matrix to calculate overall accuracy, precision and recall on TESTING data
install.packages('caret', dependencies = TRUE)
library(caret)

AccuracyResults=confusionMatrix(DataForMLTest$Prediction, DataForMLTest$TargetVariable, mode = "prec_recall")

# Since AccuracyResults is a list of multiple items, fetching useful components only

AccuracyResults[['table']]
AccuracyResults[['byClass']]
AccuracyResults[['overall']][1]

print(paste('### Overall Accuracy of Logistic Reg Model is: ', round(100 * AccuracyResults[['overall']][1]) , '%'))


#############################################################################################
###### Ctree Decision Tree #######
library(party)

DT_Model_1=ctree(TargetVariable ~ Fare+Sex , data=DataForMLTrain)
plot(DT_Model_1)

##among these two variables, most important variable is "Sex", so it is splitted
#first. Then for male and female separately, "fare" variable is again bifurcated, and 
#we get the corresponding results
#0:not survived
#1:survived
##we get the proportions of 0 and 1 in each case


startTime=Sys.time()
DT_Model=ctree( TargetVariable~ . , data=DataForMLTrain)
DT_Model
plot(DT_Model)

endTime=Sys.time()
endTime-startTime


# Checking Accuracy of model on Testing data
DataForMLTest$Prediction =predict(DT_Model, DataForMLTest)
head(DataForMLTest)

# Creating the Confusion Matrix to calculate overall accuracy, precision and recall on TESTING data
library(caret)
AccuracyResults=confusionMatrix(DataForMLTest$Prediction, DataForMLTest$TargetVariable, mode = "prec_recall")

# Since AccuracyResults is a list of multiple items, fetching useful components only
AccuracyResults[['table']]
AccuracyResults[['byClass']]

print(paste('### Overall Accuracy of Ctree Model is: ', round(100 * AccuracyResults[['overall']][1]) , '%'))






