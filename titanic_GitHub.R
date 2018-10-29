#########################
#Created By :- Devansh Popat
#########################


#Loading the required libraries
library(tidyverse)
library(ggplot2)
library(randomForest)
library(rpart)
library(rpart.plot)

#Loading the data set
train <- read.csv("../input/train.csv")
test <- read.csv("../input/test.csv")

#Combining the train and test data frames in one for cleaning
titanic<-rbind(train[,-2],test)

# Data Cleaning ##########################################################################
#Assigning correct data types to the variables
titanic$Name<-as.character(titanic$Name)
titanic$SibSp<-as.factor(titanic$SibSp)
titanic$Parch<-as.factor(titanic$Parch)
titanic$Ticket<-as.character(titanic$Ticket)
titanic$Cabin<-as.character(titanic$Cabin)
titanic$Pclass<-as.factor(titanic$Pclass)

#Performing univariate analysis

#Embarked Variable has two rows with missing data
titanic %>% filter(Embarked=="") %>% count()
table(titanic$Embarked)

#The factor S occurs maximum times so filling the missing values with S
titanic$Embarked[titanic$Embarked==""]="S"
titanic$Embarked<-factor(titanic$Embarked)

#Fare Variable has NA values so we replace them with median of all the ticket fares
titanic %>% filter(is.na(titanic$Fare)=="TRUE")
titanic$Fare[is.na(titanic$Fare)] <- median(titanic$Fare, na.rm=TRUE)

#Parch, SibSp, Pclass Variables
table(titanic$Parch)
table(titanic$SibSp)
table(titanic$Pclass)

#Data Wrangling in Name Variable
titanic$Name<-gsub('(.*, )|(\\..*)','',titanic$Name)
titanic$Name[titanic$Name %in% c('Capt','Col','Don','Dr','Jonkheer','Major','Rev','Sir','the Countess')]<-'Officer'
titanic$Name[titanic$Name %in% c('Mlle','Ms','Lady','Dona')]<-'Miss'
titanic$Name[titanic$Name %in% c('Mme')]<-'Mrs'
table(titanic$Name)
class(titanic$Name)
titanic$Name<-as.factor(titanic$Name)


#Age variable
table(is.na(titanic$Age))
#Age variable has 263 missing values. Imputing them using Randomforest approach
row.has.na <- apply(titanic, 1, function(x){any(is.na(x))})
age_train<-titanic[!row.has.na,]
age_test<-titanic[row.has.na,]
#Estimating variables which are correlated with Age
summary(aov(Age~.,data = age_train))
#Sex and Fare are not responsible for affecting the Age so we exclude them from the Random Forest Model
model2<-randomForest(Age~Pclass+Name+SibSp+Parch+Embarked,data = age_train)
print(model2)
varImpPlot(model2)
summary(predict(model2,newdata = age_test))


#Imputing the age using the model that we built
age_test$Age<-predict(model2,newdata = age_test)
age_test$Age<-round(age_test$Age)

#Combining back age_train and age_test to original data frame
titanic_cleaned<-rbind(age_train,age_test)
titanic_cleaned$Age<-round(titanic_cleaned$Age)
summary(titanic_cleaned$Age)
titanic_cleaned <- titanic_cleaned[order(titanic_cleaned$PassengerId),]



#Fare Should be numeric
titanic_cleaned$Fare<-as.numeric(titanic_cleaned$Fare)

#Cabin has too many missing values and it doesn't seem to be having meaningful in predicting the Survival for passenger
titanic %>% filter(titanic$Cabin=="") %>% count()
titanic_cleaned$Cabin<-NULL


#Similar approach for Ticket variable
titanic_cleaned$Ticket<-NULL
#Data Cleaning done, spliting into original train and test data now
titanic_test<-titanic_cleaned %>% filter(PassengerId > nrow(train))
titanic_train<-titanic_cleaned %>% filter(PassengerId < (nrow(train)+1))
titanic_train$Survived<-train$Survived

# Data Visualization ###################################################################
#Survival vs. Sex
#More Females were saved
ggplot(data = titanic_train)+aes(x=Survived, fill=Sex)+geom_bar(position="stack")+scale_fill_brewer(palette="Pastel2")+labs(title='Relationship between survival and gender',x="Survived", y="Frequency")


#Survival vs. Passenger class
ggplot(data=titanic_train)+aes(x=Survived,fill=Pclass)+geom_bar(position="stack")+scale_fill_brewer(palette="Pastel2")+labs(title='Relationship between survival and ticket class',x="Survived", y="Frequency")
#Ticket class is 1 -> More people survived
#Ticket class is 2 -> No of people survived=No of people killed
#Ticket class is 3 -> More people died
#I will always buy a first class ticket !!!


#Survival vs. SibSp
ggplot(data=titanic_train)+aes(x=Survived,fill=SibSp)+geom_bar(position="dodge")+scale_fill_brewer(palette="Pastel1")+labs(title='Relationship between survival and Siblings/Spouse',x="Survived", y="Frequency")
#Passengers with 1 or 2 Siblings/Spouse managed to Survive. More than 50% of Rest others failed to Survive
#You have 1 or 2 Siblings/Spouse? You could make it to the LifeBoat!


#Survival vs. Parch
ggplot(data=titanic_train)+aes(x=Survived,fill=Parch)+geom_bar(position="dodge")+scale_fill_brewer(palette="Pastel2")+labs(title='Relationship between survival and Parents/Children',x="Survived", y="Frequency")
#Passengers with 1 or 2 or 3 Parents/Children Survived
#If I travel by ship, I'll take my parents and kids along!!!



#Survival vs. Embarked
ggplot(data=titanic_train)+aes(x=Survived,fill=Embarked)+geom_bar(position="fill")+scale_fill_brewer(palette="Pastel1")+labs(title='Relationship between survival and Departure Port',x="Survived", y="Frequency")
#Port of Embarkation = Cherbourg? -> More people survived
#Port of Embarkation = Queenstoun? -> No. of people killed= No. of people Survived
#Port of Embarkation = Southampton? -> More people died
#I guess People from Cherbourg were rich !!! (They bribed the guards maybe!)



#Survival vs. Fare
titanic_train %>% ggplot(aes(x=Fare, fill=Survived)) + geom_density(alpha=0.5) + scale_fill_brewer(palette="Spectral")+labs(title='Relationship between survival and Fare',x="Fare", y="Density of People Killed")
#You got cheap tickets? -> You got a cabin at ground level -> Tough to Survive



#Survival vs. Age
titanic_train %>% ggplot(aes(x=Age, fill=Survived)) + geom_density(alpha=0.5) + scale_fill_brewer(palette="Spectral")+labs(title='Relationship between survival and Age',x="Age", y="Density of People Killed")
#The guards took maximum people with age > 60 (Seniors) and age <20 (Children and Youngsters) to the life boat!!



# Data Modelling#################################################################
#Train test split
set.seed(1234)
train_index <- sample(1:nrow(titanic_train), 0.8 * nrow(titanic_train))
test_index <- setdiff(1:nrow(titanic_train), train_index)

X_train<-titanic_train[train_index,]
y_train<-titanic_train[train_index,"Survived"]

X_test<-titanic_train[test_index,]
y_test<-titanic_train[test_index,"Survived"]



#Logistic Regression===============
model_lr<-glm(Survived~.,family = binomial(link = 'logit'),data=X_train)

#Test data has Parch=6 which was not there in the train data
model_lr$xlevels[["Parch"]] <- union(model_lr$xlevels[["Parch"]], levels(titanic_test$Parch))

#Accuracy of model on train data
result_lr<-predict(model_lr, newdata = X_train[,-10], type = 'response')
result_lr<-ifelse(result_lr>0.5,1,0)
error_lr<-mean(result_lr!=y_train)
print(paste('Accuracy of model on train data using Logistic regression = ',1-error_lr))


#Accuracy of model on test data
result_lr<-predict(model_lr, newdata = X_test[,-10], type = 'response')
result_lr<-ifelse(result_lr>0.5,1,0)
error_lr<-mean(result_lr!=y_test)
print(paste('Accuracy of model on test data using Logistic regression = ',1-error_lr))



#Decision Tree model=======================================================
model_dt<-rpart(Survived~.,data = X_train, parms = list(split = "information"))
printcp(model_dt)
model_dt<-rpart(Survived~.,data = X_train, parms = list(split = "information"),control=rpart.control(minsplit = 4, cp=0.015))

result_dt<-predict(model_dt,newdata = X_train)
result_dt<-ifelse(result_dt>0.5,1,0)
error_dt<-mean(result_dt!=y_train)
print(paste('Accuracy of model on train data using Decision tree = ',1-error_dt))


result_dt<-predict(model_dt,newdata = X_test)
result_dt<-ifelse(result_dt>0.5,1,0)
error_dt<-mean(result_dt!=y_test)
print(paste('Accuracy of model on train data using Decision Tree = ',1-error_dt))

#Plotting the decision tree
rpart.plot(model_dt, type = 3 ,clip.right.labs=FALSE)



#Random Forest Model=======================================================
model_rf<-randomForest(Survived~.,data = X_train)

result_rf<-predict(model_rf,newdata = X_train)
result_rf<-ifelse(result_rf>0.5,1,0)
error_rf<-mean(result_rf!=y_train)
print(paste('Accuracy of model on train data using Random Forest = ',1-error_rf))


result_rf<-predict(model_rf,newdata = X_test)
result_rf<-ifelse(result_rf>0.5,1,0)
error_rf<-mean(result_rf!=y_test)
print(paste('Accuracy of model on train data using Random Forest = ',1-error_rf))
varImpPlot(model_rf)



#Final predictions on unseen test data using random forest############################
result_final<-predict(model_rf, newdata = titanic_test)
result_final<-ifelse(result_final>0.5,1,0)

titanic_predictions<-data.frame("PassengerId"=titanic_test$PassengerId, "Survived"=result_final)

summary(titanic_predictions)
str(titanic_predictions)

############################################################################################








