
#Logistic :-->

#Data insertion
#-----------------------------------------------


#input csv
inputData <- read.csv("C:/Melwin/COLLEGE PORTION/Anu Sahani/Project description/Datasets/healthcare-dataset-stroke-data/train_2v.csv")

head(inputData)


inputData


d=data.frame(inputData)

View(d)


#Assigning missing values as 'NA'
#-----------------------------------------------
d$smoking_status[d$smoking_status == ''] <- NA  #To assign Missing values as NA

nrow(d) #-->43400


#------instead of deleting imputation is used

nrow(na.omit(d)) #-->29072 (AFTER OMITTING MISSING VALUES IN bmi AND smoking_status TOTAL MISSING VALUES IN BOTH 14328)

proper=na.omit(d)

proper

View(proper) # proper data after cleaning of missing values.


#imputation using mice
#-----------------------------------------------

#Imputation method used instead of deleting

d1=d

View(d1)

#MICE IS USED FOR IMPUTATION OF DATA

# https://datascienceplus.com/imputing-missing-data-with-r-mice-package/

library(mice)


empData <- mice(d1,m=5,maxit=2,meth='pmm',seed=500)


completedData <- complete(empData,1)


View(empData)

completedData
View(completedData)

nrow(completedData) #43400

# to check if there is bais in data or no

table(completedData$stroke)

#0       1
#42617   783


#Histograms and boxplots


install.packages("ggplot2")

hist(completedData$stroke,xlab="stroke",labels=T,main="Histogram of stroke")

hist(completedData$smoking_status,xlab="smoking_status",labels=T,main="Histogram of smoking_status")

# done proper
hist(completedData$bmi,xlab="bmi",labels=T,main="Histogram of bmi")  #proper histogram


hist(completedData$avg_glucose_level,xlab="glucose_level",labels=T,main="Histogram of glucose_level")



write.csv(completedData, file = "test.csv")


#completedData$stroke <- as.factor(completedData$stroke)

str(completedData)



completedData$hypertension <- as.factor(completedData$hypertension)

completedData$heart_disease <- as.factor(completedData$heart_disease)

completedData$stroke <- as.factor(completedData$stroke)

#Train and test datasets

split <- sample(seq_len(nrow(completedData)), size = floor(0.75 * nrow(completedData)))
StrokeTrain <- completedData[split, ]
StrokeTest  <- completedData[-split, ]
dim(StrokeTrain)
dim(StrokeTest)


#sampling
library(ROSE)

data.rose_train <- ROSE(stroke ~ ., data = StrokeTrain, seed = 1)$data
table(data.rose_train$stroke)


prop.table(table(StrokeTrain$stroke))
prop.table(table(StrokeTest$stroke))


#logistic regression and prediction on train and test datastes



logitMod <- glm(stroke ~ hypertension + heart_disease + smoking_status, data=data.rose_train, family="binomial")

pred_stroke <- predict(logitMod, newdata = StrokeTest, type = "response")

ConfMatrixTable  <- confusionMatrix(pred_stroke,StrokeTest$stroke)
ConfMatrixTable

str(pred_stroke)


install.packages("lattice")
library(lattice)
install.packages("ggplot2")
library(ggplot2)


pred_stroke <- predict(logitMod, newdata = StrokeTest, type = "response")
pred_stroke[pred_stroke>0.5] <- 1
pred_stroke[pred_stroke != 1] <- 0
pred_stroke <- as.factor(pred_stroke)
StrokeTest$stroke <- as.ordered(StrokeTest$stroke)

summary(logitMod)
summary(pred_stroke)



install.packages("caret")


library(caret)



ConfMatrixTable  <- confusionMatrix(pred_stroke,StrokeTest$stroke)
ConfMatrixTable


str(pred_stroke)
# Evaluation methods on logistic regression.

# confusion matrix

#------------------------------------------------------------------------------------------

str(completedData)

#install.packages("randomForest")
library(randomForest)

regressor = randomForest(completedData$stroke ~ hypertension+heart_disease+smoking_status,tree = 10, data = completedData, importance=TRUE)


print(regressor)



#--------------------------------------------------------------------------------------------


install.packages("randomForest")
library(randomForest)

#applying the random forest model to the test_df dataframe derived during random forest operation
rf_model<-randomForest(stroke ~ hypertension+heart_disease+smoking_status,data=completedData,ntree=500,mtry=6,importance=TRUE)

summary(rf_model)

# Predicting on train set
predTrain <- rf_model$predicted

# Checking classification accuracy
table(predTrain,StrokeTrain$stroke)  

# Predicting on test set
predtest <- predict(rf_model,StrokeTest, type = "class")

summary(predtest)

# Checking classification accuracy
accuracyRF <- mean(predtest == StrokeTest$stroke)                    
accuracyRF
table(predtest,StrokeTest$stroke)



#---------------------------------

classifier = randomForest(x = StrokeTrain,y = StrokeTrain$stroke, ntree = 500)

# Predicting the Test set results
y_pred = predict(classifier, newdata = StrokeTest)

# Making the Confusion Matrix
cm = table(StrokeTest, y_pred)

















frml <- (stroke~gender+age+hypertension+heart_disease+ever_married+work_type+Residence_type+avg_glucose_level+bmi)

model_rf <- randomForest(formula = frml, data = completedData)

prediction_rf <- predict(object = model_rf,newdata = select(completedData, -stroke),type = "class")

confusionMatrix(data = prediction_rf, reference = Stroke_Data$stroke)
