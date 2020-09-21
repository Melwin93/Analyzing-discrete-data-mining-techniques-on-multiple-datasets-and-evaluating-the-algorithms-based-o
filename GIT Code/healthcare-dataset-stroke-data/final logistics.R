install.packages("caTools")
install.packages("pscl")
install.packages("caret")
install.packages("ROCR")
install.packages("e1071")
install.packages("class")
install.packages("lattice")
install.packages("dplyr")
library(dplyr)
library(psych)
library(car)
library(Hmisc)
library(caret)
library(caTools)
library(ROCR)
library(class)
library(e1071)
library(Amelia)


#input csv
inputData <- read.csv("C:/Melwin/COLLEGE PORTION/Anu Sahani/Project description/Datasets/healthcare-dataset-stroke-data/train_2v.csv")

head(inputData)

nrow(inputData)

View(inputData)

str(inputData)

table(inputData$stroke)

sapply(inputData,function(x) sum(is.na(x)))

missmap(inputData, main="Missing Values vs Observed")

#transfromation
inputData$bmi <- as.numeric(inputData$bmi)
inputData$bmi = ifelse(is.na(inputData$bmi),ave(inputData$bmi, FUN = function(x) mean(x, na.rm = TRUE)),inputData$bmi)


inputData$smoking_status = ifelse(is.na(inputData$smoking_status),ave(inputData$smoking_status, FUN = function(x) median(x, na.rm = TRUE)),inputData$smoking_status)

inputData$stroke=as.factor(inputData$stroke)
str(inputData)

nrow(inputData)


#sampling

library(ROSE)
library(corrplot)
sampling <- ROSE(stroke ~ ., data = inputData, seed = 1)$data
table(sampling$stroke)


#correlation
corrplot(cor(temp),method = "pie")

temp = select(inputData, c(hypertension,heart_disease,avg_glucose_level,bmi,smoking_status))

str(sampling)

inputData=sampling


set.seed(1234)
split <- sample(seq_len(nrow(inputData)), size = floor(0.75 * nrow(inputData)))
DiseaseTrain <- inputData[split, ]
DiseaseTest  <- inputData[-split, ]
dim(DiseaseTrain)
dim(DiseaseTest)

table(DiseaseTest$stroke)
table(DiseaseTrain$stroke)




model <- glm(formula = stroke ~ hypertension + heart_disease + smoking_status + bmi, family = binomial(link = "logit"),data = DiseaseTrain)


predTest = predict(model, DiseaseTest, type= 'response')
predTest <- ifelse(predTest > 0.5,1,0)

u <- union(predTest, DiseaseTest$stroke)
t <- table(factor(predTest, u), factor(DiseaseTest$stroke, u))
confusionMatrix(t) 


#68.14% accuracy #98.53% with sampling

