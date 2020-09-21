library(corrplot)
library(dplyr)

#input csv
read_data <- read.csv("C:/Melwin/COLLEGE PORTION/Anu Sahani/Project description/Datasets/healthcare-dataset-stroke-data/train_2v.csv")

head(read_data)


sapply(read_data,function(x) sum(is.na(x)))

missmap(read_data, main="Missing Values vs Observed")
read_data$bmi <- as.numeric(read_data$bmi)
read_data$bmi = ifelse(is.na(read_data$bmi),ave(read_data$bmi, FUN = function(x) mean(x, na.rm = TRUE)),read_data$bmi)


read_data$smoking_status = ifelse(is.na(read_data$smoking_status),ave(read_data$smoking_status, FUN = function(x) median(x, na.rm = TRUE)),read_data$smoking_status)

read_data$stroke=as.factor(read_data$stroke)
str(read_data)


#sampling

library(ROSE)

sampling <- ROSE(stroke ~ ., data = read_data, seed = 1)$data
table(sampling$stroke)

table(sampling$stroke)

str(read_data)

#correlation
corrplot(cor(temp),method = "pie")

temp = select(read_data, c(hypertension,heart_disease,avg_glucose_level,bmi,smoking_status))



set.seed(1234)
split <- sample(seq_len(nrow(sampling)), size = floor(0.75 * nrow(sampling)))
DiseaseTrain <- sampling[split, ]
DiseaseTest  <- sampling[-split, ]
dim(DiseaseTrain)
dim(DiseaseTest)

table(DiseaseTest$stroke)
table(DiseaseTrain$stroke)

install.packages("randomForest")
library(randomForest)

#applying the random forest model to the proper dataframe derived during random forest operation



rf_model<-randomForest(stroke ~  hypertension + heart_disease + smoking_status + bmi,data=DiseaseTrain,ntree=450,importance=TRUE)

print(rf_model)

summary(rf_model)

rf_model$confusion


library(caret)

# Predicting on data set
predTrain <- rf_model$predicted

# Checking classification accuracy
table(predTrain,DiseaseTrain$stroke)  

# Predicting on test set
predtest <- predict(rf_model,DiseaseTest)

head(predtest)

head(DiseaseTest$stroke)

confusionMatrix(predtest,DiseaseTest$stroke) #82.02%

plot(rf_model)

