install.packages("caTools")
install.packages("pscl")
install.packages("caret")
install.packages("ROCR")
install.packages("e1071")
install.packages("class")
install.packages("lattice")
library(psych)
library(car)
library(Hmisc)
library(caret)
library(caTools)
library(ROCR)
library(class)
library(e1071)


#I have commented some codes which are used in kaggle but actually is not required

#input csv
KNN_dataset <- read.csv("C:/Melwin/COLLEGE PORTION/Anu Sahani/Project description/Datasets/adult-income-dataset/adult.csv")

head(KNN_dataset)

View(KNN_dataset)

nrow(KNN_dataset)


str(KNN_dataset)

View(KNN_dataset)

KNN_dataset[KNN_dataset == "?"] <- NA

KNN_dataset=na.omit(KNN_dataset)

nrow(KNN_dataset)

install.packages("corrplot")
library(corrplot)

corrplot(cor(temp),method = "pie")

temp = select(KNN_dataset, c(age,fnlwgt,educational.num,capital.gain,capital.loss))

str(temp)

cor(temp)

str(KNN_dataset)

str(KNN_dataset)

library(dplyr)

#converting catagorical columns to one hot encoded


# used to convert rows data into columns (pivot) only categories and not continus nos are converted into 0,1
a<-dummyVars("~.",data=KNN_dataset)
encd_shp <- data.frame(predict(a, newdata = KNN_dataset))

str(encd_shp)

# if >50 it is 1 else 0
table(encd_shp$income...50K)

table(encd_shp$income..50K)

prop.table(table(encd_shp$income...50K))*100


#drop column
encd_shp$income..50K= NULL



library(randomForest)


#is used for which column is significane
#fit_rf <- randomForest(income...50K~., data=encd_shp)

#table(encd_shp$income...50K)

#importance(fit_rf)
#options(scipen = 999)




#Some Visualisations
library(ggplot2)
ggplot(KNN_dataset, aes(x=education))+geom_bar()+facet_grid(~gender)

ggplot(KNN_dataset, aes(x=income, y=1, fill=marital.status))+geom_bar(stat = "identity", position = "fill")+facet_grid(~gender)


ggplot(KNN_dataset, aes(x=income, y=age))+geom_boxplot()+ facet_grid(~gender)



library(caret)
library(lattice)
library(class)

set.seed(1234)
split <- sample(seq_len(nrow(encd_shp)), size = floor(0.75 * nrow(encd_shp)))
Train_KNN <- encd_shp[split, ]
Test_KNN  <- encd_shp[-split, ]
dim(Train_KNN)
dim(Test_KNN)

table(Train_KNN$income...50K)
table(Test_KNN$income...50K)


#str(encd_shp)

knn1 <- knn(train=Train_KNN, test=Test_KNN, cl=Train_KNN$income...50K, k=1) #73%
knn212 <- knn(train=Train_KNN, test=Test_KNN, cl=Train_KNN$income...50K, k=212)

#76%  #sq root of og dataset

confusionMatrix(as.factor(Test_KNN$income...50K),knn212)

