

#input csv
Main_dataset <- read.csv("C:/Melwin/COLLEGE PORTION/Anu Sahani/Project description/Datasets/adult-income-dataset/adult.csv")

head(Main_dataset)

View(Main_dataset)


str(Main_dataset)

nrow(Main_dataset)
Main_dataset[Main_dataset == "?"] <- NA

nrow(is.na(Main_dataset))
Main_dataset=na.omit(Main_dataset)

nrow(Main_dataset)


Main_dataset$workclass<-as.character(Main_dataset$workclass)
table(Main_dataset$workclass)



Main_dataset$workclass[Main_dataset$workclass=="Federal-gov"|
                         Main_dataset$workclass=="Local-gov"|
                         Main_dataset$workclass=="Self-emp-inc"|
                         Main_dataset$workclass=="Self-emp-not-inc"|
                         Main_dataset$workclass == "State-gov"|
                         Main_dataset$workclass == "Private"] <- "Employed"



Main_dataset$workclass[Main_dataset$workclass == "Never-worked"|
                         Main_dataset$workclass == "Without-pay"] <- "Unemployed"




table(Main_dataset$education)
Main_dataset$education<-as.character(Main_dataset$education)
Main_dataset$education[Main_dataset$education == "1st-4th"|
                         Main_dataset$education == "5th-6th"] <- "1st-6th"

Main_dataset$education[Main_dataset$education == "7th-8th"|
                         Main_dataset$education == "9th"|
                         Main_dataset$education == "10th"|
                         Main_dataset$education == "11th"|
                         Main_dataset$education == "12th"]<-"7th-12th"

Main_dataset$education[Main_dataset$education == "Assoc-acdm"|
                         Main_dataset$education == "Assoc-voc"]<-"Associate Degree"


#Main_dataset$educational.num<-factor(Main_dataset$educational.num, levels = c(1-5,6-11,12-16), labels = c("1-5", "6-11", "12-16"))



table(Main_dataset$marital.status)
Main_dataset$marital.status<-as.character(Main_dataset$marital.status)
Main_dataset$marital.status[Main_dataset$marital.status == "Divorced"|
                              Main_dataset$marital.status == "Never-married"|
                              Main_dataset$marital.status == "Separated"|
                              Main_dataset$marital.status =="Widowed"] <- "Not Married"

Main_dataset$marital.status[Main_dataset$marital.status == "Married-AF-spouse" |
                              Main_dataset$marital.status == "Married-civ-spouse" |
                              Main_dataset$marital.status == "Married-spouse-absent"] <- "Married"



summary(Main_dataset$hours.per.week)

#Main_dataset$hours.per.week<-factor(Main_dataset$hours.per.week, levels = c(1-24, 25-49, 50-74, 75-99), labels = c("<24", "24-49", "50-74", "75+") )

str(Main_dataset)

table(Main_dataset$hours.per.week)


library(dplyr)
install.packages("corrplot")
library(corrplot)

#correlation
corrplot(cor(temp),method = "pie")

temp = select(Main_dataset, c(age,fnlwgt,educational.num,capital.gain,capital.loss))



#Some Visualisations
library(ggplot2)
ggplot(Main_dataset, aes(x=education))+geom_bar()+facet_grid(~gender)

ggplot(Main_dataset, aes(x=income, y=1, fill=marital.status))+geom_bar(stat = "identity", position = "fill")+facet_grid(~gender)


ggplot(Main_dataset, aes(x=income, y=age))+geom_boxplot()+ facet_grid(~gender)





library(caret)
library(lattice)
index<-createDataPartition(Main_dataset$income, p=0.8, list =F)
Train<-Main_dataset[index,]
Test<-Main_dataset[-index,]

model2<-rpart(income~., data=Train)
pred_class<-predict(model2, Test, type= "class")
confusionMatrix(pred_class, Test$income, positive = ">50K")
#83.83% accuracy

View(Main_dataset)

#ROC
library(gplots)
library(ROCR)
pred_prob<-predict(model2, Test)
P_Test<- prediction(pred_prob[,2], Test$income)
perf<-performance(P_Test, "tpr", "fpr")
plot(perf)
performance(P_Test, "auc")@y.values




library(rpart.plot)   

rpart.plot(model2, box.palette="RdBu", shadow.col="gray", nn=TRUE)
