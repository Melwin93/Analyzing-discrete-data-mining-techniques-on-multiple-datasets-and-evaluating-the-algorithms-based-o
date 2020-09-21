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

library(corrplot)
library(dplyr)
library(ggplot2)
library(caret)
library(lattice)
library(class)



#input csv
Naive_dataset <- read.csv("C:/Melwin/COLLEGE PORTION/Anu Sahani/Project description/Datasets/gun-deaths-in-the-us/guns.csv")

head(Naive_dataset)

View(Naive_dataset)

nrow(Naive_dataset)
str(Naive_dataset)

Naive_dataset[Naive_dataset == ""] <- NA

Naive_dataset=na.omit(Naive_dataset)

nrow(Naive_dataset) #99343 after removing NA values

#to plot race wise deaths on male and female
barplot(t(data.frame("F"=summary(Naive_dataset$race[Naive_dataset$sex=="F"]), "M"=summary(Naive_dataset$race[Naive_dataset$sex=="M"]))), main="Gun Deaths by Race", beside=T, col=rainbow(2))
legend("topleft", c("Female", "Male"), cex=1.4, 
       bty="n", fill=rainbow(2))



hist(Naive_dataset$age, main="Age at Gun Death", xlab="Age in Years", ylab="Number of Deaths")


str(Naive_dataset)

#correlation
corrplot(cor(temp),method = "pie")

temp = select(Naive_dataset, c(age,hispanic,education,age))



#drop column
Naive_dataset$X= NULL
Naive_dataset$year= NULL
Naive_dataset$month= NULL

Naive_dataset$intent=as.numeric(Naive_dataset$intent)

Naive_dataset$intent[Naive_dataset$intent=="Accidental"]="1"
Naive_dataset$intent[Naive_dataset$intent=="Homicide"]="2"
Naive_dataset$intent[Naive_dataset$intent=="Suicide"]="3"
Naive_dataset$intent[Naive_dataset$intent=="Undetermined"]="4"

str(Naive_dataset)

Naive_dataset$intent=as.integer(Naive_dataset$intent) #it won't convert into pivot for int


# used to convert rows data into columns (pivot) only categories and not continus nos are converted into 0,1
a<-dummyVars("~.",data=Naive_dataset)
encd_shp <- data.frame(predict(a, newdata = Naive_dataset))

str(encd_shp)

encd_shp$intent=as.factor(encd_shp$intent)

set.seed(1234)
split <- sample(seq_len(nrow(encd_shp)), size = floor(0.75 * nrow(encd_shp)))
Train <- encd_shp[split, ]
Test  <- encd_shp[-split, ]
dim(Train)
dim(Test)

str(Train)


str(Test)


library(e1071)

View(encd_shp)

#for train
model=naiveBayes(intent ~., data = Train)

a=predict(model,Train)

summary(a)

confusionMatrix(as.factor(a),Train$intent)

#for test
a=predict(model,Test)

confusionMatrix(as.factor(a),Test$intent) #62.76%

