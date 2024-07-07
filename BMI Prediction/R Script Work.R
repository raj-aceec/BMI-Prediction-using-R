#installing Required Packages
install.packages("tidyverse") 
install.packages("base")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("broom")
install.packages("ggpubr")
install.packages("tidyverse")
install.packages("catTools")
install.packages("predict3d")
install.packages("corrplot")
install.packages("car")
install.packages("Metrics")

#Loading Packages
library(tidyverse) #to better transform and present Data
library(base) #Contains the basic functions
library(ggplot2) #for Data Visualization
library(dplyr) #to make data manipulation
library(broom) #takes messy output of built in functions
library(ggpubr) #for better creation of beautiful ggplot2 based graphs
library(caTools) #for basic utility function
library(predict3d) #to predicts plots
library(corrplot) #to visualize correlation matrix
library(car) #provides tools for regression analysis
library(Metrics) #Provides Metrics for Model

#Reading the Dataset and storing it in BMIData Variable
BMIData<-read.csv("BMI.csv")


#Displaying First 5 Records in BMI Dataset
head(BMIData)

#Displaying Last 5 Records in BMI Dataset
tail(BMIData)

#Checking the Dimensions of BMI Dataset
dim(BMIData)

#Summary of BMI Dataset
summary(BMIData)


#Calculating BMI and Addition of BMI Column to the above Data
BMIData<-mutate(BMIData,BMI=Weight/(Height)^2)
head(BMIData)


#Adding result column to the Data to know the obesity level by applying 
#if else condition on BMI column of the BMIData
T<-BMIData$BMI
Result<- ifelse(T<18.5,"Under Weight",
         ifelse(T>=18.5 & T<25,"Normal Weight",
         ifelse(T>=25 & T<30,"Over Weight",
         ifelse(T>=30 & T<35,"Obesity Class-I",
         ifelse(T>=35 & T<40,"Obesity Class-II","Obesity Class-III")))))

#Adding Result Column to Data
BMIData<-mutate(BMIData,Result)
head(BMIData)
tail(BMIData)

#Spliting the Dataset into test and train
ind=sample.split(Y=BMIData$id,SplitRatio=0.6)

#subsetting into Train data
BMItrain=BMIData[ind,]

#subsetting into Test data
BMItest=BMIData[!ind,]

#Checking the Dimnsions of Train and Test
dim(BMItrain)
dim(BMItest)


#Checking Assumptions of Multi linear Regression
#1.Independence of Observations
cor(BMItrain$Height,BMItrain$Weight)

#2.Normality
hist(BMItrain$BMI)

#3.Linearity
#We can Check using the Scatter Plot
plot(BMI~Age,data=BMItrain,main="BMI VS Age")

plot(BMI~Height,data=BMItrain,main="BMI VS Height")

plot(BMI~Weight,data=BMItrain,main="BMI VS Weight")

#Multi Collinearity Check

# Remove the Categorical Variable Value column
reduced_data<-subset(BMItrain,select = -Gender)
reduced_data<-subset(reduced_data,select = -Result)
reduced_data<-subset(reduced_data,select = -id)
corr_matrix<-round(cor(reduced_data), 2)

# Compute the Correlation Matrix
corrplot(corr_matrix,method="color",addCoef.col="black",tl.col="black", order="hclust", tl.srt=30,insig="blank",main="Correlation Matrix")


#Multi linear Regression Model Building

#Gender and Result are Categorical variable so we can avoid it and id is just for count so we no need to include it
BMIPredictModel<-lm(BMI~Age+Weight+Height,data=BMItrain)
summary(BMIPredictModel)

#Removing Age Since becauase of it p value
BMIPredictModel<-lm(BMI~Weight+Height,data=BMItrain)
summary(BMIPredictModel)

#Added Variable plots for the BMIPrediction Regression Model
avPlots(BMIPredictModel)

#Checking for the Homoscedasicity
par(mfrow=c(2,2))
plot(BMIPredictModel)
par(mfrow=c(1,1))


#Data Visualization
#Plotting BMIPrediction Model using ggPredict as the title Changes in BMI as a function of Height and Weight
ggPredict(BMIPredictModel,interactive=TRUE)

#Predicting Values Based on linear Model
BMItest$BMI.Predicted<-predict.lm(BMIPredictModel,newdata=BMIData)

#Rounding the Values of Height
BMItest$Height<-round(as.numeric(BMItest$Height),digits=1)

#Change the Height Variable into a factor
BMItest$Height<-as.factor(BMItest$Height)

#Plot the Original Data

BMI.plot<-ggplot(BMItest,aes(y=BMI.Predicted,x=Weight,color=Height))+geom_point()+stat_smooth(method="lm",se=FALSE)+theme_bw()+labs(title="Changes in BMI as a Function of Weight and Height")
BMI.plot

#Plotting Predicted vs Actual
ggplot(BMItrain,aes(x=BMItrain$BMI,y=predict(BMIPredictModel)))+geom_point()+geom_abline(intercept=0,slope=1,col="red")+labs(x="Actual BMI Values",y="Predicted BMI Values",title="Actual BMI VS Predict BMI")+theme_bw()


#RMSE of Model
rmse(BMItest$BMI,BMItest$BMI.Predicted)
