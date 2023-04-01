---
  title: "KNN"
author: "UNTGH-AA/MF"
date: "2023-02-16"
output:
  pdf_document: default
html_document: default
word_document: default
---
  
  KNN algorithm , K nearest neighbors algorithm is also a supervised learning method that categorizes data into classes. An object is being assigned to a class most common among its k nearest neighbors 

We will be using this for classifying our driver status for drivers at Alpha bus company, which we can now use for prediction. Driver status tells whether a driver leaves or remains with the company.

Alpha Driver data table has 315 observations. The objective of this analysis is to classify the data and find patterns/factors why drivers leave the company.

Drivers in this analysis were hired in 2022, and worked for the company for 30days and over. Also included in the data is demographic data and schedule data.

#Reading the data
```{r}
library(iml)
library(caret)
library(readxl)

driverdatatab<-read_xlsx("C:\\Users\\aoa0139\\OneDrive - UNT System\\Ade PhD\\RA\\Driver Attrition\\driverdatatable.xlsx")

driverdatatab<- (driverdatatab [9:15])
```
#Datacleaning and feature scaling. 
This data scaling improves model accuracy.


```{r}
driverdatatab<-fastDummies::dummy_cols(driverdatatab)
driverdata<-driverdatatab[c(4:13)]
driverdata

library(dplyr)
driverdata <- driverdata %>%           # Applying functions of dplyr
  mutate_at(c("Workhrdy", "AGE","TrfTime"), ~(scale(.) %>% as.vector))
head(driverdata)

```

Split data to Train and Test sets

```{r}
#set.seed(259)
dd <- sample(1:nrow(driverdata),size=nrow(driverdata)*0.75,replace = FALSE) #random selection of 75% data.

train.dd <- driverdata[dd,] # 75% training data
test.dd <- driverdata[-dd,] # remaining 25% test data


```

Identifying target variable

```{r}
train_status<-driverdata[dd,4]
test_status<-driverdata[-dd,4]
```

Build KNN model. To find optimal number of K, we take the square root of observations in the train data.

```{r}
set.seed(259)
library(class)
knn.15<-knn(train=train.dd, test = test.dd,cl=train.dd$Status,k=15)

```

```{r}
tab<-table(knn.15, test.dd$Status)




confusionMatrix(table(knn.15, test.dd$Status))
```

Looking at both k of 15, there is a 87% accuracy, 10 of the testdata was misclassified.


