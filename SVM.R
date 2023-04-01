---
  title: "Support Vector Classification"
author: "UNTGH-AA/MF"
date: "2023-02-14"
output:
  pdf_document: default
html_document: default
---
  Support vector is an algorithm that could be used for both regression and classification.The support vector uses a hyperplane (line) to seperate datapoints into different classes. 

We will be using this for classifying our driver status for drivers at Alpha bus company, which we can now use for prediction. Driver status tells whether a driver leaves or remains with the company.

Alpha Driver data table has 315 observations. The objective of this analysis is to classify the data and find patterns/factors why drivers leave the company.

Drivers in this analysis were hired in 2022, and worked for the company for 30days and over.Also included in the data is demographic data and schedule data.

```{r}
library(caret)
library(readxl)
driverdatatab<-read_xlsx("C:\\Users\\aoa0139\\OneDrive - UNT System\\Ade PhD\\RA\\Driver Attrition\\driverdatatable.xlsx")

driverdatatab<- (driverdatatab [9:15])
```
Demographic data and schedule data was used in the analysis.
```{r}
driverdatatab<-fastDummies::dummy_cols(driverdatatab)
driverdata<-driverdatatab[c(4:13)]
driverdata
```

```{r}
driverdata$Status<- factor(driverdata$Status, levels = c(0,1))


```
I will then split the dataset into training and testing portion. The training sample is used to build the model, while the testing sample will be used to validate the model.

To transform data to same range scale, i will apply the feature scaling to the dataset.

```{r}
library(dplyr)
driverdata <- driverdata %>%           # Applying functions of dplyr
  mutate_at(c("Workhrdy", "AGE","TrfTime"), ~(scale(.) %>% as.vector))
head(driverdata)
```



```{r}
library(caTools)
set.seed(259)
split<- sample.split(driverdata$Status, SplitRatio =0.75)

Training<-subset(driverdata,split == TRUE)
Test_data<- subset(driverdata,split ==FALSE)

```





Fitting the SVM model

```{r}
library(e1071)

driverdataclass <- svm(formula = Status~., data=Training, type= 'C-classification', kernel= 'linear', cost = 0.5, scale = FALSE)
driverdatatuned<-tune.svm(Status~., data=Training, kernel= 'linear', cost =2^(-4:8))
```


```{r}
driverdatapred<- predict(driverdataclass, newdata = Test_data[-4])





drivercm<- table(Test_data$Status,driverdatapred)
drivercm
```

```{r}
sensitivity(Test_data$Status,driverdatapred)
specificity(Test_data$Status,driverdatapred)

```
About 63% of the data is correctly classified leaving an error rate of 37%
  