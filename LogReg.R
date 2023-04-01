---
  title: "Driverattlog"
author: "UNTGH-AA/MF"
date: "2023-02-20"
output: html_document
---
  ```{r}
```


```{r}
library(caret)
library(readxl)

driverdatatab<-read_xlsx("C:\\Users\\aoa0139\\OneDrive - UNT System\\Ade PhD\\RA\\Driver Attrition\\driverdatatable.xlsx")

driverdatatab<- (driverdatatab [9:15])
```


```{r}
driverdatatab$Status<- factor(driverdatatab$Status, levels = c(0,1))

driverdatatab<-fastDummies::dummy_cols(driverdatatab)
driverdata<-driverdatatab[c(4:13)]
driverdata

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
Test_data<- subset(driverdata,split == FALSE)

mylogit <- glm(Status ~., data = Training, family = "binomial")
summary(mylogit)

```


prob <- predict(mylogit, Test_data, type = "response")
predicted.classes <- ifelse(prob >0.5, 1, 0)
table(predicted.classes,Test_data$Status)
confusionMatrix(data=as.factor(as.numeric(prob >0.5)), reference =Test_data$Status)





```

