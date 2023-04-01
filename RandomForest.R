library(caret)
library(rpart)
library(rpart.plot)
library(readxl)
library(randomForest)

driverdatatab<-read_xlsx("C:\\Users\\aoa0139\\OneDrive - UNT System\\Ade PhD\\RA\\Driver Attrition\\driverdatatable.xlsx")

driverdatatab<- (driverdatatab [9:15])
driverdatatab
table(driverdatatab$Status)


driverdatatab$Status<- factor(driverdatatab$Status, levels = c(0,1))

driverdatatab<-upSample(x=driverdatatab[ ,-1],y= driverdatatab$Status)

table(driverdatatab$Status)

driverdatatab<-fastDummies::dummy_cols(driverdatatab)
driverdata<-driverdatatab[c(4:13)]
driverdata


library(dplyr)
driverdata <- driverdata %>%           # Applying functions of dplyr
  mutate_at(c("Workhrdy", "AGE","TrfTime"), ~(scale(.) %>% as.vector))
head(driverdata)
colnames(driverdata)

library(caTools)
set.seed(259)
split<- sample.split(driverdata$Status, SplitRatio =0.75)

Training<-subset(driverdata,split == TRUE)
Test_data<- subset(driverdata,split ==FALSE)
colnames(Training)



model.rf<-randomForest(Status~.,data=Training, proximity=TRUE)
model.rf1<-randomForest(Status~.,data=Training,ntree= 100,  proximity=TRUE)

#model.rf<-randomForest(x=Training[-7],y=Training$Status, proximity=TRUE)


da.rf<-predict(model.rf, Test_data)
confusionMatrix(da.rf,Test_data$Status)

print(mean((da.rf - Test_data$Status)^2))

fit.caret <- train(
  Status ~ ., 
  data = Training, 
  method = 'ranger',
  trControl = trainControl(method = 'cv', number = 5), 
  preProc = c('center', 'scale')
)


# create the X and Y vectors for the training and testing datasets
tr.x <- Training %>% select(Workhrdy, AGE, TrfTime, OperClass_E, OperClass_R, Gender_F, Gender_M, MilitaryService_N, MilitaryService_Y)
tr.y <- Training %>% select(Status)
te.x <- Test_data %>% select(Workhrdy, AGE, TrfTime, OperClass_E, OperClass_R, Gender_F, Gender_M, MilitaryService_N, MilitaryService_Y)
te.y <- Test_data %>% select(Status)


# create a Predictor object as a container for the prediction model and the data
predictor <- Predictor$new(fit.caret, data = tr.x, y = tr.y)

# compute feature contributions for a single prediction as the Shapley value
shapley <- Shapley$new(predictor, x.interest = te.x[1, ])

# plot the results
shapley$plot()

# pick another observation to explain
set.seed(259)
shapley$explain(x.interest = te.x[10, ])
shapley$plot()

shap.values<-predict(fit.caret, tr.x, predcontrib = TRUE, approxcontrib = F)
plot(shap.values)

var_importance(shap.values)

imp <- FeatureImp$new(predictor, loss = "mse")
library("ggplot2")
plot(imp)
