library(tidyr)
library(dplyr)
library(readr)
library(forcats)
library(tableone)
library(naniar)
library(mice)
library(xgboost)
library(ggplot2)
library(caret)
library(Hmisc)
library(pROC)
library(ROCR)
library(gbm)
library(SHAPforxgboost)
library(data.table)
library(shapviz)
library(reshape2)
library(lubridate)
library(corrplot)
library(epiR)
library(ROSE)
library(kernelshap)
library(shapr)
library(fastshap)
library(geepack)
library(gridExtra)
library(ggpubr)
library(grid)
library(mRMRe)

setwd("C:/Anya/IMS PhD/Thesis/vaping dependence cohort")

#load data
df0 <- read.csv('imputed_vdc1.csv')
summary(df0)
str(df0)
df0$X <- NULL
df0$ID<- NULL
#n=6435, p=36


###Building model with predictor SHAP value threshold >0.1
df<- df0 %>% select(ecu8, ecu16, psecdi, eds, epp7, epp10, kab1nonecig, kab1necig, ecu17, alc, race,
                    resp, ecu20a, osu15, kab3, quit)

df[,c(2,3,5:16)] <- lapply(df[,c(2,3,5:16)], as.factor)

str(df)
#n=6435, p=16

####Random Forest on initial dataset###########
set.seed(123)
splitIndex.df <- createDataPartition(df$quit,p=0.8,
                                     list=FALSE,times=1)
train.df <- df[splitIndex.df,]
#n=5149, p=16
test.df <- df[-splitIndex.df,]
#n=1286, p=16

table(train.df$quit)
#data imbalance, using oversampling
set.seed(123)
overTrain.df <- ROSE(quit ~ ., data = train.df, seed = 123)$data
table(overTrain.df$quit)

## random foresting
tunegrid <- expand.grid(.mtry = (1:15)) 

trControl <- trainControl(method = "cv", number = 10, search = "grid")

set.seed(123)
rf <- train(quit ~ ., data = overTrain.df, method = "rf", metric = "Accuracy",
            trControl = trControl, tuneGrid = tunegrid, ntree = 500)
print(rf)
#best mtry 7, optimal accuracy 0.83
##using it for final model
set.seed(123)
rf <- train(quit ~ ., data = overTrain.df, method = "rf", metric = "Accuracy",
            trControl = trControl, tuneGrid = rf$bestTune, ntree= 500)
print(rf)
#Accuracy on training set 0.83

## get prediction
prediction_rf <- predict(rf, test.df)
tb_rf <- table(prediction_rf, test.df$quit)
confusionMatrix(tb_rf)
#accuracy 0.75 (95% CI 0.72, 0.77)
#sensitivity 0.80, specificity 0.47

## need pred prob
pred_rf <- predict(rf, newdata = test.df, type = "prob")
pred.class_rf <- prediction(pred_rf[, 2], test.df$quit)
perf_rf <- performance(pred.class_rf,measure = "tpr",x.measure= "fpr")
AUC_rf <- performance(pred.class_rf, 'auc')@y.values
print(AUC_rf)
#AUC 0.705

###Applying mRMR 
#converting all to numeric variables
quit <- df[, "quit"]
encoded_vars <- df[, !names(df) %in% c("quit")]
dummy_model <- dummyVars(~ ., data = encoded_vars, fullRank = TRUE) 
df_onehot <- predict(dummy_model, newdata = encoded_vars)
df_onehot <- as.data.frame(df_onehot)
df_mrmr <- cbind(quit, df_onehot)
df_mrmr$quit<- as.numeric(as.character(df_mrmr$quit))
str(df_mrmr)
#n=6435, p=29

###mRMR
mrmr_data <- mRMR.data(data = df_mrmr)
set.seed(123)
mrmr_solution <- mRMR.classic(data = mrmr_data,
                              target_indices = 1,
                              feature_count = 8)
selected_idx <- solutions(mrmr_solution)[[1]]
selected_features <- featureNames(mrmr_data)[selected_idx]
selected_features
#Reatined 8 features are ecu8, osu15.1, kab1nonecig1, epp10.2, ecu17.3, race.1, ecu16.1, and ecu20a.1
##reatining only the selected 8 features
df1<- df %>% select(ecu8, ecu16, epp10, osu15, kab1nonecig, ecu17, race, ecu20a, quit)
str(df1)
#n=6435, p=9

###Building model with slected 8 features from mRMR
####Random Forest###########
set.seed(123)
splitIndex.df <- createDataPartition(df1$quit,p=0.8,
                                     list=FALSE,times=1)
train.df1 <- df1[splitIndex.df,]
#n=5149, p=9
test.df1 <- df1[-splitIndex.df,]
#n=1286, p=9

table(train.df1$quit)
#data imbalance, using oversampling
set.seed(123)
overTrain.df1 <- ROSE(quit ~ ., data = train.df1, seed = 123)$data
table(overTrain.df1$quit)

## random foresting
tunegrid <- expand.grid(.mtry = (1:15)) 

trControl <- trainControl(method = "cv", number = 10, search = "grid")

set.seed(123)
rf1 <- train(quit ~ ., data = overTrain.df1, method = "rf", metric = "Accuracy",
            trControl = trControl, tuneGrid = tunegrid, ntree = 500)
print(rf1)
#best mtry 4, optimal accuracy 0.71
##using it for final model
set.seed(123)
rf1 <- train(quit ~ ., data = overTrain.df1, method = "rf", metric = "Accuracy",
            trControl = trControl, tuneGrid = rf1$bestTune, ntree= 500)
print(rf1)
#Accuracy on training set 0.71

## get prediction
prediction_rf1 <- predict(rf1, test.df1)
tb_rf1 <- table(prediction_rf1, test.df1$quit)
confusionMatrix(tb_rf1)
#accuracy 0.75 (95% CI 0.73, 0.78)
#sensitivity 0.79, specificity 0.55

## need pred prob
pred_rf1 <- predict(rf1, newdata = test.df1, type = "prob")
pred.class_rf1 <- prediction(pred_rf1[, 2], test.df1$quit)
perf_rf1 <- performance(pred.class_rf1,measure = "tpr",x.measure= "fpr")
AUC_rf1 <- performance(pred.class_rf1, 'auc')@y.values
print(AUC_rf1)
#AUC 0.712


###Building model with predictor SHAP value threshold >0.3
df2<- df0 %>% select(ecu8, ecu16, psecdi, eds, quit)

df2[,c(2,3,5)] <- lapply(df2[,c(2,3,5)], as.factor)

str(df2)
#n=6435, p=5

####Random Forest###########
set.seed(123)
splitIndex.df <- createDataPartition(df2$quit,p=0.8,
                                     list=FALSE,times=1)
train.df2 <- df2[splitIndex.df,]
#n=5149, p=5
test.df2 <- df2[-splitIndex.df,]
#n=1286, p=5

table(train.df2$quit)
#data imbalance, using oversampling
set.seed(123)
overTrain.df2 <- ROSE(quit ~ ., data = train.df2, seed = 123)$data
table(overTrain.df2$quit)

## random foresting
tunegrid <- expand.grid(.mtry = (1:15)) 

trControl <- trainControl(method = "cv", number = 10, search = "grid")

set.seed(123)
rf2 <- train(quit ~ ., data = overTrain.df2, method = "rf", metric = "Accuracy",
             trControl = trControl, tuneGrid = tunegrid, ntree = 500)
print(rf2)
#best mtry 3, optimal accuracy 0.68
##using it for final model
set.seed(123)
rf2 <- train(quit ~ ., data = overTrain.df2, method = "rf", metric = "Accuracy",
             trControl = trControl, tuneGrid = rf2$bestTune, ntree= 500)
print(rf2)
#Accuracy on training set 0.69

## get prediction
prediction_rf2 <- predict(rf2, test.df2)
tb_rf2 <- table(prediction_rf2, test.df2$quit)
confusionMatrix(tb_rf2)
#accuracy 0.74 (95% CI 0.72, 0.77)
#sensitivity 0.78, specificity 0.56

## need pred prob
pred_rf2 <- predict(rf2, newdata = test.df2, type = "prob")
pred.class_rf2 <- prediction(pred_rf2[, 2], test.df2$quit)
perf_rf2 <- performance(pred.class_rf2,measure = "tpr",x.measure= "fpr")
AUC_rf2 <- performance(pred.class_rf2, 'auc')@y.values
print(AUC_rf2)
#AUC 0.730

###Building model with common predictors between all models
df3<- df0 %>% select(ecu8, ecu16, psecdi, eds, epp7, epp10, kab1necig, ecu17, age, quit)

df3[,c(2,3,5:8,10)] <- lapply(df3[,c(2,3,5:8,10)], as.factor)

str(df3)
#n=6435, p=10

####Random Forest###########
set.seed(123)
splitIndex.df <- createDataPartition(df3$quit,p=0.8,
                                     list=FALSE,times=1)
train.df3 <- df3[splitIndex.df,]
#n=5149, p=9
test.df3 <- df3[-splitIndex.df,]
#n=1286, p=9

table(train.df3$quit)
#data imbalance, using oversampling
set.seed(123)
overTrain.df3 <- ROSE(quit ~ ., data = train.df3, seed = 123)$data
table(overTrain.df3$quit)

## random foresting
tunegrid <- expand.grid(.mtry = (1:15)) 

trControl <- trainControl(method = "cv", number = 10, search = "grid")

set.seed(123)
rf3 <- train(quit ~ ., data = overTrain.df3, method = "rf", metric = "Accuracy",
             trControl = trControl, tuneGrid = tunegrid, ntree = 500)
print(rf3)
#best mtry 4, optimal accuracy 0.75
##using it for final model
set.seed(123)
rf3 <- train(quit ~ ., data = overTrain.df3, method = "rf", metric = "Accuracy",
             trControl = trControl, tuneGrid = rf3$bestTune, ntree= 500)
print(rf3)
#Accuracy on training set 0.75

## get prediction
prediction_rf3 <- predict(rf3, test.df3)
tb_rf3 <- table(prediction_rf3, test.df3$quit)
confusionMatrix(tb_rf3)
#accuracy 0.74(95% CI 0.71, 0.76)
#sensitivity 0.78, specificity 0.50

## need pred prob
pred_rf3 <- predict(rf3, newdata = test.df3, type = "prob")
pred.class_rf3 <- prediction(pred_rf3[, 2], test.df3$quit)
perf_rf3 <- performance(pred.class_rf3,measure = "tpr",x.measure= "fpr")
AUC_rf3 <- performance(pred.class_rf3, 'auc')@y.values
print(AUC_rf3)
#AUC 0.719

###Highest AUC with threshold >0.3, trying with only 2 predictors: frequency and perceived addiction
df4<- df0 %>% select(ecu8, ecu16, quit)

df4[,c(2,3)] <- lapply(df4[,c(2,3)], as.factor)

str(df4)
#n=6435, p=3

####Random Forest###########
set.seed(123)
splitIndex.df <- createDataPartition(df4$quit,p=0.8,
                                     list=FALSE,times=1)
train.df4 <- df4[splitIndex.df,]
#n=5149, p=3
test.df4 <- df4[-splitIndex.df,]
#n=1286, p=3

table(train.df4$quit)
#data imbalance, using oversampling
set.seed(123)
overTrain.df4 <- ROSE(quit ~ ., data = train.df4, seed = 123)$data
table(overTrain.df4$quit)

## random foresting
tunegrid <- expand.grid(.mtry = (1:15)) 

trControl <- trainControl(method = "cv", number = 10, search = "grid")

set.seed(123)
rf4 <- train(quit ~ ., data = overTrain.df4, method = "rf", metric = "Accuracy",
             trControl = trControl, tuneGrid = tunegrid, ntree = 500)
print(rf4)
#best mtry 2, optimal accuracy 0.68
##using it for final model
set.seed(123)
rf4 <- train(quit ~ ., data = overTrain.df4, method = "rf", metric = "Accuracy",
             trControl = trControl, tuneGrid = rf4$bestTune, ntree= 500)
print(rf4)
#Accuracy on training set 0.68

## get prediction
prediction_rf4 <- predict(rf4, test.df4)
tb_rf4 <- table(prediction_rf4, test.df4$quit)
confusionMatrix(tb_rf4)
#accuracy 0.67 (95% CI 0.64, 0.70)
#sensitivity 0.67, specificity 0.69

## need pred prob
pred_rf4 <- predict(rf4, newdata = test.df4, type = "prob")
pred.class_rf4 <- prediction(pred_rf4[, 2], test.df4$quit)
perf_rf4 <- performance(pred.class_rf4,measure = "tpr",x.measure= "fpr")
AUC_rf4 <- performance(pred.class_rf4, 'auc')@y.values
print(AUC_rf4)
#AUC 0.726

###trying with only 1 predictor: frequency
df5<- df0 %>% select(ecu8, quit)

df5$quit<- as.factor(df5$quit)

str(df5)
#n=6435, p=2

####Random Forest###########
set.seed(123)
splitIndex.df <- createDataPartition(df5$quit,p=0.8,
                                     list=FALSE,times=1)
train.df5 <- df5[splitIndex.df,]
#n=5149, p=2
test.df5 <- df5[-splitIndex.df,]
#n=1286, p=2

table(train.df5$quit)
#data imbalance, using oversampling
set.seed(123)
overTrain.df5 <- ROSE(quit ~ ., data = train.df5, seed = 123)$data
table(overTrain.df5$quit)

## random foresting
tunegrid <- expand.grid(.mtry = (1:15)) 

trControl <- trainControl(method = "cv", number = 10, search = "grid")

set.seed(123)
rf5 <- train(quit ~ ., data = overTrain.df5, method = "rf", metric = "Accuracy",
             trControl = trControl, tuneGrid = tunegrid, ntree = 500)
print(rf5)
#best mtry 1, optimal accuracy 0.60
##using it for final model
set.seed(123)
rf5 <- train(quit ~ ., data = overTrain.df5, method = "rf", metric = "Accuracy",
             trControl = trControl, tuneGrid = rf5$bestTune, ntree= 500)
print(rf5)
#Accuracy on training set 0.60

## get prediction
prediction_rf5 <- predict(rf5, test.df5)
tb_rf5 <- table(prediction_rf5, test.df5$quit)
confusionMatrix(tb_rf5)
#accuracy 0.72 (95% CI 0.70, 0.75)
#sensitivity 0.75, specificity 0.57

## need pred prob
pred_rf5 <- predict(rf5, newdata = test.df5, type = "prob")
pred.class_rf5 <- prediction(pred_rf5[, 2], test.df5$quit)
perf_rf5 <- performance(pred.class_rf5,measure = "tpr",x.measure= "fpr")
AUC_rf5 <- performance(pred.class_rf5, 'auc')@y.values
print(AUC_rf5)
#AUC 0.696

###trying with only 1 predictor: frequency
df6<- df0 %>% select(psecdi, quit)

df6$quit<- as.factor(df6$quit)
df6$psecdi<- as.factor(df6$psecdi)

str(df6)
#n=6436, p=2

####Random Forest###########
set.seed(123)
splitIndex.df <- createDataPartition(df6$quit,p=0.8,
                                     list=FALSE,times=1)
train.df6 <- df6[splitIndex.df,]
#n=5149, p=2
test.df6 <- df6[-splitIndex.df,]
#n=1286, p=2

table(train.df6$quit)
#data imbalance, using oversampling
set.seed(123)
overTrain.df6 <- ROSE(quit ~ ., data = train.df6, seed = 123)$data
table(overTrain.df6$quit)

## random foresting
tunegrid <- expand.grid(.mtry = (1:16)) 

trControl <- trainControl(method = "cv", number = 10, search = "grid")

set.seed(123)
rf6 <- train(quit ~ ., data = overTrain.df6, method = "rf", metric = "Accuracy",
             trControl = trControl, tuneGrid = tunegrid, ntree = 600)
print(rf6)
#best mtry 3, optimal accuracy 0.63
##using it for final model
set.seed(123)
rf6 <- train(quit ~ ., data = overTrain.df6, method = "rf", metric = "Accuracy",
             trControl = trControl, tuneGrid = rf6$bestTune, ntree= 600)
print(rf6)
#Accuracy on training set 0.63

## get prediction
prediction_rf6 <- predict(rf6, test.df6)
tb_rf6 <- table(prediction_rf6, test.df6$quit)
confusionMatrix(tb_rf6)
#accuracy 0.76 (96% CI 0.74, 0.78)
#sensitivity 0.81, specificity 0.49

## need pred prob
pred_rf6 <- predict(rf6, newdata = test.df6, type = "prob")
pred.class_rf6 <- prediction(pred_rf6[, 2], test.df6$quit)
perf_rf6 <- performance(pred.class_rf6,measure = "tpr",x.measure= "fpr")
AUC_rf6 <- performance(pred.class_rf6, 'auc')@y.values
print(AUC_rf6)
#AUC 0.692

###Model with single predictor: eds
df7<- df0 %>% select(eds, quit)

df7$quit<- as.factor(df7$quit)

str(df7)
#n=6435, p=2

####Random Forest###########
set.seed(123)
splitIndex.df <- createDataPartition(df7$quit,p=0.8,
                                     list=FALSE,times=1)
train.df7 <- df7[splitIndex.df,]
#n=5149, p=2
test.df7 <- df7[-splitIndex.df,]
#n=1286, p=2

table(train.df7$quit)
#data imbalance, using oversampling
set.seed(123)
overTrain.df7 <- ROSE(quit ~ ., data = train.df7, seed = 123)$data
table(overTrain.df7$quit)

## random foresting
tunegrid <- expand.grid(.mtry = (1:17)) 

trControl <- trainControl(method = "cv", number = 10, search = "grid")

set.seed(123)
rf7 <- train(quit ~ ., data = overTrain.df7, method = "rf", metric = "Accuracy",
             trControl = trControl, tuneGrid = tunegrid, ntree = 700)
print(rf7)
#best mtry 1, optimal accuracy 0.56
##using it for final model
set.seed(123)
rf7 <- train(quit ~ ., data = overTrain.df7, method = "rf", metric = "Accuracy",
             trControl = trControl, tuneGrid = rf7$bestTune, ntree= 700)
print(rf7)
#Accuracy on training set 0.56

## get prediction
prediction_rf7 <- predict(rf7, test.df7)
tb_rf7 <- table(prediction_rf7, test.df7$quit)
confusionMatrix(tb_rf7)
#accuracy 0.64 (97% CI 0.61, 0.66)
#sensitivity 0.63, specificity 0.64

## need pred prob
pred_rf7 <- predict(rf7, newdata = test.df7, type = "prob")
pred.class_rf7 <- prediction(pred_rf7[, 2], test.df7$quit)
perf_rf7 <- performance(pred.class_rf7,measure = "tpr",x.measure= "fpr")
AUC_rf7 <- performance(pred.class_rf7, 'auc')@y.values
print(AUC_rf7)
#AUC 0.655

###Model with single predictor: self-perceived addiction
df8<- df0 %>% select(ecu16, quit)

df8$quit<- as.factor(df8$quit)
df8$ecu16<- as.factor(df8$ecu16)

str(df8)
#n=6435, p=2

####Random Forest###########
set.seed(123)
splitIndex.df <- createDataPartition(df8$quit,p=0.8,
                                     list=FALSE,times=1)
train.df8 <- df8[splitIndex.df,]
#n=5149, p=2
test.df8 <- df8[-splitIndex.df,]
#n=1286, p=2

table(train.df8$quit)
#data imbalance, using oversampling
set.seed(123)
overTrain.df8 <- ROSE(quit ~ ., data = train.df8, seed = 123)$data
table(overTrain.df8$quit)

## random foresting
tunegrid <- expand.grid(.mtry = (1:18)) 

trControl <- trainControl(method = "cv", number = 10, search = "grid")

set.seed(123)
rf8 <- train(quit ~ ., data = overTrain.df8, method = "rf", metric = "Accuracy",
             trControl = trControl, tuneGrid = tunegrid, ntree = 800)
print(rf8)
#best mtry 1, optimal accuracy 0.64
##using it for final model
set.seed(123)
rf8 <- train(quit ~ ., data = overTrain.df8, method = "rf", metric = "Accuracy",
             trControl = trControl, tuneGrid = rf8$bestTune, ntree= 800)
print(rf8)
#Accuracy on training set 0.64

## get prediction
prediction_rf8 <- predict(rf8, test.df8)
tb_rf8 <- table(prediction_rf8, test.df8$quit)
confusionMatrix(tb_rf8)
#accuracy 0.74 (98% CI 0.71, 0.76)
#sensitivity 0.78, specificity 0.53

## need pred prob
pred_rf8 <- predict(rf8, newdata = test.df8, type = "prob")
pred.class_rf8 <- prediction(pred_rf8[, 2], test.df8$quit)
perf_rf8 <- performance(pred.class_rf8,measure = "tpr",x.measure= "fpr")
AUC_rf8 <- performance(pred.class_rf8, 'auc')@y.values
print(AUC_rf8)
#AUC 0.656




###Final model is rf2
###SHAP analysis
pred_fun <- function(model, newdata) {
  probs <- predict(model, newdata, type = "prob")
  if (is.null(probs) || any(is.na(probs))) {
    stop("Prediction function returned invalid probabilities")
  }
  return(probs[, "1"])  # Return the probabilities for the '1' class
}
set.seed(123)
X <- test.df2[, -which(names(test.df2) == "quit")] 
bg_X <- X[sample(1:nrow(X), size = 100), ]
shap_values<- kernelshap(rf2, X = X, bg_X = bg_X, pred_fun = pred_fun)
print(shap_values)
shp.rf <- shapviz(shap_values)
str(shp.rf$X)

#renaming features
rename_map <- c(
  ecu8='Frequency of vaping',
  ecu16='Self-perceived addiction to vaping',
  psecdi="Dependence (PSECDI)",
  eds='Dependence (EDS score)'
)

colnames(shp.rf$X)[colnames(shp.rf$X) %in% names(rename_map)] <- rename_map[colnames(shp.rf$X) %in% names(rename_map)]
str(shp.rf$X)
colnames(shp.rf[["S"]])[colnames(shp.rf[["S"]]) %in% names(rename_map)] <- rename_map[colnames(shp.rf[["S"]]) %in% names(rename_map)]
colnames(shp.rf[["S"]])

#Feature importance
shp.rf.imp<- sv_importance(shp.rf)
rf.imp<- data.frame(shp.rf.imp$data)
rf.imp$value <- round(rf.imp$value,3)

G1<- ggplot(data=rf.imp,aes(x=reorder(feature,value),y=value))+
  geom_bar(fill= "navyblue", stat="identity")+
  geom_text(aes(label= value), y=0.02,color="white",size=3)+
  coord_flip()+
  labs(y="mean SHAP value",
       x= "")+
  theme(text = element_text(size= 12), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
print(G1)
shp.rf.summary<- sv_importance(shp.rf, kind = "beeswarm", show_numbers = TRUE)
print(shp.rf.summary)

###Depedence plot
p1<- sv_dependence(shp.rf, v = "Frequency of vaping", color_var = NULL)

p2<- sv_dependence(shp.rf, v = "Self-perceived addiction to vaping", color_var = NULL)
p2$data$"Self-perceived addiction to vaping"<- factor(p2$data$"Self-perceived addiction to vaping" , levels= c(0,1,2), 
                                                      labels= c('Not addicted', 'Addicted', 'Unknown'))

p3<- sv_dependence(shp.rf, v = "Dependence (PSECDI)", color_var = NULL)
p3$data$"Dependence (PSECDI)" <- factor(p3$data$"Dependence (PSECDI)", levels= c(0,1,2,3), 
                                        labels= c('No', 'Low', "Moderate", "High"))

p4<- sv_dependence(shp.rf, v = "Dependence (EDS score)", color_var = NULL)

gridExtra::grid.arrange(p1, p2,p3,p4, ncol=2)

#SHAP force plot for single individual
sv_force(shp.rf, row_id = 200)
sv_waterfall(shp.rf, row_id = 650)

