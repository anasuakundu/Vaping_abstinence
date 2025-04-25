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
library(DMwR)
library(missForest)

setwd("C:/Anya/IMS PhD/Thesis/vaping dependence cohort")

#load data
df <- read.csv('imputed_vdc1.csv')
summary(df)
str(df)
df$X <- NULL
#n=6435, p=37

df[,c(3:8, 10:16, 18:31, 33:37)] <- lapply(df[,c(3:8, 10:16, 18:31, 33:37)], as.factor)

str(df)
#n=6435, p=37

#scaling numeric variables
df[,c(2,9,17,32)] <- lapply(df[,c(2,9,17,32)], function(x) c(scale(x)))
str(df)
#n=6435, p=37

###checking collinearity in dataset
df_cor<- df
df_cor$ID<- NULL
cor<- (DescTools::PairApply(df_cor, DescTools::CramerV))
corrplot(cor, method="circle")
cor <- as.data.frame(as.table(cor))
cor <- subset(cor, abs(Freq) >= 0.6)
cor<- cor %>%
  filter(Freq!=1)
write.csv(cor, 'correlation1.csv')
#Correlated variables are age, education, Gender, sexual orientation, kab3, kab4, kab1_nonecig, kab1_necig
#all correlation close to 0.6.
#keeping all of them.

###GEE model
df_gee<- df
df_gee$quit <- as.numeric(as.character(df_gee$quit))
row.names(df_gee) <- NULL
df_gee <- df_gee %>%
  rename(spending = epp13)

gee_model <- geeglm(quit ~ ., 
                    family = binomial(link = "logit"), 
                    data = df_gee, 
                    id = ID, 
                    corstr = "exchangeable")
##performance
predicted_probabilities <- predict(gee_model, df_gee, type = "response")
true_labels <- df_gee$quit
roc_gee <- roc(response = true_labels, predictor = predicted_probabilities)
auc_gee <- auc(roc_gee)
print(paste("AUC:", auc_gee))
#AUC 0.756

#Confusion matrix
threshold <- 0.5
predicted_class <- ifelse(predicted_probabilities > threshold, 1, 0)
conf_matrix <- confusionMatrix(factor(predicted_class), factor(true_labels))
print(conf_matrix)
#accuracy 0.84 (95% CI 0.83, 0.85)
#sensitivity 0.98, specificity 0.12

####Builing models
df$ID<- NULL

####Random Forest###########
set.seed(123)
splitIndex.df <- createDataPartition(df$quit,p=0.8,
                                     list=FALSE,times=1)
train.df <- df[splitIndex.df,]
#n=5149, p=37
test.df <- df[-splitIndex.df,]
#n=1286, p=37

table(train.df$quit)
table(test.df$quit)
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
#best mtry 4, optimal accuracy 0.89
##using it for final rf model
set.seed(123)
rf <- train(quit ~ ., data = overTrain.df, method = "rf", metric = "Accuracy",
                 trControl = trControl, tuneGrid = rf$bestTune, ntree= 500)
print(rf)
#Accuracy on training set 0.90

## get prediction
prediction_rf <- predict(rf, test.df)
tb_rf <- table(prediction_rf, test.df$quit)
confusionMatrix(tb_rf)
#accuracy 0.79 (95% CI 0.77, 0.81)
#sensitivity 0.86, specificity 0.44

## need pred prob
pred_rf <- predict(rf, newdata = test.df, type = "prob")
pred.class_rf <- prediction(pred_rf[, 2], test.df$quit)
perf_rf <- performance(pred.class_rf,measure = "tpr",x.measure= "fpr")
AUC_rf <- performance(pred.class_rf, 'auc')@y.values
print(AUC_rf)
#AUC 0.737

###GBM model
set.seed(123)
splitIndex.df <- createDataPartition(df$quit,p=0.8,
                                     list=FALSE,times=1)
train.df <- df[splitIndex.df,]
#n=5149, p=37
test.df <- df[-splitIndex.df,]
#n=1286, p=37

##Oversampling
overTrain.df <- ROSE(quit ~ ., data = train.df, seed = 123)$data

#parameters
train_control <- trainControl(
  method = "cv",
  number = 10,
  verboseIter = TRUE, 
  classProbs = TRUE, 
  summaryFunction = twoClassSummary
)

# Define the tuning grid for GBM
tune_grid <- expand.grid(
  interaction.depth = c(1, 3, 5),  
  n.trees = c(100, 200, 500),       
  shrinkage = c(0.01, 0.1),   
  n.minobsinnode = c(10)    
)

#ensuring output is factor variable
overTrain.df$quit <- factor(overTrain.df$quit, levels = c(0, 1), labels = c("No", "Yes"))
test.df$quit <- factor(test.df$quit, levels = c(0, 1), labels = c("No", "Yes"))

#building the model
set.seed(123)  
gbm_model <- train(
  quit ~ .,  
  data = overTrain.df,
  method = "gbm",  
  trControl = train_control,
  tuneGrid = tune_grid,
  verbose = FALSE  
)
print(gbm_model)
print(gbm_model$bestTune)
#best parameters n.trees = 500, interaction.depth = 5, shrinkage =0.1 and n.minobsinnode = 10
#using best hyperparameters for final gbm model
set.seed(123)  
gbm_model <- train(
  quit ~ .,  
  data = overTrain.df,
  method = "gbm",  
  trControl = train_control,
  tuneGrid = gbm_model$bestTune,
  verbose = FALSE  
)
print(gbm_model)
##ROC on training set 0.83

# confusion matrix
class_predictions <- predict(gbm_model, test.df)
prob_predictions <- predict(gbm_model, test.df, type = "prob")
confusion_matrix <- confusionMatrix(class_predictions, test.df$quit)
print(confusion_matrix)
#accuracy 0.77 (95% CI 0.74, 0.79)
#sensitivity 0.83, specificity 0.43

##measuring AUC
pred.gbm <- prediction(prob_predictions[ , "Yes"], test.df$quit)
perf_gbm <- performance(pred.gbm, measure = "tpr",x.measure= "fpr")
AUC_gbm <- performance(pred.gbm, 'auc')@y.values
print(AUC_gbm)
#AUC 0.696

###XGBoost model
#One hot encoding
quit <- df[, "quit"]
encoded_vars <- df[, !names(df) %in% c("quit")]
dummy_model <- dummyVars(~ ., data = encoded_vars, fullRank = TRUE) 
df_onehot <- predict(dummy_model, newdata = encoded_vars)
df_onehot <- as.data.frame(df_onehot)
df_onehot <- cbind(df_onehot, quit)
str(df_onehot)
#n=6435, p=56

#data splitting
set.seed(123)
splitIndex.df_onehot <- createDataPartition(df_onehot$quit,p=0.8,
                                     list=FALSE,times=1)
train.df_onehot <- df_onehot[splitIndex.df_onehot,]
#n=5149, p=57
test.df_onehot <- df_onehot[-splitIndex.df_onehot,]
#n=1286, p=57

#Oversampling
df_overtrain <- ROSE(quit ~ ., data = train.df_onehot, seed = 123)$data
table(df_overtrain$quit)
#level 0 2590, level 1 2559

##preparing data
X_overtrain <- as.matrix(df_overtrain[, !names(df_overtrain) %in% c("quit")])
y_overtrain <- df_overtrain$quit
X_test <- as.matrix(test.df_onehot[, !names(test.df_onehot) %in% c("quit")])
y_test <- test.df_onehot$quit


#fit the model on training set
#tuning
xgb_grid <- expand.grid(
  max_depth = c(3, 5, 7),
  nrounds = c(100, 200, 300),
  eta = c(0.01, 0.1),
  gamma = 0,
  min_child_weight = c(1, 5),
  subsample = 1,
  colsample_bytree = 1
)

train_control <- trainControl(
  method = "cv",
  number = 10, 
  verboseIter = FALSE, 
  allowParallel = TRUE 
)

set.seed(123)
xgb <- train(
  X_overtrain, y_overtrain,
  method = "xgbTree", 
  trControl = train_control,
  tuneGrid = xgb_grid,
  verbose = TRUE
)
print(xgb$bestTune)
##best hyperparameters:nrounds 300, max_depth 3, eta 0.1, gamma 0,colsample_bytree 1,min_child_weight 5,subsample 1

###Use the best tune parameters to build final model
set.seed(123)
xgb_final <- train(
  X_overtrain, y_overtrain,
  method = "xgbTree", 
  trControl = train_control,
  tuneGrid = xgb$bestTune,
  verbose = TRUE
)
print(xgb_final)
#accuracy 0.73 on tarining data

# confusion matrix
y_pred <- predict(xgb_final, X_test)
prob_predictions <- predict(xgb_final, X_test, type = "prob")
confusion_matrix <- confusionMatrix(y_pred, y_test)
print(confusion_matrix)
#accuracy 0.80 (95% CI 0.78, 0.82)
#sensitivity 0.88, specificity 0.37

##measuring AUC
pred.xgb <- prediction(prob_predictions[, 2], y_test)
perf_xgb <- performance(pred.xgb, measure = "tpr",x.measure= "fpr")
AUC_xgb <- performance(pred.xgb, 'auc')@y.values
print(AUC_xgb)
#AUC 0.726


###Plotting AUCs
tiff('AUC.past30dayabstinenceV4.tiff', height = 8, width = 8, units = 'in', res=300)
plot(perf_rf,lwd=2,col="red",
     xlab="False Positive Rate (1-Specificity)", ylab="True Positive Rate (Sensitivity)")
plot(perf_gbm,add=TRUE,lwd=2,col="blue")
plot(perf_xgb,add=TRUE,lwd=2,col="green")
abline(coef=c(0,1),lwd=1)

legend('bottomright',
       legend = c('Random Forest (AUC 0.737)',
                  'GBM (AUC 0.696)', 'XGBoost (AUC 0.726)'),
       col= c('red', 'blue', 'green'),
       lwd=2)
dev.off()

###saving final models
###GBM is the final model as of highest AUC, but keeping XGBoost as the alternate model too
saveRDS(rf, "rf_past30abstinence1.rds")
saveRDS(gbm_model, "gbm_model_past30abstinence1.rds")
saveRDS(xgb_final, "xgb_model_past30abstinence1.rds")

###SHAP analysis on RF model
#securing original train and test set
set.seed(123)
splitIndex.df <- createDataPartition(df$quit,p=0.8,
                                     list=FALSE,times=1)
train.df <- df[splitIndex.df,]
#n=5149, p=37
test.df <- df[-splitIndex.df,]
#n=1286, p=37

pred_fun <- function(model, newdata) {
  probs <- predict(model, newdata, type = "prob")
  if (is.null(probs) || any(is.na(probs))) {
    stop("Prediction function returned invalid probabilities")
  }
  return(probs[, "1"])  # Return the probabilities for the '1' class
}
set.seed(123)
X <- test.df[, -which(names(test.df) == "quit")] 
bg_X <- X[sample(1:nrow(X), size = 100), ]
shap_values<- kernelshap(rf, X = X, bg_X = bg_X, pred_fun = pred_fun)
print(shap_values)
shp.rf <- shapviz(shap_values)
str(shp.rf$X)

#renaming features
rename_map <- c(
  age='Age',
  sex= 'Sex',
  race= 'Race',
  Gender= 'Gender',
  sexorient= 'Sexual orientation',
  edu= 'Education',
  married= 'Marital status',
  ecu8='Past month frequency of vaping',
  ecu12a='Puffs per vape',
  ecu16='Self-perceived addiction to vaping',
  ecu17='Intention to quit',
  ecu20a='Seeing others vaping',
  epp1='Device type',
  epp7='Flavor used',
  epp10='Nicotine strength',
  epp13='Past month expense of vaping',
  osu15='Past 30-day binge drinking',
  csmk='Past 30-day smoking',
  can='Past 30-day cannabis use',
  alc='Past 30-day alcohol drinking',
  otob='Past 30-day other tobacco products use',
  peer='Peer vaping',
  kab1nonecig='Harm perception of non-nicotine vaping',
  kab1necig='Harm perception of nicotine vaping',
  kab3='Believing EC is less harmful than cigarette',
  kab4= 'Believing vaping can help in quitting smoking',
  ghealth='Perceived general health',
  mhealth1='Perceived mental health',
  pstress='Perceived stress',
  psecdi="Dependence (PSECDI)",
  eds='Dependence (EDS score)',
  ecu18='Past 3-months adverse effects',
  pro1='Exposure to e-cigarette marketing',
  depression='Depression', 
  resp='Past 4-months respiratory symptoms'
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
  geom_text(aes(label= value), y=0.005,color="white",size=3)+
  coord_flip()+
  labs(y="mean SHAP value",
       x= "")+
  theme(text = element_text(size= 12), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
print(G1)
shp.rf.summary<- sv_importance(shp.rf, kind = "beeswarm", show_numbers = TRUE)
print(shp.rf.summary)

#Shap dependence plot
p1<- sv_dependence(shp.rf, v = "Past month frequency of vaping", color_var = NULL)

p2<- sv_dependence(shp.rf, v = "Self-perceived addiction to vaping", color_var = NULL)
p2$data$"Self-perceived addiction to vaping"<- factor(p2$data$"Self-perceived addiction to vaping" , levels= c(0,1,2,3), 
                                                  labels= c('Not addicted', 'Less addicted', 'Very addicted', 'Unknown'))

p3<- sv_dependence(shp.rf, v = "Dependence (EDS score)", color_var = NULL)

p4<- sv_dependence(shp.rf, v = "Dependence (PSECDI)", color_var = NULL)
p4$data$"Dependence (PSECDI)" <- factor(p4$data$"Dependence (PSECDI)", levels= c(0,1,2,3), 
                                        labels= c('No', 'Low', "Moderate", "High"))

p5<- sv_dependence(shp.rf, v = "Nicotine strength", color_var = NULL)
p5$data$"Nicotine strength" <- factor(p5$data$"Nicotine strength", levels= c(0,1,2,3), 
                                      labels= c('None', 'Low', "High","Unknown"))

p6<- sv_dependence(shp.rf, v = "Harm perception of non-nicotine vaping", color_var = NULL)
p6$data$"Harm perception of non-nicotine vaping" <- factor(p6$data$"Harm perception of non-nicotine vaping", levels= c(0,1,2), 
                                                           labels= c('No risk', 'Risky', "Unknown"))

p7<- sv_dependence(shp.rf, v = "Flavor used", color_var = NULL)
p7$data$"Flavor used" <- factor(p7$data$"Flavor used", levels= c(0,1,2,3,4), 
                                labels= c('Tobacco', 'Fruit or sweet', "Menthol/mint", 
                                          "Food or beeverage", "Unknown"))

p8<- sv_dependence(shp.rf, v = "Intention to quit", color_var = NULL)
p8$data$"Intention to quit"<- factor(p8$data$"Intention to quit" , levels= c(1,2,3), 
                                     labels= c('No', 'After 30 days', 'By 30 days'))

p9<- sv_dependence(shp.rf, v = "Harm perception of nicotine vaping", color_var = NULL)
p9$data$"Harm perception of nicotine vaping" <- factor(p9$data$"Harm perception of nicotine vaping", levels= c(0,1,2), 
                                                        labels= c('No risk', 'Risky', "Unknown"))

p10<- sv_dependence(shp.rf, v = "Past 30-day alcohol drinking", color_var = NULL)
p10$data$"Past 30-day alcohol drinking" <- factor(p10$data$"Past 30-day alcohol drinking", 
                                                  levels= c(0,1), labels= c('No', 'Yes'))

p11<- sv_dependence(shp.rf, v = "Past 4-months respiratory symptoms", color_var = NULL)
p11$data$"Past 4-months respiratory symptoms" <- factor(p11$data$"Past 4-months respiratory symptoms",
                                          levels= c(0,1), labels= c('No', 'Yes'))

p12<- sv_dependence(shp.rf, v = "Seeing others vaping", color_var = NULL)
p12$data$"Seeing others vaping" <- factor(p12$data$"Seeing others vaping",
                                          levels= c(0,1), labels= c('No', 'Yes'))

p13<- sv_dependence(shp.rf, v = "Past 30-day binge drinking", color_var = NULL)
p13$data$"Past 30-day binge drinking" <- factor(p13$data$"Past 30-day binge drinking", 
                                                  levels= c(0,1), labels= c('No', 'Yes'))

p14<- sv_dependence(shp.rf, v = "Believing EC is less harmful than cigarette", color_var = NULL)
p14$data$"Believing EC is less harmful than cigarette" <- factor(p14$data$"Believing EC is less harmful than cigarette", 
                                                                 levels= c(0,1,2), labels= c('Agree', 'Disagree', "Unknown"))


gridExtra::grid.arrange(p1,p2,p3,p4,p5,p7, ncol=2)
gridExtra::grid.arrange(p6,p8,p9,p14, ncol=2)
gridExtra::grid.arrange(p10,p11,p12,p13, ncol=2)


###socio-demographics one way dependence plots
g1<- sv_dependence(shp.rf, v = "Age", color_var = NULL)

g2<- sv_dependence(shp.rf, v = "Sex", color_var = NULL)
g2$data$"Sex" <- factor(g2$data$"Sex", levels= c(0,1,2), 
                        labels= c('Male', 'Female','Undisclosed'))

g3<- sv_dependence(shp.rf, v = "Race", color_var = NULL)
g3$data$"Race" <- factor(g3$data$"Race", levels= c(0,1), 
                         labels= c('Non-White', 'White'))

g4<- sv_dependence(shp.rf, v = "Gender", color_var = NULL)
g4$data$"Gender" <- factor(g4$data$"Gender", levels= c(0,1,2), 
                           labels= c('Cisgender', 'Others', 'Undisclosed'))

g5<- sv_dependence(shp.rf, v = "Sexual orientation", color_var = NULL)
g5$data$"Sexual orientation" <- factor(g5$data$"Sexual orientation", levels= c(0,1,2), 
                                       labels= c('Heterosexual', '2SLGBTQ+', 'Undisclosed'))

g6<- sv_dependence(shp.rf, v = "Education", color_var = NULL)
g6$data$"Education" <- factor(g6$data$"Education", levels= c(0,1,2), 
                              labels= c('< High school', 'High school', '> High School'))

g7<- sv_dependence(shp.rf, v = "Marital status", color_var = NULL)
g7$data$"Marital status" <- factor(g7$data$"Marital status", levels= c(0,1), 
                                   labels= c('No', 'Yes'))


pdp1<- gridExtra::grid.arrange(g1, g2, g3, g4, ncol=2)
pdp2<- gridExtra::grid.arrange(g5, g6, g7, ncol=3)

grid.arrange(pdp1, pdp2, ncol=1, heights = c(2, 1))


###SHAP interactions
xvars <- c("Age", "Sex", "Race", "Gender", 
           "Sexual orientation", "Education", 
           "Marital status")

#examining interactions
sv_dependence(shp.rf, v = xvars, color_var = "Age")
##nothing significant
sv_dependence(shp.rf, v = xvars, color_var = "Sex")
#nothing significant, except sex x gender
sv_dependence(shp.rf, v = xvars, color_var = "Race")
#nothing significant, but still can do race and gender, race and sexual orientation
sv_dependence(shp.rf, v = xvars, color_var = "Education")
#nothing significant
sv_dependence(shp.rf, v = xvars, color_var = "Gender")
##nothing significant, except sexual orientation x gender
sv_dependence(shp.rf, v = xvars, color_var = "Sexual orientation")
#visible effect between gender x sexual orientation
sv_dependence(shp.rf, v = xvars, color_var = "Marital status")
#nothing significant

###Interactions between race, sexual orientation, gender
g9<- sv_dependence2D(shp.rf, x = "Sexual orientation", y = c("Sexual orientation", "Race"), alpha = 0.5)
g9[[2]][["data"]][["Race"]]<- factor(g9[[2]][["data"]][["Race"]], 
                                    levels= c(0,1), 
                                    labels= c('Non-White', 'White'))
g9[[2]][["data"]][["Sexual orientation"]]<- factor(g9[[2]][["data"]][["Sexual orientation"]], 
                                                   levels= c(0,1,2), 
                                                   labels= c('Heterosexual', '2SLGBTQ+', 'Undisclosed'))


g10<- sv_dependence2D(shp.rf, x = "Gender", y = c("Gender", "Race"), alpha = 0.5)
g10[[2]][["data"]][["Gender"]]<- factor(g10[[2]][["data"]][["Gender"]], 
                                       levels= c(0,1,2), 
                                       labels= c('Cisgender', 'Others', 'Undisclosed'))
g10[[2]][["data"]][["Race"]]<- factor(g10[[2]][["data"]][["Race"]], 
                                     levels= c(0,1), 
                                     labels= c('Non-White', 'White'))

g11<- sv_dependence2D(shp.rf, x = "Sexual orientation", y = c("Sexual orientation", "Gender"), alpha = 0.5)
g11[[2]][["data"]][["Gender"]]<- factor(g11[[2]][["data"]][["Gender"]], 
                                        levels= c(0,1,2), 
                                        labels= c('Cisgender', 'Others', 'Undisclosed'))
g11[[2]][["data"]][["Sexual orientation"]]<- factor(g11[[2]][["data"]][["Sexual orientation"]], 
                                                   levels= c(0,1,2), 
                                                   labels= c('Heterosexual', '2SLGBTQ+', 'Undisclosed'))

gridExtra::grid.arrange(g9, g10, g11, ncol=1)


#SHAP force plot for single individual
sv_force(shp.rf, row_id = 100)
sv_waterfall(shp.rf, row_id = 100)

###SHAP analysis on XGBoost model
#One hot encoding
quit <- df[, "quit"]
encoded_vars <- df[, !names(df) %in% c("quit")]
dummy_model <- dummyVars(~ ., data = encoded_vars, fullRank = TRUE) 
df_onehot <- predict(dummy_model, newdata = encoded_vars)
df_onehot <- as.data.frame(df_onehot)
df_onehot <- cbind(df_onehot, quit)
str(df_onehot)
#n=6435, p=58

#data splitting
set.seed(123)
splitIndex.df_onehot <- createDataPartition(df_onehot$quit,p=0.8,
                                            list=FALSE,times=1)
train.df_onehot <- df_onehot[splitIndex.df_onehot,]
#n=5149, p=54
test.df_onehot <- df_onehot[-splitIndex.df_onehot,]
#n=1286, p=54

#Oversampling
df_overtrain <- ROSE(quit ~ ., data = train.df_onehot, seed = 123)$data
table(df_overtrain$quit)
#level 0 2590, level 1 2559

##preparing data
X_overtrain <- as.matrix(df_overtrain[, !names(df_overtrain) %in% c("quit")])
y_overtrain <- df_overtrain$quit
X_test <- as.matrix(test.df_onehot[, !names(test.df_onehot) %in% c("quit")])
y_test <- test.df_onehot$quit


##SHAP analysis
pred_fun <- function(model, newdata) {
  probs <- predict(model, newdata, type = "prob")
  if (is.null(probs) || any(is.na(probs))) {
    stop("Prediction function returned invalid probabilities")
  }
  return(probs[, 2])  # Return the probabilities for the '1' class
}
set.seed(123)
X <- X_test
bg_X <- X[sample(1:nrow(X), size = 100), ]
shap_values_xgb<- kernelshap(xgb_final, X = X, bg_X = bg_X, pred_fun = pred_fun)
print(shap_values_xgb)
shp.xgb <- shapviz(shap_values_xgb)
str(shp.xgb$X)

#renaming variables
rename_map <- c(
  age='Age',
  sex.1= 'Sex',
  race.1 = 'Race',
  Gender.1= 'Gender.1',
  Gender.2= 'Gender.2',
  sexorient.1= 'Sexual orientation.1',
  sexorient.2= 'Sexual orientation.2',
  edu.1= 'Education.1',
  edu.2= 'Education.2',
  married.1= 'Marital status',
  ecu8='Past month frequency of vaping',
  ecu12a.1='Puffs per vape.1',
  ecu12a.2='Puffs per vape.2',
  ecu16.1='Self-perceived addiction to vaping.1',
  ecu16.2='Self-perceived addiction to vaping.2',
  ecu16.3='Self-perceived addiction to vaping.3',
  ecu17.2='Intention to quit.2',
  ecu17.3='Intention to quit.3',
  ecu20a.1='Seeing others vaping',
  epp1.1='Device type.1',
  epp1.2='Device type.2',
  epp1.3='Device type.3',
  epp7.1='Flavor used.1',
  epp7.2='Flavor used.2',
  epp7.3='Flavor used.3',
  epp7.4='Flavor used.4',
  epp10.1='Nicotine strength.1',
  epp10.2='Nicotine strength.2',
  epp10.3='Nicotine strength.3',
  epp13='Past month expense of vaping',
  osu15.1='Past 30-day binge drinking',
  csmk.1='Past 30-day smoking',
  can.1='Past 30-day cannabis use',
  alc.1='Past 30-day alcohol drinking',
  otob.1='Past 30-day other tobacco products use',
  peer.1='Peer vaping.1',
  peer.2='Peer vaping.2',
  kab1nonecig.1='Harm perception of non-nicotine vaping.1',
  kab1nonecig.2='Harm perception of non-nicotine vaping.2',
  kab1necig.1='Harm perception of nicotine vaping.1',
  kab1necig.2='Harm perception of nicotine vaping.2',
  kab3.1='Believing EC is less harmful than cigarette.1',
  kab3.2='Believing EC is less harmful than cigarette.2',
  kab4.1= 'Believing vaping can help in quitting smoking.1',
  kab4.2= 'Believing vaping can help in quitting smoking.2',
  ghealth.1='Perceived general health',
  mhealth1.1='Perceived mental health',
  pstress.1='Perceived stress',
  psecdi.1="Dependence (PSECDI).1",
  psecdi.2="Dependence (PSECDI).2",
  psecdi.3="Dependence (PSECDI).3",
  eds='Dependence (EDS score)',
  ecu18.1='Past 3-months adverse effects',
  pro1.1='Exposure to e-cigarette marketing',
  depression.1='Depression', 
  resp.1='Past 4-months respiratory symptoms'
)

colnames(shp.xgb$X)[colnames(shp.xgb$X) %in% names(rename_map)] <- rename_map[colnames(shp.xgb$X) %in% names(rename_map)]
str(shp.xgb$X)
colnames(shp.xgb[["S"]])[colnames(shp.xgb[["S"]]) %in% names(rename_map)] <- rename_map[colnames(shp.xgb[["S"]]) %in% names(rename_map)]
colnames(shp.xgb[["S"]])

#Feature importance
shp.xgb.imp<- sv_importance(shp.xgb)
xgb.imp<- data.frame(shp.xgb.imp$data)
xgb.imp$value <- round(xgb.imp$value,3)

G1_xgb<- ggplot(data=xgb.imp,aes(x=reorder(feature,value),y=value))+
  geom_bar(fill= "navyblue", stat="identity")+
  geom_text(aes(label= value), y=0.005,color="white",size=3)+
  coord_flip()+
  labs(y="mean SHAP value",
       x= "")+
  theme(text = element_text(size= 12), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
print(G1_xgb)


###Sensitivity analysis
df_comp<- read.csv("complete_cases1.csv")
str(df_comp)
df_comp$X<- NULL
df_comp$ID<- NULL
df_comp[,c(2:7, 9:15, 17:30, 32:36)] <- lapply(df_comp[,c(2:7, 9:15, 17:30, 32:36)], as.factor)
df_comp[,c(1,8,16,31)] <- lapply(df_comp[,c(1,8,16,31)], function(x) c(scale(x)))
str(df_comp)
#N=5288, p=36

#running RF model on df_comp 
set.seed(123)
splitIndex.df_comp <- createDataPartition(df_comp$quit,p=0.8,
                                     list=FALSE,times=1)
train.df_comp <- df_comp[splitIndex.df_comp,]
#n=4231, p=36
test.df_comp <- df_comp[-splitIndex.df_comp,]
#n=1057, p=36

table(train.df_comp$quit)
table(test.df_comp$quit)
#data imbalance, using oversampling
set.seed(123)
overTrain.df_comp <- ROSE(quit ~ ., data = train.df_comp, seed = 123)$data
table(overTrain.df_comp$quit)

## random foresting
tunegrid <- expand.grid(.mtry = (1:15)) 

trControl <- trainControl(method = "cv", number = 10, search = "grid")

set.seed(123)
rf_comp <- train(quit ~ ., data = overTrain.df_comp, method = "rf", metric = "Accuracy",
               trControl = trControl, tuneGrid = tunegrid, ntree = 500)
print(rf_comp)
#best mtry 4, optimal accuracy 0.91
##using it for final model
set.seed(123)
rf_comp <- train(quit ~ ., data = overTrain.df_comp, method = "rf", metric = "Accuracy",
            trControl = trControl, tuneGrid = rf_comp$bestTune, ntree= 500)
print(rf_comp)
#Accuracy on training set 0.91

## get prediction
prediction_rf_comp <- predict(rf_comp, test.df_comp)
tb_rf_comp <- table(prediction_rf_comp, test.df_comp$quit)
confusionMatrix(tb_rf_comp)
#accuracy 0.77 (95% CI 0.74, 0.80)
#sensitivity 0.83, specificity 0.45

## need pred prob
pred_rf_comp <- predict(rf_comp, newdata = test.df_comp, type = "prob")
pred.class_rf_comp <- prediction(pred_rf_comp[, 2], test.df_comp$quit)
perf_comp_rf_comp <- performance(pred.class_rf_comp,measure = "tpr",x.measure= "fpr")
AUC_rf_comp <- performance(pred.class_rf_comp, 'auc')@y.values
print(AUC_rf_comp)
#AUC 0.730


###SHAP analysis
pred_fun <- function(model, newdata) {
  probs <- predict(model, newdata, type = "prob")
  if (is.null(probs) || any(is.na(probs))) {
    stop("Prediction function returned invalid probabilities")
  }
  return(probs[, "1"])  # Return the probabilities for the '1' class
}
set.seed(123)
X <- test.df_comp[, -which(names(test.df_comp) == "quit")] 
bg_X <- X[sample(1:nrow(X), size = 100), ]
shap_values_comp<- kernelshap(rf_comp, X = X, bg_X = bg_X, pred_fun = pred_fun)
print(shap_values_comp)
shp.rf_comp <- shapviz(shap_values_comp)

#renaming features
rename_map <- c(
  age='Age',
  sex= 'Sex',
  race= 'Race',
  Gender= 'Gender',
  sexorient= 'Sexual orientation',
  edu= 'Education',
  married= 'Marital status',
  ecu8='Past month frequency of vaping',
  ecu12a='Puffs per vape',
  ecu16='Self-perceived addiction to vaping',
  ecu17='Intention to quit',
  ecu20a='Seeing others vaping',
  epp1='Device type',
  epp7='Flavor used',
  epp10='Nicotine strength',
  epp13='Past month expense of vaping',
  osu15='Past 30-day binge drinking',
  csmk='Past 30-day smoking',
  can='Past 30-day cannabis use',
  alc='Past 30-day alcohol drinking',
  otob='Past 30-day other tobacco products use',
  peer='Peer vaping',
  kab1nonecig='Harm perception of non-nicotine vaping',
  kab1necig='Harm perception of nicotine vaping',
  kab3='Believing EC is less harmful than cigarette',
  kab4= 'Believing vaping can help in quitting smoking',
  ghealth='Perceived general health',
  mhealth1='Perceived mental health',
  pstress='Perceived stress',
  psecdi="Dependence (PSECDI)",
  eds='Dependence (EDS score)',
  ecu18='Past 3-months adverse effects',
  pro1='Exposure to e-cigarette marketing',
  depression='Depression', 
  resp='Past 4-months respiratory symptoms'
)

colnames(shp.rf_comp$X)[colnames(shp.rf_comp$X) %in% names(rename_map)] <- rename_map[colnames(shp.rf_comp$X) %in% names(rename_map)]
str(shp.rf_comp$X)
colnames(shp.rf_comp[["S"]])[colnames(shp.rf_comp[["S"]]) %in% names(rename_map)] <- rename_map[colnames(shp.rf_comp[["S"]]) %in% names(rename_map)]
colnames(shp.rf_comp[["S"]])


#Feature importance
shp.rf.imp_comp<- sv_importance(shp.rf_comp)
rf.imp_comp<- data.frame(shp.rf.imp_comp$data)
rf.imp_comp$value <- round(rf.imp_comp$value,3)

G2<- ggplot(data=rf.imp_comp ,aes(x=reorder(feature,value),y=value))+
  geom_bar(fill= "navyblue", stat="identity")+
  geom_text(aes(label= value), y=0.005,color="white",size=3)+
  coord_flip()+
  labs(y="mean SHAP value",
       x= "")+
  theme(text = element_text(size= 12), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
print(G2)

###RF model on modified data
df_mod<- read.csv("modified_data1.csv")
str(df_mod)
df_mod$X<- NULL
df_mod$ID<- NULL
df_mod[,c(2:7, 9:15, 17:30, 32:36)] <- lapply(df_mod[,c(2:7, 9:15, 17:30, 32:36)], as.factor)
str(df_mod)
#N=6038, p=36

#imputation of missing variables
sapply(df_mod, function(x) sum(is.na(x)))

vis_miss(df_mod,sort_miss=TRUE)
#2.2% missing data

init <- mice(df_mod, maxit=0)
meth <- init$method
predM <- init$predictorMatrix

# exclude outcome as predictor
predM[,c("quit")] <- 0

# methods for different variables
pmm <- c("ecu8", 'epp13')
polr <- c('ecu17')
logreg<- c('sex', 'race', 'ecu18', 
           'ecu20a', 'csmk', 'can', 'alc', 'otob',
           'osu15', 'mhealth1', 'pstress', 'ghealth', 'resp')

meth[pmm] <- "pmm"
meth[polr] <- "polr"
meth[logreg] <- "logreg"

set.seed(123)
imputed_mod <- mice(df_mod, method=meth, predictorMatrix=predM)
summary(imputed_mod)
df_mod <- complete(imputed_mod, 1)

vis_miss(df_mod,sort_miss=TRUE)
#all imputed

#scaling numeric variables
df_mod[,c(1,8,16,31)] <- lapply(df_mod[,c(1,8,16,31)], function(x) c(scale(x)))
str(df_mod)
#N=6038, p=36

#running RF model on df_mod 
set.seed(123)
splitIndex.df_mod <- createDataPartition(df_mod$quit,p=0.8,
                                         list=FALSE,times=1)
train.df_mod <- df_mod[splitIndex.df_mod,]
#n=4831, p=36
test.df_mod <- df_mod[-splitIndex.df_mod,]
#n=1207, p=36

table(train.df_mod$quit)
table(test.df_mod$quit)
#data imbalance, using oversampling
set.seed(123)
overTrain.df_mod <- ROSE(quit ~ ., data = train.df_mod, seed = 123)$data
table(overTrain.df_mod$quit)

## random foresting
tunegrid <- expand.grid(.mtry = (1:15))

trControl <- trainControl(method = "cv", number = 10, search = "grid")

set.seed(123)
rf_mod <- train(quit ~ ., data = overTrain.df_mod, method = "rf", metric = "Accuracy",
                trControl = trControl, tuneGrid = tunegrid, ntree = 500)
print(rf_mod)
#best mtry 4, optimal accuracy 0.90
##using it for final model
set.seed(123)
rf_mod <- train(quit ~ ., data = overTrain.df_mod, method = "rf", metric = "Accuracy",
                trControl = trControl, tuneGrid = rf_mod$bestTune, ntree= 500)
print(rf_mod)
#Accuracy on training set 0.91

## get prediction
prediction_rf_mod <- predict(rf_mod, test.df_mod)
tb_rf_mod <- table(prediction_rf_mod, test.df_mod$quit)
confusionMatrix(tb_rf_mod)
#accuracy 0.81 (95% CI 0.78, 0.83)
#sensitivity 0.87, specificity 0.39

## need pred prob
pred_rf_mod <- predict(rf_mod, newdata = test.df_mod, type = "prob")
pred.class_rf_mod <- prediction(pred_rf_mod[, 2], test.df_mod$quit)
perf_mod_rf_mod <- performance(pred.class_rf_mod,measure = "tpr",x.measure= "fpr")
AUC_rf_mod <- performance(pred.class_rf_mod, 'auc')@y.values
print(AUC_rf_mod)
#AUC 0.723


###SHAP analysis
pred_fun <- function(model, newdata) {
  probs <- predict(model, newdata, type = "prob")
  if (is.null(probs) || any(is.na(probs))) {
    stop("Prediction function returned invalid probabilities")
  }
  return(probs[, "1"])  # Return the probabilities for the '1' class
}
set.seed(123)
X <- test.df_mod[, -which(names(test.df_mod) == "quit")] 
bg_X <- X[sample(1:nrow(X), size = 100), ]
shap_values_mod<- kernelshap(rf_mod, X = X, bg_X = bg_X, pred_fun = pred_fun)
print(shap_values_mod)
shp.rf_mod <- shapviz(shap_values_mod)

#renaming features
rename_map <- c(
  age='Age',
  sex= 'Sex',
  race= 'Race',
  Gender= 'Gender',
  sexorient= 'Sexual orientation',
  edu= 'Education',
  married= 'Marital status',
  ecu8='Past month frequency of vaping',
  ecu12a='Puffs per vape',
  ecu16='Self-perceived addiction to vaping',
  ecu17='Intention to quit',
  ecu20a='Seeing others vaping',
  epp1='Device type',
  epp7='Flavor used',
  epp10='Nicotine strength',
  epp13='Past month expense of vaping',
  osu15='Past 30-day binge drinking',
  csmk='Past 30-day smoking',
  can='Past 30-day cannabis use',
  alc='Past 30-day alcohol drinking',
  otob='Past 30-day other tobacco products use',
  peer='Peer vaping',
  kab1nonecig='Harm perception of non-nicotine vaping',
  kab1necig='Harm perception of nicotine vaping',
  kab3='Believing EC is less harmful than cigarette',
  kab4= 'Believing vaping can help in quitting smoking',
  ghealth='Perceived general health',
  mhealth1='Perceived mental health',
  pstress='Perceived stress',
  psecdi="Dependence (PSECDI)",
  eds='Dependence (EDS score)',
  ecu18='Past 3-months adverse effects',
  pro1='Exposure to e-cigarette marketing',
  depression='Depression', 
  resp='Past 4-months respiratory symptoms'
)

colnames(shp.rf_mod$X)[colnames(shp.rf_mod$X) %in% names(rename_map)] <- rename_map[colnames(shp.rf_mod$X) %in% names(rename_map)]
str(shp.rf_mod$X)
colnames(shp.rf_mod[["S"]])[colnames(shp.rf_mod[["S"]]) %in% names(rename_map)] <- rename_map[colnames(shp.rf_mod[["S"]]) %in% names(rename_map)]
colnames(shp.rf_mod[["S"]])

##Feature importance
shp.rf.imp_mod<- sv_importance(shp.rf_mod)
rf.imp_mod<- data.frame(shp.rf.imp_mod$data)
rf.imp_mod$value <- round(rf.imp_mod$value,3)

G3<- ggplot(data=rf.imp_mod,aes(x=reorder(feature,value),y=value))+
  geom_bar(fill= "navyblue", stat="identity")+
  geom_text(aes(label= value), y=0.005,color="white",size=3)+
  coord_flip()+
  labs(y="mean SHAP value",
       x= "")+
  theme(text = element_text(size= 12), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
print(G3)


##Comparing predictors
##Combining plots
G1_with_title <- arrangeGrob(G1, top = textGrob("RF model", gp = gpar(fontsize = 12, font = 2)))
G1_xgb_with_title <- arrangeGrob(G1_xgb, top = textGrob("XGBoost model", gp = gpar(fontsize = 12, font = 2)))
G2_with_title <- arrangeGrob(G2, top = textGrob("Completed cases (RF model)", gp = gpar(fontsize = 12, font = 2)))
G3_with_title <- arrangeGrob(G3, top = textGrob("Limited dataset (RF model)", gp = gpar(fontsize = 12, font = 2)))

grid.arrange(G1_with_title, G1_xgb_with_title, G2_with_title, G3_with_title, ncol = 2)
