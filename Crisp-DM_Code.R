setwd("D:/")
# Collect data
library(readr)
df<-read_csv(url("https://raw.githubusercontent.com/IBM/telco-customer-churn-on-icp4d/master/data/Telco-Customer-Churn.csv"))
# Save to PC
write.csv(df,"D:/IBM_Churn_Data.csv",
          row.names=FALSE)
rm(df)
# Load data from PC
df<-read.csv("IBM_Churn_Data.csv")
head(df)
str(df)
summary(df)

'''
DATA UNDERSTANDING

# Demographic info about customers - gender, age range, and if they have partners and dependents
gender = Male or Female
SeniorCitizen = Senior or not
Partner = Partner or not
Dependents = Dependents or not

# Services that each customer has signed up for - phone, multiple lines, internet, online 
security, online backup, device protection, tech support, and streaming TV and movies
PhoneService = Phone service or not
MultipleLines = Multiple lines: Yes, No, No phone service
InternetService = Internet service provided: DSL,Fiber or No.
OnlineSecurity = Internet security: Yes, No, No internet.
OnlineBackup = Online backup: Yes, No, No internet service
DeviceProtection = Device protection: Yes, No, No internet service
TechSupport = Tech support: Yes, No, No internet service
StreamingTV = TV streaming: Yes, No, No internet service
StreamingMovies = Movies streaming: Yes, No, No internet service

# Customer account information - how long theyve been a customer, contract, payment method, 
paperless billing, monthly charges, and total charges
Tenure = Number of months customer has stayed with compony
Contract = Contract term: Month-to-month, One year, Two year
PaperlessBilling = Paperless billing or not: Yes, No
PaymentMethod = Payment method: Electronic check, Mailed check, Bank transfer (automatic), 
Credit card (automatic)
MonthlyCharges = The amount charged to the customer monthly
TotalCharges = The total amount charged to the customer

Churn = Whether the customer left in the last month: Yes, No
'''

# Construct a data quality report per feature
library(dataMaid)
makeDataReport(df,
               smartNum=FALSE,
               replace=TRUE,
               reportTitle="Raw_Data_Quality_Report",
               file="Raw_Data_Report")
# Test for outliers on continous variables
identifyOutliers(df$tenure)
identifyOutliers(df$MonthlyCharges)
identifyOutliers(df$TotalCharges)

'''
Whats wrong with the data as seen from Raw_Data_Quality_Report?
1) Inconsistant variable classes.
2) Numeric and character booleans.
3) Customer ID class with high cardinality
4) Missing values in TotalCharges.
5) tenure skew right.
6) TotalCharges skew right.
7) Spaces between categorical data values.
8) Redundant categories within features associated with other features. 
E.G. MultipleLines has "No phone service" class, and there is a boolean feature called "PhoneService"
9) Imbalanced target class.
'''

# Remove customerID
df$customerID=NULL
# Remove any spaces from categorical data.
library(tidyverse)
cat_df<-df %>% 
  dplyr::select(!(c("SeniorCitizen","tenure","MonthlyCharges","TotalCharges")))
cat_df<-as.data.frame(apply(cat_df,2,function(x)gsub('\\s+','_',x)))
con_df<-df %>% 
  dplyr::select(c("SeniorCitizen","tenure","MonthlyCharges","TotalCharges"))
df<-as.data.frame(cbind(con_df,cat_df))
rm(cat_df,con_df)
# Change NAs to 0. Original NA values for TotalCharges had tenure of 0, 
# meaning that these where new clients which just stated and had not yet made a payment
df[is.na(df)]<-0
any(is.na(df))

# Relationship between continuous features 
con_df<-df %>% dplyr::select(c("tenure","MonthlyCharges","TotalCharges"))
# Test for correlations
library(PerformanceAnalytics)
chart.Correlation(as.matrix(con_df),TRUE) #6.5x6.5
# Remove TotalCharges, as it has high cardinality and highly correlated with tenure
con_df$TotalCharges=NULL
chart.Correlation(as.matrix(con_df),TRUE) #6.5x6.5
rm(con_df)
df$TotalCharges=NULL

# Relationship between categorical features
# https://cran.r-project.org/web/packages/GoodmanKruskal/vignettes/GoodmanKruskal.html
cat_df<-df %>% dplyr::select(!(c("tenure","MonthlyCharges")))
cat_df$Churn=NULL
# Strength of associations
library(GoodmanKruskal)
cat_Cor<-GKtauDataframe(cat_df)
plot(GKtauDataframe(cat_df)) #14x11
# Above plot looks bad, save as table as well (read from row to col)
cat_Cor<-as.numeric(cat_Cor)
cat_Cor<-as.data.frame(matrix(cat_Cor,(length(colnames(cat_df)))))
rownames(cat_Cor)<-colnames(cat_df)
colnames(cat_Cor)<-colnames(cat_df)
# Exporting results .csv
write.csv(cat_Cor,"D:/Categorical_Var_Associations_Before.csv",
          row.names=TRUE)
rm(cat_Cor,cat_df)
# Multiple lines has a strong forward selection with Phone service
df_PhoneService_No<-subset(df,PhoneService=="No")
df_PhoneService_Yes<-subset(df,PhoneService=="Yes")
par(mfrow=c(1,3))
plot(df$MultipleLines,main="MultipleLines class frequencies",xlab="MultipleLines classes",ylab="Frequency")
plot(df_PhoneService_No$MultipleLines,main="Phone service = No",xlab="MultipleLines classes",ylab="Frequency")
plot(df_PhoneService_Yes$MultipleLines,main="Phone service = Yes",xlab="MultipleLines classes",ylab="Frequency")
# Mosiac plot
plot(df$PhoneService,df$MultipleLines,ylab="Multiple lines",xlab="Phone service",
     main="Relationship between multiple lines and phone service")
rm(df_PhoneService_No,df_PhoneService_Yes)

# Relationship between continuous and categorical features
par(mfrow=c(4,4),mai=c(0.7,0.7,0.3,0.3))
boxplot(tenure~SeniorCitizen,df)
boxplot(tenure~gender,df)
boxplot(tenure~Partner,df)
boxplot(tenure~Dependents,df)
boxplot(tenure~PhoneService,df)
boxplot(tenure~PaperlessBilling,df)
boxplot(tenure~MultipleLines,df)
boxplot(tenure~InternetService,df)
boxplot(tenure~OnlineSecurity,df)
boxplot(tenure~OnlineBackup,df)
boxplot(tenure~DeviceProtection,df)
boxplot(tenure~TechSupport,df)
boxplot(tenure~StreamingTV,df)
boxplot(tenure~StreamingMovies,df)
boxplot(tenure~Contract,df)
boxplot(tenure~PaymentMethod,df)
# SeniorCitizen,gender,PhoneService and PaperlessBilling are closely related to tenure
# There is no relationship and do not add any descriptive power to the model and thus tenure
# should be removed.
par(mfrow=c(4,4),mai=c(0.7,0.7,0.3,0.3))
boxplot(MonthlyCharges~SeniorCitizen,df)
boxplot(MonthlyCharges~gender,df)
boxplot(MonthlyCharges~Partner,df)
boxplot(MonthlyCharges~Dependents,df)
boxplot(MonthlyCharges~PhoneService,df)
boxplot(MonthlyCharges~PaperlessBilling,df)
boxplot(MonthlyCharges~MultipleLines,df)
boxplot(MonthlyCharges~InternetService,df)
boxplot(MonthlyCharges~OnlineSecurity,df)
boxplot(MonthlyCharges~OnlineBackup,df)
boxplot(MonthlyCharges~DeviceProtection,df)
boxplot(MonthlyCharges~TechSupport,df)
boxplot(MonthlyCharges~StreamingTV,df)
boxplot(MonthlyCharges~StreamingMovies,df)
boxplot(MonthlyCharges~Contract,df)
boxplot(MonthlyCharges~PaymentMethod,df)
# gender is closely related to tenure.
df$tenure=NULL
df$MonthlyCharges=NULL

# Feature engineering
# Change 2 level factors to numeric binary features
df<-df %>% 
  mutate(gender=ifelse(gender=="Female",1,0)) %>%
  rename(Female=gender) %>% 
  mutate(Partner=ifelse(Partner=="Yes",1,0)) %>% 
  mutate(Dependents=ifelse(Dependents=="Yes",1,0)) %>% 
  mutate(PhoneService=ifelse(PhoneService=="Yes",1,0)) %>% 
  mutate(PaperlessBilling=ifelse(PaperlessBilling=="Yes",1,0))
# Perform feature engineering to remove redundancy
df<-df %>% 
  mutate(MultipleLines=ifelse(MultipleLines=="Yes",1,0)) %>% 
  mutate(OnlineSecurity=ifelse(OnlineSecurity=="Yes",1,0)) %>% 
  mutate(OnlineBackup=ifelse(OnlineBackup=="Yes",1,0)) %>% 
  mutate(DeviceProtection=ifelse(DeviceProtection=="Yes",1,0)) %>% 
  mutate(TechSupport=ifelse(TechSupport=="Yes",1,0)) %>% 
  mutate(StreamingTV=ifelse(StreamingTV=="Yes",1,0)) %>% 
  mutate(StreamingMovies=ifelse(StreamingMovies=="Yes",1,0))
# One-Hot encode factors with more than 2 classes
library(caret)
dumy<-data.frame(predict(dummyVars("~InternetService+Contract+PaymentMethod",data=df),newdata=df))
dumy<-dumy %>%
  rename(DSL=InternetService.DSL) %>% 
  rename(Fiber=InternetService.Fiber_optic) %>% 
  rename(No_Internet_Service=InternetService.No) %>% 
  rename(Contract_Montly=Contract.Month.to.month) %>% 
  rename(Contract_One_Year=Contract.One_year) %>% 
  rename(Contract_Two_Year=Contract.Two_year) %>%
  rename(EFT=PaymentMethod.Bank_transfer_.automatic.) %>%
  rename(Credit_Card=PaymentMethod.Credit_card_.automatic.) %>%
  rename(Electronic_Check=PaymentMethod.Electronic_check) %>%
  rename(Mailed_Check=PaymentMethod.Mailed_check)
df$InternetService=NULL
df$Contract=NULL
df$PaymentMethod=NULL
df<-as.data.frame(cbind(dumy,df))
rm(dumy)

# Correlation between all variables
library(ggcorrplot)
df_cor<-df[,1:23]
ggcorrplot(cor(df_cor, method="pearson"),type="lower",lab=TRUE,lab_size=2,tl.cex= 10)#8x8
rm(df_cor)

# Construct a data quality report for processed features
library(dataMaid)
makeDataReport(df,
               smartNum=FALSE,
               replace=TRUE,
               reportTitle="Clean_Data_Quality_Report",
               file="Clean_Data_Report")

# Save cleaned data
write.csv(df,"D:/Cleaned_Data.csv",
          row.names = FALSE)
rm(df)


# Modeling
# https://towardsdatascience.com/ensemble-methods-bagging-boosting-and-stacking-c9214a10a205
# https://topepo.github.io/caret/train-models-by-tag.html
library(tidyverse)
library(caret)
library(parallel)
library(doParallel)
library(DMwR)
# Load cleaned data
df<-read.csv("Cleaned_Data.csv")
# Look at class ballence
plot(df$Churn,main="Churn class frequencies",xlab="Churn classes",ylab="Frequency") #5x4.5
# Splitting data using stratified random sampling
set.seed(321)
df_Split<-createDataPartition(df$Churn,p=0.7,list=FALSE)
df_train<-df[df_Split,]
df_test<-df[-df_Split,]
rm(df_Split,df)
# Class ballencing
set.seed(9560)
down_train<-downSample(x=df_train[,-ncol(df_train)],y=df_train$Churn)
down_train<-down_train %>% rename(Churn=Class)
smote_train<-SMOTE(Churn~.,data=df_train)   
rm(df_train)

# Perform feature selection
library(car)
library(MASS)
model_1<-glm(Churn~.,data=down_train,family=binomial(link="logit"))
model_2<-stepAIC(model_1,direction="both")
# Test multicollinearity
(vif<-as.data.frame(vif(model_2)))
# All features have VIF scores of under 5, with highest score being 3.68.
rm(model_1,model_2,vif)

# DOWN Generalized Linear Model 0 parameters
system.time(model_glm_down<-caret::train(Churn~DSL+Fiber+Contract_Montly+Contract_One_Year+EFT+Credit_Card+SeniorCitizen+Dependents+
                                           OnlineSecurity+OnlineBackup+DeviceProtection+TechSupport+StreamingMovies+PaperlessBilling,
                                         method="glm",
                                         data=na.omit(down_train),
                                         tuneLength=10,
                                         metric="Kappa",
                                         trControl= trainControl(summaryFunction=multiClassSummary)))
#Down Time. user:1.48 system:0.00 elapsed:1.48
# Saving model Generalized Linear Model
saveRDS(model_glm_down, file="D:/Models/model_glm_down.rds")
# Reading model Generalized Linear Model
model_glm_down<-readRDS("D:/Models/model_glm_down.rds")
# Confusion matrix Generalized Linear Model
predict_glm_down<-predict(model_glm_down, na.omit(df_test))
glm_down_con_mat<-confusionMatrix(data=predict_glm_down, na.omit(df_test$Churn),mode="prec_recall")
# Variable importance Generalized Linear Model
plot(varImp(model_glm_down),main="glm variable importance down") #5x5
glm_down_Evaluation<-as.data.frame(cbind("Model_Type"=model_glm_down$method,
                                         "Accuracy"=glm_down_con_mat[["overall"]][["Accuracy"]],
                                         "CI_5%"=glm_down_con_mat[["overall"]][["AccuracyLower"]],
                                         "CI_95%"=glm_down_con_mat[["overall"]][["AccuracyUpper"]],
                                         "No_Information_Rate"=glm_down_con_mat[["overall"]][["AccuracyNull"]],
                                         "Kappa"=glm_down_con_mat[["overall"]][["Kappa"]],
                                         "Precision"=glm_down_con_mat[["byClass"]][["Precision"]],
                                         "Recall"=glm_down_con_mat[["byClass"]][["Recall"]],
                                         "F1"=glm_down_con_mat[["byClass"]][["F1"]],
                                         "Prevalence"=glm_down_con_mat[["byClass"]][["Prevalence"]],
                                         "Detection_Rate"=glm_down_con_mat[["byClass"]][["Detection Rate"]],
                                         "Detection_Prevalence"=glm_down_con_mat[["byClass"]][["Detection Prevalence"]],
                                         "Balanced_Accuracy"=glm_down_con_mat[["byClass"]][["Balanced Accuracy"]],
                                         "Time"="0 min 1 sec"))
rm(predict_glm_down,model_glm_down,glm_down_con_mat)

# SMOTE Generalized Linear Model 0 parameters
system.time(model_glm_smote<-caret::train(Churn~DSL+Fiber+Contract_Montly+Contract_One_Year+EFT+Credit_Card+SeniorCitizen+Dependents+
                                            OnlineSecurity+OnlineBackup+DeviceProtection+TechSupport+StreamingMovies+PaperlessBilling,
                                          method="glm",
                                          data=na.omit(smote_train),
                                          tuneLength=10,
                                          metric="Kappa",
                                          trControl= trainControl(summaryFunction=multiClassSummary)))
#Smote Time. user:2.89 system:0.00 elapsed:2.89
# Saving model Generalized Linear Model
saveRDS(model_glm_smote, file="D:/Models/model_glm_smote.rds")
# Reading model Generalized Linear Model
model_glm_smote<-readRDS("D:/Models/model_glm_smote.rds")
# Confusion matrix Generalized Linear Model
predict_glm_smote<-predict(model_glm_smote, na.omit(df_test))
glm_smote_con_mat<-confusionMatrix(data=predict_glm_smote, na.omit(df_test$Churn),mode="prec_recall")
# Variable importance Generalized Linear Model
plot(varImp(model_glm_smote),main="glm variable importance smote") #5x5
glm_smote_Evaluation<-as.data.frame(cbind("Model_Type"=model_glm_smote$method,
                                          "Accuracy"=glm_smote_con_mat[["overall"]][["Accuracy"]],
                                          "CI_5%"=glm_smote_con_mat[["overall"]][["AccuracyLower"]],
                                          "CI_95%"=glm_smote_con_mat[["overall"]][["AccuracyUpper"]],
                                          "No_Information_Rate"=glm_smote_con_mat[["overall"]][["AccuracyNull"]],
                                          "Kappa"=glm_smote_con_mat[["overall"]][["Kappa"]],
                                          "Precision"=glm_smote_con_mat[["byClass"]][["Precision"]],
                                          "Recall"=glm_smote_con_mat[["byClass"]][["Recall"]],
                                          "F1"=glm_smote_con_mat[["byClass"]][["F1"]],
                                          "Prevalence"=glm_smote_con_mat[["byClass"]][["Prevalence"]],
                                          "Detection_Rate"=glm_smote_con_mat[["byClass"]][["Detection Rate"]],
                                          "Detection_Prevalence"=glm_smote_con_mat[["byClass"]][["Detection Prevalence"]],
                                          "Balanced_Accuracy"=glm_smote_con_mat[["byClass"]][["Balanced Accuracy"]],
                                          "Time"="0 min 3 sec"))
rm(predict_glm_smote,model_glm_smote,glm_smote_con_mat)

# down Support Vector Machines with Linear Kernel 1 parameter
set.seed(321)
cl <- makeCluster(3/4 * detectCores())
registerDoParallel(cl)
system.time(model_svmLinear_down<-caret::train(Churn~DSL+Fiber+Contract_Montly+Contract_One_Year+EFT+Credit_Card+SeniorCitizen+Dependents+
                                                 OnlineSecurity+OnlineBackup+DeviceProtection+TechSupport+StreamingMovies+PaperlessBilling, 
                                               method="svmLinear",
                                               data=na.omit(down_train),
                                               allowParallel=TRUE,
                                               tuneLength=10,
                                               metric="Kappa",
                                               trControl= trainControl(summaryFunction=multiClassSummary)))
stopCluster(cl); remove(cl)
registerDoSEQ()
# down Time. user:1.22 system:0.04 elapsed:8.06
# Saving model Support Vector Machines with Linear Kernel
saveRDS(model_svmLinear_down, file="D:/Models/model_svmLinear_down.rds")
# Reading model Support Vector Machines with Linear Kernel
model_svmLinear_down<-readRDS("D:/Models/model_svmLinear_down.rds")
# Confusion matrix Support Vector Machines with Linear Kernel
predict_svmLinear_down<-predict(model_svmLinear_down, na.omit(df_test))
svmLinear_down_con_mat<-confusionMatrix(data=predict_svmLinear_down, na.omit(df_test$Churn),mode="prec_recall")
# Variable importance Support Vector Machines with Linear Kernel
plot(varImp(model_svmLinear_down),main="svm variable importance down") #5x5
svm_down_Evaluation<-as.data.frame(cbind("Model_Type"=model_svmLinear_down$method,
                                         "Accuracy"=svmLinear_down_con_mat[["overall"]][["Accuracy"]],
                                         "CI_5%"=svmLinear_down_con_mat[["overall"]][["AccuracyLower"]],
                                         "CI_95%"=svmLinear_down_con_mat[["overall"]][["AccuracyUpper"]],
                                         "No_Information_Rate"=svmLinear_down_con_mat[["overall"]][["AccuracyNull"]],
                                         "Kappa"=svmLinear_down_con_mat[["overall"]][["Kappa"]],
                                         "Precision"=svmLinear_down_con_mat[["byClass"]][["Precision"]],
                                         "Recall"=svmLinear_down_con_mat[["byClass"]][["Recall"]],
                                         "F1"=svmLinear_down_con_mat[["byClass"]][["F1"]],
                                         "Prevalence"=svmLinear_down_con_mat[["byClass"]][["Prevalence"]],
                                         "Detection_Rate"=svmLinear_down_con_mat[["byClass"]][["Detection Rate"]],
                                         "Detection_Prevalence"=svmLinear_down_con_mat[["byClass"]][["Detection Prevalence"]],
                                         "Balanced_Accuracy"=svmLinear_down_con_mat[["byClass"]][["Balanced Accuracy"]],
                                         "Time"="0 min 8 sec"))
rm(predict_svmLinear_down,model_svmLinear_down,svmLinear_down_con_mat)

# smote Support Vector Machines with Linear Kernel 1 parameter
set.seed(321)
cl <- makeCluster(3/4 * detectCores())
registerDoParallel(cl)
system.time(model_svmLinear_smote<-caret::train(Churn~DSL+Fiber+Contract_Montly+Contract_One_Year+EFT+Credit_Card+SeniorCitizen+Dependents+
                                                  OnlineSecurity+OnlineBackup+DeviceProtection+TechSupport+StreamingMovies+PaperlessBilling, 
                                                method="svmLinear",
                                                data=na.omit(smote_train),
                                                allowParallel=TRUE,
                                                tuneLength=10,
                                                metric="Kappa",
                                                trControl= trainControl(summaryFunction=multiClassSummary)))
stopCluster(cl); remove(cl)
registerDoSEQ()
# smote Time. user:4.78 system:0.16 elapsed:40.21
# Saving model Support Vector Machines with Linear Kernel
saveRDS(model_svmLinear_smote, file="D:/Models/model_svmLinear_smote.rds")
# Reading model Support Vector Machines with Linear Kernel
model_svmLinear_smote<-readRDS("D:/Models/model_svmLinear_smote.rds")
# Confusion matrix Support Vector Machines with Linear Kernel
predict_svmLinear_smote<-predict(model_svmLinear_smote, na.omit(df_test))
svmLinear_smote_con_mat<-confusionMatrix(data=predict_svmLinear_smote, na.omit(df_test$Churn),mode="prec_recall")
# Variable importance Support Vector Machines with Linear Kernel
plot(varImp(model_svmLinear_smote),main="svm variable importance smote") #5x5
svm_smote_Evaluation<-as.data.frame(cbind("Model_Type"=model_svmLinear_smote$method,
                                          "Accuracy"=svmLinear_smote_con_mat[["overall"]][["Accuracy"]],
                                          "CI_5%"=svmLinear_smote_con_mat[["overall"]][["AccuracyLower"]],
                                          "CI_95%"=svmLinear_smote_con_mat[["overall"]][["AccuracyUpper"]],
                                          "No_Information_Rate"=svmLinear_smote_con_mat[["overall"]][["AccuracyNull"]],
                                          "Kappa"=svmLinear_smote_con_mat[["overall"]][["Kappa"]],
                                          "Precision"=svmLinear_smote_con_mat[["byClass"]][["Precision"]],
                                          "Recall"=svmLinear_smote_con_mat[["byClass"]][["Recall"]],
                                          "F1"=svmLinear_smote_con_mat[["byClass"]][["F1"]],
                                          "Prevalence"=svmLinear_smote_con_mat[["byClass"]][["Prevalence"]],
                                          "Detection_Rate"=svmLinear_smote_con_mat[["byClass"]][["Detection Rate"]],
                                          "Detection_Prevalence"=svmLinear_smote_con_mat[["byClass"]][["Detection Prevalence"]],
                                          "Balanced_Accuracy"=svmLinear_smote_con_mat[["byClass"]][["Balanced Accuracy"]],
                                          "Time"="0 min 40 sec"))
rm(predict_svmLinear_smote,model_svmLinear_smote,svmLinear_smote_con_mat)

# down Decision Tree Classifier (CART) rpart 1 parameter
system.time(model_rpart_down<-caret::train(Churn~DSL+Fiber+Contract_Montly+Contract_One_Year+EFT+Credit_Card+SeniorCitizen+Dependents+
                                             OnlineSecurity+OnlineBackup+DeviceProtection+TechSupport+StreamingMovies+PaperlessBilling, 
                                           method="rpart",
                                           data=na.omit(down_train),
                                           tuneLength=10,
                                           metric="Kappa",
                                           trControl= trainControl(summaryFunction=multiClassSummary)))
# down Time. user:2.94 system:0.00 elapsed:2.95
# Saving model Decision Tree Classifier (CART)
saveRDS(model_rpart_down, file="D:/Models/model_rpart_down.rds")
# Reading model Decision Tree Classifier (CART)
model_rpart_down<-readRDS("D:/Models/model_rpart_down.rds")
# Confusion matrix Decision Tree Classifier (CART)
predict_rpart_down<-predict(model_rpart_down, na.omit(df_test))
rpart_down_con_mat<-confusionMatrix(data=predict_rpart_down, na.omit(df_test$Churn),mode="prec_recall")
# Variable importance Decision Tree Classifier (CART)
plot(varImp(model_rpart_down),main="rpart variable importance down") #5x5
rpart_down_Evaluation<-as.data.frame(cbind("Model_Type"=model_rpart_down$method,
                                           "Accuracy"=rpart_down_con_mat[["overall"]][["Accuracy"]],
                                           "CI_5%"=rpart_down_con_mat[["overall"]][["AccuracyLower"]],
                                           "CI_95%"=rpart_down_con_mat[["overall"]][["AccuracyUpper"]],
                                           "No_Information_Rate"=rpart_down_con_mat[["overall"]][["AccuracyNull"]],
                                           "Kappa"=rpart_down_con_mat[["overall"]][["Kappa"]],
                                           "Precision"=rpart_down_con_mat[["byClass"]][["Precision"]],
                                           "Recall"=rpart_down_con_mat[["byClass"]][["Recall"]],
                                           "F1"=rpart_down_con_mat[["byClass"]][["F1"]],
                                           "Prevalence"=rpart_down_con_mat[["byClass"]][["Prevalence"]],
                                           "Detection_Rate"=rpart_down_con_mat[["byClass"]][["Detection Rate"]],
                                           "Detection_Prevalence"=rpart_down_con_mat[["byClass"]][["Detection Prevalence"]],
                                           "Balanced_Accuracy"=rpart_down_con_mat[["byClass"]][["Balanced Accuracy"]],
                                           "Time"="0 min 3 sec"))
rm(predict_rpart_down,model_rpart_down,rpart_down_con_mat)

# smote Decision Tree Classifier (CART) rpart 1 parameter
system.time(model_rpart_smote<-caret::train(Churn~DSL+Fiber+Contract_Montly+Contract_One_Year+EFT+Credit_Card+SeniorCitizen+Dependents+
                                              OnlineSecurity+OnlineBackup+DeviceProtection+TechSupport+StreamingMovies+PaperlessBilling, 
                                            method="rpart",
                                            data=na.omit(smote_train),
                                            tuneLength=10,
                                            metric="Kappa",
                                            trControl= trainControl(summaryFunction=multiClassSummary)))
# smote Time. user:4.92 system:0.11 elapsed:5.03
# Saving model Decision Tree Classifier (CART)
saveRDS(model_rpart_smote, file="D:/Models/model_rpart_smote.rds")
# Reading model Decision Tree Classifier (CART)
model_rpart_smote<-readRDS("D:/Models/model_rpart_smote.rds")
# Confusion matrix Decision Tree Classifier (CART)
predict_rpart_smote<-predict(model_rpart_smote, na.omit(df_test))
rpart_smote_con_mat<-confusionMatrix(data=predict_rpart_smote, na.omit(df_test$Churn),mode="prec_recall")
# Variable importance Decision Tree Classifier (CART)
plot(varImp(model_rpart_smote),main="rpart variable importance smote") #5x5
rpart_smote_Evaluation<-as.data.frame(cbind("Model_Type"=model_rpart_smote$method,
                                            "Accuracy"=rpart_smote_con_mat[["overall"]][["Accuracy"]],
                                            "CI_5%"=rpart_smote_con_mat[["overall"]][["AccuracyLower"]],
                                            "CI_95%"=rpart_smote_con_mat[["overall"]][["AccuracyUpper"]],
                                            "No_Information_Rate"=rpart_smote_con_mat[["overall"]][["AccuracyNull"]],
                                            "Kappa"=rpart_smote_con_mat[["overall"]][["Kappa"]],
                                            "Precision"=rpart_smote_con_mat[["byClass"]][["Precision"]],
                                            "Recall"=rpart_smote_con_mat[["byClass"]][["Recall"]],
                                            "F1"=rpart_smote_con_mat[["byClass"]][["F1"]],
                                            "Prevalence"=rpart_smote_con_mat[["byClass"]][["Prevalence"]],
                                            "Detection_Rate"=rpart_smote_con_mat[["byClass"]][["Detection Rate"]],
                                            "Detection_Prevalence"=rpart_smote_con_mat[["byClass"]][["Detection Prevalence"]],
                                            "Balanced_Accuracy"=rpart_smote_con_mat[["byClass"]][["Balanced Accuracy"]],
                                            "Time"="0 min 5 sec"))
rm(predict_rpart_smote,model_rpart_smote,rpart_smote_con_mat)

# dowm Random forest 1 parameter
set.seed(321)
cl <- makeCluster(3/4 * detectCores())
registerDoParallel(cl)
system.time(model_rf_down<-caret::train(Churn~DSL+Fiber+Contract_Montly+Contract_One_Year+EFT+Credit_Card+SeniorCitizen+Dependents+
                                          OnlineSecurity+OnlineBackup+DeviceProtection+TechSupport+StreamingMovies+PaperlessBilling, 
                                        method="rf",
                                        data=na.omit(down_train),
                                        allowParallel=TRUE,
                                        tuneLength=10,
                                        metric="Kappa",
                                        trControl= trainControl(summaryFunction=multiClassSummary)))
stopCluster(cl); remove(cl)
registerDoSEQ()
# down Time. user:2.29 system:0.18 elapsed:103.61
# Saving model Random forest
saveRDS(model_rf_down, file="D:/Models/model_rf_down.rds")
# Reading model Random forest
model_rf_down<-readRDS("D:/Models/model_rf_down.rds")
# Confusion matrix Random forest
predict_rf_down<-predict(model_rf_down, na.omit(df_test))
rf_down_con_mat<-confusionMatrix(data=predict_rf_down, na.omit(df_test$Churn),mode="prec_recall")
# Variable importance Random forest
plot(varImp(model_rf_down),main="rf variable importance down") #5x5
rf_down_Evaluation<-as.data.frame(cbind("Model_Type"=model_rf_down$method,
                                        "Accuracy"=rf_down_con_mat[["overall"]][["Accuracy"]],
                                        "CI_5%"=rf_down_con_mat[["overall"]][["AccuracyLower"]],
                                        "CI_95%"=rf_down_con_mat[["overall"]][["AccuracyUpper"]],
                                        "No_Information_Rate"=rf_down_con_mat[["overall"]][["AccuracyNull"]],
                                        "Kappa"=rf_down_con_mat[["overall"]][["Kappa"]],
                                        "Precision"=rf_down_con_mat[["byClass"]][["Precision"]],
                                        "Recall"=rf_down_con_mat[["byClass"]][["Recall"]],
                                        "F1"=rf_down_con_mat[["byClass"]][["F1"]],
                                        "Prevalence"=rf_down_con_mat[["byClass"]][["Prevalence"]],
                                        "Detection_Rate"=rf_down_con_mat[["byClass"]][["Detection Rate"]],
                                        "Detection_Prevalence"=rf_down_con_mat[["byClass"]][["Detection Prevalence"]],
                                        "Balanced_Accuracy"=rf_down_con_mat[["byClass"]][["Balanced Accuracy"]],
                                        "Time"="1 min 43 sec"))
rm(predict_rf_down,model_rf_down,rf_down_con_mat)

# smote Random forest 1 parameter
set.seed(321)
cl <- makeCluster(3/4 * detectCores())
registerDoParallel(cl)
system.time(model_rf_smote<-caret::train(Churn~DSL+Fiber+Contract_Montly+Contract_One_Year+EFT+Credit_Card+SeniorCitizen+Dependents+
                                           OnlineSecurity+OnlineBackup+DeviceProtection+TechSupport+StreamingMovies+PaperlessBilling, 
                                         method="rf",
                                         data=na.omit(smote_train),
                                         allowParallel=TRUE,
                                         tuneLength=10,
                                         metric="Kappa",
                                         trControl= trainControl(summaryFunction=multiClassSummary)))
stopCluster(cl); remove(cl)
registerDoSEQ()
# smote Time. user:7.46 system:0.21 elapsed:436.97
# Saving model Random forest
saveRDS(model_rf_smote, file="D:/Models/model_rf_smote.rds")
# Reading model Random forest
model_rf_smote<-readRDS("D:/Models/model_rf_smote.rds")
# Confusion matrix Random forest
predict_rf_smote<-predict(model_rf_smote, na.omit(df_test))
rf_smote_con_mat<-confusionMatrix(data=predict_rf_smote, na.omit(df_test$Churn),mode="prec_recall")
# Variable importance Random forest
plot(varImp(model_rf_smote),main="rf variable importance smote") #5x5
rf_smote_Evaluation<-as.data.frame(cbind("Model_Type"=model_rf_smote$method,
                                         "Accuracy"=rf_smote_con_mat[["overall"]][["Accuracy"]],
                                         "CI_5%"=rf_smote_con_mat[["overall"]][["AccuracyLower"]],
                                         "CI_95%"=rf_smote_con_mat[["overall"]][["AccuracyUpper"]],
                                         "No_Information_Rate"=rf_smote_con_mat[["overall"]][["AccuracyNull"]],
                                         "Kappa"=rf_smote_con_mat[["overall"]][["Kappa"]],
                                         "Precision"=rf_smote_con_mat[["byClass"]][["Precision"]],
                                         "Recall"=rf_smote_con_mat[["byClass"]][["Recall"]],
                                         "F1"=rf_smote_con_mat[["byClass"]][["F1"]],
                                         "Prevalence"=rf_smote_con_mat[["byClass"]][["Prevalence"]],
                                         "Detection_Rate"=rf_smote_con_mat[["byClass"]][["Detection Rate"]],
                                         "Detection_Prevalence"=rf_smote_con_mat[["byClass"]][["Detection Prevalence"]],
                                         "Balanced_Accuracy"=rf_smote_con_mat[["byClass"]][["Balanced Accuracy"]],
                                         "Time"="7 min 16 sec"))
rm(predict_rf_smote,model_rf_smote,rf_smote_con_mat)

# down Random forest ranger 3 parameters
system.time(model_ranger_down<-caret::train(Churn~DSL+Fiber+Contract_Montly+Contract_One_Year+EFT+Credit_Card+SeniorCitizen+Dependents+
                                              OnlineSecurity+OnlineBackup+DeviceProtection+TechSupport+StreamingMovies+PaperlessBilling, 
                                            method="ranger",
                                            importance = 'impurity',
                                            data=na.omit(down_train),
                                            tuneLength=10,
                                            metric="Kappa",
                                            trControl= trainControl(summaryFunction=multiClassSummary,classProbs=TRUE)))
# down Time. user:1231.02 system:22.53 elapsed:273.14
# Saving model Random forest ranger
saveRDS(model_ranger_down, file="D:/Models/model_ranger_down.rds")
# Reading model Random forest ranger
model_ranger_down<-readRDS("D:/Models/model_ranger_down.rds")
# Confusion matrix Random forest ranger
predict_ranger_down<-predict(model_ranger_down, na.omit(df_test))
ranger_down_con_mat<-confusionMatrix(data=predict_ranger_down, na.omit(df_test$Churn),mode="prec_recall")
# Variable importance Random forest ranger
plot(varImp(model_ranger_down),main="ranger variable importance down") #5x5
ranger_down_Evaluation<-as.data.frame(cbind("Model_Type"=model_ranger_down$method,
                                            "Accuracy"=ranger_down_con_mat[["overall"]][["Accuracy"]],
                                            "CI_5%"=ranger_down_con_mat[["overall"]][["AccuracyLower"]],
                                            "CI_95%"=ranger_down_con_mat[["overall"]][["AccuracyUpper"]],
                                            "No_Information_Rate"=ranger_down_con_mat[["overall"]][["AccuracyNull"]],
                                            "Kappa"=ranger_down_con_mat[["overall"]][["Kappa"]],
                                            "Precision"=ranger_down_con_mat[["byClass"]][["Precision"]],
                                            "Recall"=ranger_down_con_mat[["byClass"]][["Recall"]],
                                            "F1"=ranger_down_con_mat[["byClass"]][["F1"]],
                                            "Prevalence"=ranger_down_con_mat[["byClass"]][["Prevalence"]],
                                            "Detection_Rate"=ranger_down_con_mat[["byClass"]][["Detection Rate"]],
                                            "Detection_Prevalence"=ranger_down_con_mat[["byClass"]][["Detection Prevalence"]],
                                            "Balanced_Accuracy"=ranger_down_con_mat[["byClass"]][["Balanced Accuracy"]],
                                            "Time"="4 min 33 sec"))
rm(predict_ranger_down,model_ranger_down,ranger_down_con_mat)

# smote Random forest ranger 3 parameters
system.time(model_ranger_smote<-caret::train(Churn~DSL+Fiber+Contract_Montly+Contract_One_Year+EFT+Credit_Card+SeniorCitizen+Dependents+
                                               OnlineSecurity+OnlineBackup+DeviceProtection+TechSupport+StreamingMovies+PaperlessBilling, 
                                             method="ranger",
                                             importance = 'impurity',
                                             data=na.omit(smote_train),
                                             tuneLength=10,
                                             metric="Kappa",
                                             trControl= trainControl(summaryFunction=multiClassSummary,classProbs=TRUE)))
# smote Time. user:5767.79 system:55.64 elapsed:1068.94 
# Saving model Random forest ranger
saveRDS(model_ranger_smote, file="D:/Models/model_ranger_smote.rds")
# Reading model Random forest ranger
model_ranger_smote<-readRDS("D:/Models/model_ranger_smote.rds")
# Confusion matrix Random forest ranger
predict_ranger_smote<-predict(model_ranger_smote, na.omit(df_test))
ranger_smote_con_mat<-confusionMatrix(data=predict_ranger_smote, na.omit(df_test$Churn),mode="prec_recall")
# Variable importance Random forest ranger
plot(varImp(model_ranger_smote),main="ranger variable importance smote") #5x5
ranger_smote_Evaluation<-as.data.frame(cbind("Model_Type"=model_ranger_smote$method,
                                             "Accuracy"=ranger_smote_con_mat[["overall"]][["Accuracy"]],
                                             "CI_5%"=ranger_smote_con_mat[["overall"]][["AccuracyLower"]],
                                             "CI_95%"=ranger_smote_con_mat[["overall"]][["AccuracyUpper"]],
                                             "No_Information_Rate"=ranger_smote_con_mat[["overall"]][["AccuracyNull"]],
                                             "Kappa"=ranger_smote_con_mat[["overall"]][["Kappa"]],
                                             "Precision"=ranger_smote_con_mat[["byClass"]][["Precision"]],
                                             "Recall"=ranger_smote_con_mat[["byClass"]][["Recall"]],
                                             "F1"=ranger_smote_con_mat[["byClass"]][["F1"]],
                                             "Prevalence"=ranger_smote_con_mat[["byClass"]][["Prevalence"]],
                                             "Detection_Rate"=ranger_smote_con_mat[["byClass"]][["Detection Rate"]],
                                             "Detection_Prevalence"=ranger_smote_con_mat[["byClass"]][["Detection Prevalence"]],
                                             "Balanced_Accuracy"=ranger_smote_con_mat[["byClass"]][["Balanced Accuracy"]],
                                             "Time"="17 min 48 sec"))
rm(predict_ranger_smote,model_ranger_smote,ranger_smote_con_mat)

# down Neural Network 2 parameters 
set.seed(321)
cl <- makeCluster(3/4 * detectCores())
registerDoParallel(cl)
system.time(model_nnet_down<-caret::train(Churn~DSL+Fiber+Contract_Montly+Contract_One_Year+EFT+Credit_Card+SeniorCitizen+Dependents+
                                            OnlineSecurity+OnlineBackup+DeviceProtection+TechSupport+StreamingMovies+PaperlessBilling, 
                                          method="nnet",
                                          data=na.omit(down_train),
                                          allowParallel=TRUE,
                                          tuneLength=10,
                                          metric="Kappa",
                                          trControl= trainControl(summaryFunction=multiClassSummary)))
stopCluster(cl); remove(cl)
registerDoSEQ()
# down Time. user:3.11 system:0.29 elapsed:450.53
# Saving model Neural Network
saveRDS(model_nnet_down, file="D:/Models/model_nnet_down.rds")
# Reading model Neural Network
model_nnet_down<-readRDS("D:/Models/model_nnet_down.rds")
# Confusion matrix Neural Network
predict_nnet_down<-predict(model_nnet_down, na.omit(df_test))
nnet_down_con_mat<-confusionMatrix(data=predict_nnet_down, na.omit(df_test$Churn),mode="prec_recall")
# Variable importance Neural Network
plot(varImp(model_nnet_down),main="nnet variable importance down") #5x5
nnet_down_Evaluation<-as.data.frame(cbind("Model_Type"=model_nnet_down$method,
                                          "Accuracy"=nnet_down_con_mat[["overall"]][["Accuracy"]],
                                          "CI_5%"=nnet_down_con_mat[["overall"]][["AccuracyLower"]],
                                          "CI_95%"=nnet_down_con_mat[["overall"]][["AccuracyUpper"]],
                                          "No_Information_Rate"=nnet_down_con_mat[["overall"]][["AccuracyNull"]],
                                          "Kappa"=nnet_down_con_mat[["overall"]][["Kappa"]],
                                          "Precision"=nnet_down_con_mat[["byClass"]][["Precision"]],
                                          "Recall"=nnet_down_con_mat[["byClass"]][["Recall"]],
                                          "F1"=nnet_down_con_mat[["byClass"]][["F1"]],
                                          "Prevalence"=nnet_down_con_mat[["byClass"]][["Prevalence"]],
                                          "Detection_Rate"=nnet_down_con_mat[["byClass"]][["Detection Rate"]],
                                          "Detection_Prevalence"=nnet_down_con_mat[["byClass"]][["Detection Prevalence"]],
                                          "Balanced_Accuracy"=nnet_down_con_mat[["byClass"]][["Balanced Accuracy"]],
                                          "Time"="7 min 30 sec"))
rm(predict_nnet_down,model_nnet_down,nnet_down_con_mat)

# smote Neural Network 2 parameters 
set.seed(321)
cl <- makeCluster(3/4 * detectCores())
registerDoParallel(cl)
system.time(model_nnet_smote<-caret::train(Churn~DSL+Fiber+Contract_Montly+Contract_One_Year+EFT+Credit_Card+SeniorCitizen+Dependents+
                                             OnlineSecurity+OnlineBackup+DeviceProtection+TechSupport+StreamingMovies+PaperlessBilling, 
                                           method="nnet",
                                           data=na.omit(smote_train),
                                           allowParallel=TRUE,
                                           tuneLength=10,
                                           metric="Kappa",
                                           trControl= trainControl(summaryFunction=multiClassSummary)))
stopCluster(cl); remove(cl)
registerDoSEQ()
# smote Time. user:9.71 system:0.56 elapsed:1789.67
# Saving model Neural Network
saveRDS(model_nnet_smote, file="D:/Models/model_nnet_smote.rds")
# Reading model Neural Network
model_nnet_smote<-readRDS("D:/Models/model_nnet_smote.rds")
# Confusion matrix Neural Network
predict_nnet_smote<-predict(model_nnet_smote, na.omit(df_test))
nnet_smote_con_mat<-confusionMatrix(data=predict_nnet_smote, na.omit(df_test$Churn),mode="prec_recall")
# Variable importance Neural Network
plot(varImp(model_nnet_smote),main="nnet variable importance smote") #5x5
nnet_smote_Evaluation<-as.data.frame(cbind("Model_Type"=model_nnet_smote$method,
                                           "Accuracy"=nnet_smote_con_mat[["overall"]][["Accuracy"]],
                                           "CI_5%"=nnet_smote_con_mat[["overall"]][["AccuracyLower"]],
                                           "CI_95%"=nnet_smote_con_mat[["overall"]][["AccuracyUpper"]],
                                           "No_Information_Rate"=nnet_smote_con_mat[["overall"]][["AccuracyNull"]],
                                           "Kappa"=nnet_smote_con_mat[["overall"]][["Kappa"]],
                                           "Precision"=nnet_smote_con_mat[["byClass"]][["Precision"]],
                                           "Recall"=nnet_smote_con_mat[["byClass"]][["Recall"]],
                                           "F1"=nnet_smote_con_mat[["byClass"]][["F1"]],
                                           "Prevalence"=nnet_smote_con_mat[["byClass"]][["Prevalence"]],
                                           "Detection_Rate"=nnet_smote_con_mat[["byClass"]][["Detection Rate"]],
                                           "Detection_Prevalence"=nnet_smote_con_mat[["byClass"]][["Detection Prevalence"]],
                                           "Balanced_Accuracy"=nnet_smote_con_mat[["byClass"]][["Balanced Accuracy"]],
                                           "Time"="29 min 49 sec"))
rm(predict_nnet_smote,model_nnet_smote,nnet_smote_con_mat)

# Create Evaluation table
Evaluation_down<-as.data.frame(rbind(glm_down_Evaluation,svm_down_Evaluation,rpart_down_Evaluation,
                                     rf_down_Evaluation,ranger_down_Evaluation,nnet_down_Evaluation))
write.csv(Evaluation_down,"D:/Evaluation_Down.csv",
          row.names = FALSE)

Evaluation_smote<-as.data.frame(rbind(glm_smote_Evaluation,svm_smote_Evaluation,rpart_smote_Evaluation,
                                      rf_smote_Evaluation,ranger_smote_Evaluation,nnet_smote_Evaluation))
write.csv(Evaluation_smote,"D:/Evaluation_Smote.csv",
          row.names = FALSE)
rm(glm_smote_Evaluation,nnet_smote_Evaluation,ranger_smote_Evaluation,rf_smote_Evaluation,rpart_smote_Evaluation,svm_smote_Evaluation,
   glm_down_Evaluation,nnet_down_Evaluation,ranger_down_Evaluation,rf_down_Evaluation,rpart_down_Evaluation,svm_down_Evaluation,
   df_test,down_train,smote_train,Evaluation_down,Evaluation_smote)

# Visualising results
df<-read.csv("Cleaned_Data.csv")
df<-df %>% 
  mutate(Contract_Montly=ifelse(Contract_Montly==0,"No","Yes")) %>%
  mutate(Fiber=ifelse(Fiber==0,"No","Yes")) %>%
  mutate(OnlineSecurity=ifelse(OnlineSecurity==0,"No","Yes")) %>%
  mutate(PaperlessBilling=ifelse(PaperlessBilling==0,"No","Yes")) %>%
  mutate(TechSupport=ifelse(TechSupport==0,"No","Yes")) %>%
  mutate_if(is.character,as.factor)
df_Churn_No<-subset(df,Churn=="No")
df_Churn_Yes<-subset(df,Churn=="Yes")
par(mfrow=c(1,3)) #7.7X5
plot(df$Contract_Montly,main="Contract_Montly class frequencies",xlab="Contract_Montly classes",ylab="Frequency")
plot(df_Churn_No$Contract_Montly,main="Churn = No",xlab="Contract_Montly classes",ylab="Frequency")
plot(df_Churn_Yes$Contract_Montly,main="Churn = Yes",xlab="Contract_Montly classes",ylab="Frequency")

par(mfrow=c(1,3))#7.7X5
plot(df$Fiber,main="Fiber class frequencies",xlab="Fiber classes",ylab="Frequency")
plot(df_Churn_No$Fiber,main="Churn = No",xlab="Fiber classes",ylab="Frequency")
plot(df_Churn_Yes$Fiber,main="Churn = Yes",xlab="Fiber classes",ylab="Frequency")

df_Churn_Yes<-subset(df,Churn=="Yes")
df_Churn_Fiber_Yes<-subset(df_Churn_Yes,Fiber=="Yes")
plot(df_Churn_Fiber_Yes$Contract_Montly,main="Fiber customers leaving",xlab="Contract_Montly classes",ylab="Frequency")

# Load OLD data to plot different internet services
df_Old<-read.csv("IBM_Churn_Data.csv") #5x5
plot(df_Old$InternetService,main="Internet services",xlab="Internet classes",ylab="Frequency")
plot(df_Old$Contract,main="Contractual term",xlab="Contract classes",ylab="Frequency")

df_Old_Churn_Yes<-subset(df_Old,Churn=="Yes")
df_Old_Churn_Fiber_Yes<-subset(df_Old_Churn_Yes,InternetService=="Fiber optic")
hist(df_Old_Churn_Fiber_Yes$tenure,breaks=72,main="Number of months before fiber clients left",xlab="Tenure",ylab="Frequency")

# 3rd 4th and 5th important features from ensamble models
par(mfrow=c(1,3)) #7.7X5
plot(df$OnlineSecurity,main="OnlineSecurity class frequencies",xlab="OnlineSecurity classes",ylab="Frequency")
plot(df_Churn_No$OnlineSecurity,main="Churn = No",xlab="OnlineSecurity classes",ylab="Frequency")
plot(df_Churn_Yes$OnlineSecurity,main="Churn = Yes",xlab="OnlineSecurity classes",ylab="Frequency")

par(mfrow=c(1,3)) #7.7X5
plot(df$PaperlessBilling,main="PaperlessBilling class frequencies",xlab="PaperlessBilling classes",ylab="Frequency")
plot(df_Churn_No$PaperlessBilling,main="Churn = No",xlab="PaperlessBilling classes",ylab="Frequency")
plot(df_Churn_Yes$PaperlessBilling,main="Churn = Yes",xlab="PaperlessBilling classes",ylab="Frequency")

par(mfrow=c(1,3)) #7.7X5
plot(df$TechSupport,main="TechSupport class frequencies",xlab="TechSupport classes",ylab="Frequency")
plot(df_Churn_No$TechSupport,main="Churn = No",xlab="TechSupport classes",ylab="Frequency")
plot(df_Churn_Yes$TechSupport,main="Churn = Yes",xlab="TechSupport classes",ylab="Frequency")

rm(df_Churn_No,df_Churn_Yes,df_Old,df,df_Old_Churn_Yes,df_Old_Churn_Fiber_Yes,df_Churn_Fiber_Yes)

# Calculating probability scores
predict_ranger_smote<-predict(model_ranger_smote, type = "prob")
