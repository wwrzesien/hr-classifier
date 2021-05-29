library(RcppCNPy)
library(e1071)
library(mice)
library(ggplot2)
library(caTools)
library(caret)
library(skimr)
library(pROC)
library(ROCR)
library(xgboost)

# load data
train <- read.csv("./data/data_scientists_dataset/aug_train.csv", na.strings=c(""," ","NA"))
test <- read.csv("./data/data_scientists_dataset/aug_test.csv", na.strings=c(""," ","NA"))
results <- npyLoad("./data/test_answers.npy")

# add target results to test data
test$target = results

str(train)
str(test)

# fix typing
train$company_size <- gsub("/","-",train$company_size)
train$company_size
test$company_size <- gsub("/","-",test$company_size)
test$company_size

# change char to factor
train$city <- as.factor(train$city)
train$gender <- as.factor(train$gender)
train$relevent_experience <- as.factor(train$relevent_experience)
train$enrolled_university <- as.factor(train$enrolled_university)
train$education_level <- as.factor(train$education_level)
train$major_discipline <- as.factor(train$major_discipline)
train$experience <- as.factor(train$experience)
train$company_size <- as.factor(train$company_size)
train$company_type <- as.factor(train$company_type)
train$last_new_job <- as.factor(train$last_new_job)
train$training_hours <- as.integer(train$training_hours)
# train$target <- as.factor(train$target)

test$city <- as.factor(test$city)
test$gender <- as.factor(test$gender)
test$relevent_experience <- as.factor(test$relevent_experience)
test$enrolled_university <- as.factor(test$enrolled_university)
test$education_level <- as.factor(test$education_level)
test$major_discipline <- as.factor(test$major_discipline)
test$experience <- as.factor(test$experience)
test$company_size <- as.factor(test$company_size)
test$company_type <- as.factor(test$company_type)
test$last_new_job <- as.factor(test$last_new_job)
test$training_hours <- as.integer(test$training_hours)

# descriptive statistics
skimmed_train <- skim(train)
skimmed_train
skimmed_test <- skim(test)
skimmed_test

# # visualize empty values
# na_df <- data.frame(variable = c("enrolee_id","city","city_development_index","gender","relevent_experience","enrolled_university","education_level","major_discipline","experience","company_size","company_type","last_new_job","training_hours","target"),
#                     total_na = c(0,0,0,4508,0,386,460,2813,65,5938,6140,423,0,0))
# 
# 
# na_df$variable <- factor(na_df$variable,
#                          levels = na_df$variable[order(na_df$total_na,decreasing = TRUE)])
# 
# ggplot(na_df,aes(x=variable,y=total_na))+
#   geom_bar(stat = "identity",col = "blue")+
#     theme(axis.text.x = element_text(angle = 90, hjust =1, vjust = 0.5))+
#     geom_label(aes(label = total_na, size = NULL), nudge_y = 0.7)+
#     theme(plot.title = element_text(hjust = 0.5))+
#     ggtitle("Total NA's by each col")+
#     xlab("Missing Values")
    # ylab("Total NA's")


# # remove NA values with mice
# sum(is.na(train))
# imp <- mice(train, m=5, method=c("","","","pmm","pmm","pmm","pmm","pmm","pmm","pmm","pmm","pmm","",""), maxit=1)
# train_imp <- complete(imp, 5)
# sum(is.na(train_imp))

# remove NA with omit
sum(is.na(train))
train_clear <- na.omit(train)
sum(is.na(train_clear))

test_clear <- na.omit(test)

set.seed(123)

# source("svm.R")
# resutls_svm <- svm_classification(train, test)

train_data <- train_clear
test_data <- test_clear

str(train_data)
str(test_data)

# dummy it, dummy models same for train and test set
dummy_model <- dummyVars("~ .", data=train_data)
train_dummy <- data.frame(predict(dummy_model, newdata=train_data))
str(train_dummy)
sum(is.na(train_dummy))

test_dummy <- data.frame(predict(dummy_model, newdata=test_data))
str(test_dummy)

# for debugging purposes
train_data_head <- train_dummy[1:500, ]
test_data_head <- test_dummy[1:200, ]

# # split data - not necessary, 
# split = sample.split(train_data_head$target, SplitRatio=0.75)
# train_set = subset(train_data_head, split==TRUE)
# test_set = subset(train_data_head, split==FALSE)
# 
# sum(is.na(train_set))
# sum(is.na(test_set))
# 
# # feature scaling
# train_set[-188] = scale(train_set[-188])
# test_set[-188] = scale(test_set[-188])

# SVM  with k-fold cross validation
# k-fold cross validation
# folds = createFolds(train_data_head$target, k=5)
# cv = lapply(folds, function(x) {
#   train_fold = train_data_head[-x, ]
#   
#   test_fold = train_data_head[x, ]
#   
#   svm_classifier = svm(formula=target ~ ., 
#                        data=train_fold,
#                        type='C-classification',
#                        kernel='radial')
#   
#   y_pred = predict(svm_classifier, newdata=test_fold[-188])
#   
#   cm = table(test_fold[, 188], y_pred)
#   
#   accuracy = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
#   
#   return(accuracy)
# })

# accuracy = mean(as.numeric(cv))
# accuracy

############# SVM 
svm_classifier <- svm(formula=target~., 
                      data=train_data_head,
                      type='C-classification',
                      kernel='radial',
                      probability=TRUE, 
                      cross=5, 
                      verbosity=3)

# test data
y_pred = predict(svm_classifier, newdata=test_data_head[-188], probability = TRUE)
summary(y_pred)
# confusion matrix, TP, PF, FN, FP, accuracy, sensitivity, specificity
cm = confusionMatrix(table(y_pred, test_data_head$target))
cm

# AUC
multiclass.roc(test_data_head$target, attr(y_pred, "probabilities"))

as.numeric(as.character(y_pred))
as.numeric(test_data_head$target)

# ROC
pred <- prediction(as.numeric(as.character(y_pred)), as.numeric(test_data_head$target))
perf <- performance(pred,"tpr","fpr")
plot(perf,col="blue")
abline(0,1)



# Gradient boosting, in xgboost after cgb.cv cannot perform predict
# str(train_data_head)
# dtrain <- xgb.DMatrix(data = as.matrix(train_data_head[-188]), label= as.matrix(train_data_head[188]))
# xgboostModelCV <- xgb.cv(data =  dtrain, 
#                          nrounds = 20, 
#                          nfold = 5, 
#                          nround=2,
#                          metrics = "auc", 
#                          verbose = TRUE, 
#                          "eval_metric" = "auc",
#                          "objective" = "binary:logistic", 
#                          "max.depth" = 3, 
#                          "eta" = 0.01,                               
#                          "subsample" = 0.5, 
#                          "colsample_bytree" = 1,
#                          print_every_n = 1, 
#                          "min_child_weight" = 1,
#                          booster = "gbtree",
#                          early_stopping_rounds = 10,
#                          seed = 1234)

############# Gradient boosting with caret
xgb_trcontrol = trainControl(method = "cv",
                               number = 5,
                               verboseIter = TRUE,
                               returnData = FALSE,
                               returnResamp = "all", 
                               allowParallel = TRUE)

xgb_grid = expand.grid(nrounds = c(1000,2000,3000,4000) ,
                         eta = c(0.01, 0.001, 0.0001),
                         lambda = 1,
                         alpha = 0)

typeof(as.matrix(train_data_head[c(-1, -188)]))
train_data_head[188]
xgb_train = train(x = as.matrix(train_data_head[c(-1, -188)]),
                    y = as.factor(train_data_head[,188]),
                    trControl = xgb_trcontrol,
                    tuneGrid = xgb_grid,
                    method = "xgbLinear",
                    max.depth = 5)


# test data
y_pred = predict(xgb_train, newdata=test_data_head[c(-1, -188)], probability = TRUE)
summary(y_pred)
# confusion matrix, TP, PF, FN, FP, accuracy, sensitivity, specificity
cm = confusionMatrix(table(y_pred, test_data_head$target))
cm

# AUC
multiclass.roc(test_data_head$target, as.numeric(as.character(y_pred)))

as.numeric(as.character(y_pred))
as.numeric(test_data_head$target)

# ROC
pred <- prediction(as.numeric(as.character(y_pred)), as.numeric(test_data_head$target))
perf <- performance(pred,"tpr","fpr")
plot(perf,col="blue")
abline(0,1)






