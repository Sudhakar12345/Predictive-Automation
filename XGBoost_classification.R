#http://blog.hackerearth.com/beginners-tutorial-on-xgboost-parameter-tuning-r
#data = read.csv("/home/vinayak/sudhakar/R/automation_model/train.csv")
#data[is.na(data)] = 0
#table(is.na(data))
library(dplyr)
library(xgboost)

library(Matrix)
library(data.table)
library(tcltk)
library(svDialogs)
classify_xgb_one  = function(data){
  id = dlgList(c(colnames(data)), multiple = FALSE, title = "Select Unique ID column or Variable")$res
  de =  dlgList(c(colnames(data)), multiple = FALSE, title = "Select the Dependent Variable")$res
  if (!length(de)) {
    msgBox <- tkmessageBox(title = "Select Dependent Variable",message = "Can You please Select the Dependent Variable To Continue...", icon = "info", type = "ok")
    de =  dlgList(c(colnames(data)), multiple = FALSE, title = "Select the Dependent Variable")$res
    if (!length(de)) {
      msgBox <- tkmessageBox(title = "Select Dependent Variable",message = "Sorry You didn't select any dependent variable...Now You are redirect to Home_Page", icon = "info", type = "ok")
      source("Home_Page.R")
    }
  } else {
    msgBox <- tkmessageBox(title = "You Selected Dependent Variable is",message = print(de), icon = "info", type = "ok")
  }
  
  ind = dlgList(c(colnames(data)), multiple = TRUE, title = "Select the InDependent Variable")$res
  if (!length(ind)) {
    msgBox <- tkmessageBox(title = "Select InDependent Variable",message = "Can You please Select the InDependent Variable", icon = "info", type = "ok")
    ind = dlgList(c(colnames(data)), multiple = TRUE, title = "Select the InDependent Variable")$res
    if (!length(ind)) {
      msgBox <- tkmessageBox(title = "Select Dependent Variable",message = "Sorry You didn't select any dependent variable...Now You are redirect to Home_Page", icon = "info", type = "ok")
      source("Home_Page.R")
    }
  } else {
    msgBox <- tkmessageBox(title = "Selected  InDependent Variable",message = print(ind), icon = "info", type = "ok")
  }
  
  
  data1 = cbind(data[,c(id)], data[,c(de)], data[,c(ind)])
  names(data1)[2] = print(de)
  one_data = data1
    #one_data$dependent = ifelse(one_data$dependent == 1,0,1)
    #one_data$dependent = as.numeric(one_data$dependent)
    #str(one_data)
    
    #one_data = sapply(one_data, as.numeric)
    str(one_data)
    smp_size <- floor(0.75 * nrow(one_data))
    
    ## set the seed to make your partition reproductible
    set.seed(123)
    train_ind <- sample(seq_len(nrow(one_data)), size = smp_size)
    
    train_data <- one_data[train_ind, ]
    train_data[is.na(train_data)] = 0
    test_data <- one_data[-train_ind, ]
    test_data[is.na(test_data)] = 0
    #test_data1 = test_data[,-c(1)]
    if(sum(is.na(train_data)) > 0 & sum(is.na(test_data)) > 0){
      msgBox <- tkmessageBox(title = "UnSuccessesful to Run the XGBoost Model Missing Values are their",message = "Sorry Please try again..", icon = "info", type = "ok")
    } else {
      #train_data1 = train_data[,-1]
      ### convert all categorical variables into matrix form
      train_data1 = train_data[,-1]
      test_data1 = test_data[,-1]
      labels = as.numeric(train_data1[,1])
      
      setDT(train_data1)
      setDT(test_data1)
      
      new_tr <- model.matrix(~.+0,data = train_data1[,-1])
      new_ts <- model.matrix(~.+0,data = test_data1[,-1])
      
      dtrain <- xgb.DMatrix(data = new_tr,label = labels)
      dtest <- xgb.DMatrix(data = new_ts)

      params <- list(
        booster = "gbtree",
        objective = "binary:logistic",
        eta=0.3,
        gamma=0,
        max_depth=6,
        min_child_weight=1,
        subsample=1,
        colsample_bytree=1
      )
      
      ## cross validation using xgboost
      # xgbcv <- xgb.cv(params = params
      #                 ,data = dtrain
      #                 ,nrounds = 100
      #                 ,nfold = 5
      #                 ,showsd = T
      #                 ,stratified = T
      #                 ,print.every.n = 10
      #                 ,early.stop.round = 20
      #                 ,maximize = F
      # )
      # ##best iteration = 79
      
      #first default - model training
      xgb1 <- xgb.train(
        params = params
        ,data = dtrain
        ,nrounds = 100
        ,eval_metric = "error"
      )
      
      data_predict = predict(xgb1 , dtest)
      data_predict = as.data.frame(round(data_predict))
      names(data_predict)[1] = "Predicted_data"
      test_data = cbind(test_data, data_predict)
      write.csv(test_data,"Predicted_dataset_xgb_class.csv")
      msgBox <- tkmessageBox(title = "Model Run",message = "Successesfully Run the XGBoost Clssification Model, Please Check your directory to view prediction", icon = "info", type = "ok")
    }
}

classify_xgb_two = function(train,test){
  de =  dlgList(c(colnames(train)), multiple = FALSE, title = "Select the Dependent Variable")$res
  if (!length(de)) {
    msgBox <- tkmessageBox(title = "Select Dependent Variable",message = "Can You please Select the Dependent Variable To Continue...", icon = "info", type = "ok")
    de =  dlgList(c(colnames(train)), multiple = FALSE, title = "Select the Dependent Variable")$res
    if (!length(de)) {
      msgBox <- tkmessageBox(title = "Select Dependent Variable",message = "You Didn't Select the Dependent Variable Now you are redirect Homepage", icon = "info", type = "ok")
      source("Home_Page.R")
    }
  } else {
    msgBox <- tkmessageBox(title = "You Selected Dependent Variable is",message = print(de), icon = "info", type = "ok")
  }
  
  library(tcltk)
  ind = dlgList(c(colnames(train)), multiple = TRUE, title = "Select the InDependent Variable")$res
  if (!length(ind)) {
    msgBox <- tkmessageBox(title = "Select InDependent Variable",message = "Can You please Select the InDependent Variable", icon = "info", type = "ok")
    ind = dlgList(c(colnames(train)), multiple = TRUE, title = "Select the InDependent Variable")$res
    if (!length(ind)) {
      msgBox <- tkmessageBox(title = "Select InDependent Variable",message = "Sorry you Didn't Select the InDependent Variable Now you are redirect Home Page", icon = "info", type = "ok")
      source("Home_Page.R")
    }
  } else {
    msgBox <- tkmessageBox(title = "Selected  InDependent Variable",message = print(ind), icon = "info", type = "ok")
  }
  
  train_data = cbind(train[,c(de)],train[,c(ind)])
  test_data = test[,c(ind)]
  names(train_data)[1] = de
  train_data[is.na(train_data)] = 0
  test_data[is.na(test_data)] = 0
  if(sum(is.na(train_data)) > 0 & sum(is.na(test_data)) > 0){
    msgBox <- tkmessageBox(title = "Sorry Please try again..",message = "UnSuccessesful to Run the XGBoost Model Missing Values are their", icon = "info", type = "ok")
    } else {
      #train_data1 = train_data[,-1]
      ### convert all categorical variables into matrix form
      labels = as.numeric(train_data[,1])
      
      setDT(train_data)
      setDT(test_data)
      ?setDT
      new_tr <- model.matrix(~.+0,data = train_data[,-1])
      new_ts <- model.matrix(~.+0,data = test_data)
      
      dtrain <- xgb.DMatrix(data = new_tr,label = labels)
      dtest <- xgb.DMatrix(data = new_ts)
      
      params <- list(
        booster = "gbtree",
        objective = "binary:logistic",
        eta=0.3,
        gamma=0,
        max_depth=6,
        min_child_weight=1,
        subsample=1,
        colsample_bytree=1
      )
      
      ## cross validation using xgboost
      # xgbcv <- xgb.cv(params = params
      #                 ,data = dtrain
      #                 ,nrounds = 100
      #                 ,nfold = 5
      #                 ,showsd = T
      #                 ,stratified = T
      #                 ,print.every.n = 10
      #                 ,early.stop.round = 20
      #                 ,maximize = F
      # )
      # ##best iteration = 79
      
      #first default - model training
      xgb1 <- xgb.train(
        params = params
        ,data = dtrain
        ,nrounds = 100
        ,eval_metric = "error"
      )
      
      data_predict = predict(xgb1 , dtest)
      data_predict = as.data.frame(round(data_predict))
      names(data_predict)[1] = "Predicted_data"
      test_data = cbind(test_data, data_predict)
      write.csv(test_data,"Predicted_dataset_xgb_class1.csv")
      msgBox <- tkmessageBox(title = "Model Run",message = "Successesfully Run the XGBoost Clssification Model, Please Check your directory to view prediction", icon = "info", type = "ok")
    
    
  }
}
#data = read.csv(file.choose())
#train = read.csv(file.choose())
#test = read.csv(file.choose())
ll = menu(c("Split Dataset and Use","Already you have Train and Test Dataset"), graphics = TRUE, title = "Please Select")
if(ll == 1){
  classify_xgb_one(data)
} else{
  classify_xgb_two(train,test)
}
source("Home_Page.R")
# 
# classify_xgb_one(data)
# classify_xgb_two(train,test)
# getwd()
