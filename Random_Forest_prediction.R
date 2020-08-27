### Logistic regression model
#data = read.csv(file.choose())
#setwd(dlgDir(default = getwd())$res)
#source("Home_Page.R")
#data = read.csv(file.choose())
library(svDialogs)
library(tcltk)
library(randomForest)
#install.packages("party", dependencies = TRUE)
library(party)
random_one_pred = function(data){
  
  #data = read.csv(file.choose())
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
  
  id = dlgList(c(colnames(data)), multiple = FALSE, title = "Select Unique ID column or Variable")$res
  data1 = cbind(data[,c(id)], data[,c(de)], data[,c(ind)])
  
  data1[is.na(data1)] = 0
  ll = colnames(data1)[sapply(data1, is.factor)]
  if(length(ll) > 0){
    for(i in 1:length(ll)){
    data1[,ll[i]] = as.numeric(data1[,ll[i]])
    }
  }
  str(data1)
  
  names(data1)[2] = print(de)
  
  smp_size <- floor(0.75 * nrow(data1))
  
  ## set the seed to make your partition reproductible
  set.seed(123)
  train_ind <- sample(seq_len(nrow(data1)), size = smp_size)
  
  train_data <- data1[train_ind, ]
  train_data = train_data[,-1]
  test_data <- data1[-train_ind, ]
  test_data1 = test_data[,-c(1:2)]
  
  if(sum(is.na(data1)) > 0 ){
    msgBox <- tkmessageBox(title = "UnSuccessesful to Run the Random Forest Model",message = "Missing values are thier in Your dataset, So please remove missing values in your dataset", icon = "info", type = "ok")
  } else {
    names(train_data)[1] = "dependent"
    #set.seed(50)
    library(randomForest)
    train.fit = randomForest(dependent ~ ., data = train_data, importance = TRUE, ntree = 1000)
    #test_data = test_data[,-c(de)]
    #varImpPlot(train.fit)
    data_predict = predict(train.fit, test_data1)
    data_predict = as.data.frame(data_predict)
    names(data_predict)[1] = "Predicted_data"
    test_data = cbind(test_data, data_predict)
    write.csv(test_data,"Predicted_dataset_rf_classification.csv")
    #table(test_data$default, test_data$Predicted_data)
    msgBox <- tkmessageBox(title = "Model Run",message = "Successesfully Run the Random Forest Model, Please Check your directory to view prediction", icon = "info", type = "ok")
  }
}
#library(rpart)
#train.fit = cforest(as.factor(dependent) ~ ., data = train_data, controls=cforest_unbiased(ntree=2000, mtry=3))


random_two_pred = function(train, test){
  de =  dlgList(c(colnames(train)), multiple = FALSE, title = "Select the Dependent Variable")$res
  if (!length(de)) {
    msgBox <- tkmessageBox(title = "Select Dependent Variable",message = "Can You please Select the Dependent Variable To Continue...", icon = "info", type = "ok")
  } else {
    msgBox <- tkmessageBox(title = "You Selected Dependent Variable is",message = print(de), icon = "info", type = "ok")
  }
  
  library(tcltk)
  ind = dlgList(c(colnames(train)), multiple = TRUE, title = "Select the InDependent Variable")$res
  if (!length(ind)) {
    msgBox <- tkmessageBox(title = "Select InDependent Variable",message = "Can You please Select the InDependent Variable", icon = "info", type = "ok")
  } else {
    msgBox <- tkmessageBox(title = "Selected  InDependent Variable",message = print(ind), icon = "info", type = "ok")
  }
  
  train_dep = cbind(train[,c(de)],train[,c(ind)])
  ll = colnames(train_dep)[sapply(train_dep, is.factor)]
  if(length(ll) > 0){
    for(i in 1:length(ll)){
      train_dep[,ll[i]] = as.numeric(train_dep[,ll[i]])
    }
  }
  train_dep[is.na(train_dep)] = 0
  test_data[is.na(test_data)] = 0
  test_data = test[,c(ind)]
  ll = colnames(test_data)[sapply(test_data, is.factor)]
  if(length(ll) > 0){
    for(i in 1:length(ll)){
      test_data[,ll[i]] = as.numeric(test_data[,ll[i]])
    }
  }
  if(sum(is.na(train_dep)) > 0 & sum(is.na(test_data)) > 0 ){
    msgBox <- tkmessageBox(title = "UnSuccessesful to Run the Logistic Model",message = "Sorry Please try again..", icon = "info", type = "ok")
  } else {
    names(train_dep)[1] = "dependent"
    train.fit = randomForest(dependent ~ ., data = train_dep, importance = TRUE, ntree = 1000)
    
    
    data_predict = predict(train.fit, test_data, OOB=TRUE, type = "response")
    
    data_predict = as.data.frame(data_predict)
    names(data_predict)[1] = "Predicted_data"
    test = cbind(test, data_predict)
    write.csv(test,"Predicted_dataset_logistic.csv")
    msgBox <- tkmessageBox(title = "Model Run",message = "Successesfully Run the Random Forest Model, Please Check your directory to view prediction", icon = "info", type = "ok")
  }
}

#data = mtcars
ll = menu(c("Split Dataset and Use","Already you have Train and Test Dataset"), graphics = TRUE, title = "Please Select")
if(ll == 1){
  random_one_pred(data)
} else{
  random_two_pred(train, test)
}
source("Home_Page.R")

#?tkmessage
#library(gWidgets)
#library(gWidgetstcltk)
#win <- gwindow("Tab delimited file upload example")
#grp_name <- ggroup(container = win)
#lbl_data_frame_name <- glabel(
#"Variable to save data to: ",
#container = grp_name

