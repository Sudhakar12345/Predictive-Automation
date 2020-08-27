### Logistic regression model
#data = read.csv(file.choose())
library(svDialogs)
library(tcltk)

linear_one = function(data){
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
    msgBox <- tkmessageBox(title = "UnSuccessesful to Run the Logistic Model",message = "Sorry Please try again..", icon = "info", type = "ok")
  } else {
    train.fit = lm(train_data[,de] ~ ., data = train_data)
    #test_data = test_data[,-c(de)]
    data_predict = predict(train.fit, data = test_data1)
    data_predict = as.data.frame(data_predict)
    names(data_predict)[1] = "Predicted_data"
    test_data = cbind(test_data, data_predict)
    write.csv(test_data,"Predicted_dataset.csv")
    
    msgBox <- tkmessageBox(title = "Model Run",message = "Successesfully Run the Linear Model, Please Check your directory to view prediction", icon = "info", type = "ok")
  }
}


linear_two = function(train, test){
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
  if(sum(is.na(train)) > 0 & sum(is.na(test)) > 0){
    msgBox <- tkmessageBox(title = "UnSuccessesful to Run the Linear Model",message = "Missing Values are thier, So can't Continue Further", icon = "info", type = "ok")
  } else {
    names(train_dep)[1] = "dependent"
    train.fit = lm(dependent ~ ., data = train_dep)
    test_data = test[,c(ind)]
    data_predict = predict(train.fit, test_data)
    data_predict = as.data.frame(data_predict)
    names(data_predict)[1] = "Predicted_data"
    test = cbind(test, data_predict)
    write.csv(test,"Predicted_dataset.csv")
    msgBox <- tkmessageBox(title = "Model Run",message = "Successesfully Run the Linear Regression Model, Please Check your directory to view prediction", icon = "info", type = "ok")
  }
}

#data = mtcars
ll = menu(c("Split Dataset and Use","Already you have Train and Test Dataset"), graphics = TRUE, title = "Please Select")
if(ll == 1){
  linear_one(train)
} else{
  linear_two(train, test)
}
################ Call the function
library(tcltk)
de =  dlgList(c(ls()[sapply(mget(ls(), .GlobalEnv), is.data.frame)]), multiple = FALSE, title = "Select the Dataset To View")$res

#p = menu(c(ls()[sapply(mget(ls(), .GlobalEnv), is.data.frame)]), graphics = T,title = "Select Below One")


if(de == "data"){
  data = linear_one(data)
  #source("Home_Page.R")
}else if(de == "train"){
  train = linear_two(train)
  #source("Home_Page.R")
}else if(de == "test"){
  test = loading(test)
  #source("Home_Page.R")
}
#################################
source("Home_Page.R")
#train = read.csv(file.choose())
#test = read.csv(file.choose())

#### practising
