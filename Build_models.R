#### building models
#setwd("/home/vinayak/sudhakar/automation_model")
build = function()
  {
  s = menu(c("Classification","Clustering","Regression","Forecasting or Time Series"), graphics = T,
           title = "Select One")
  if(s == 1){
    t = menu(c("Logistic Regression","Decision Tree","K Nearest Neighbour","Random Forest","Support Vector Machines",
               "Naive - Bayes","XGBoost"), graphics = T, title = "Choose the Classification model")
    if(t == 1){
      source("Logistic.R")
    }
    else if(t == 2){
      source("Decision_tree_classification.R")
    }
    else if(t == 3){
      source("knn.R")
    }
    else if(t == 4){
      source("Random_Forest_Classification.R")
    }
    else if(t == 5){
      source("svm_classification.R")
    }
    else if(t == 6){
      source("Naive_Bayes.R")
    }
    else if(t == 7){
      source("XGBoost_classification.R")
    }
  }
  if(s == 2){
    t = menu(c("K-Means","Affinity Propagation"), graphics = T, title = "Choose the Clustering model")
    if(t == 1){
      source("K_means.R")
    }
    else if(t == 2){
      source("Affinity_Propagation.R")
    }
  }
  if(s == 3){
    t = menu(c("Linear Regression","Decision_tree_prediction","Random Forest","Support Vector Machines"
               ,"XGBoost"), graphics = T, title = "Choose Regression model")
    if(t == 1){
      source("Linear.R")
    }
    else if(t == 2){
      source("Decision_tree_prediction.R")
    }
    else if(t == 3){
      source("Random_Forest_prediction.R")
    }
    else if(t == 4){
      source("svm_prediction.R")
    }
    else if(t == 5){
      source("XGBoost_prediction.R")
    }
  }
}

build()
