#categorical = colnames(data)[is.factor(data) == T]
#data = read.csv("/home/vinayak/Downloads/test.csv")
#colnames(data)[(is.character(data$Name) == TRUE)]
#str(train)
category = function(d){
  dd = menu(c("Encoding Method","Bag of Words Model"),graphics = T,title = "Select the method you want to Apply")
  if(dd == 1){
    ll = colnames(d)[sapply(d, is.factor)]
    #length(ll)
    e = menu(c("Default Apply for all Factors","Selected Columns"),graphics = T,title = "Choose Apply Method")
    if(e == 1){
      for (i in 1:length(ll)) {
        d[,ll[i]] = as.numeric(d[,ll[i]])
      }
      msgBox <- tkmessageBox(title = "Categorical Or Factor Columns",
                             message = "Successfully All Categorical Variables are converted into Numeric", icon = "info", type = "ok")
    #  return(d)
    } else {
      mm =  dlgList(c(colnames(d)[sapply(d, is.factor)]), multiple = TRUE)$res
      for (i in 1:length(mm)){
        d[,mm[i]] = as.numeric(d[,mm[i]])
        #return(data)
      }
      msgBox <- tkmessageBox(title = "Categorical Or Factor Columns",
                             message = "Successfully Selected Categorical Variables are converted into Numeric", icon = "info", type = "ok")
    }
  }
  else if(dd == 2){
    msgBox <- tkmessageBox(title = "Categorical Or Factor Columns",
                           message = "Bag of words Model Under Construction", icon = "info", type = "ok")
  }
  return(d)
}
#data = test
#aa = colnames(train)[sapply(train, is.factor)]
#if(length(aa) > 0){
#  train = category(train)
#} else{
#  msgBox <- tkmessageBox(title = "Categorical Or Factor Columns",
#                         message = "Categorical Or Factor Columns Not Available in Your Dataset", icon = "info", type = "ok")
#}

########### Modified Code
dfs <- ls()[sapply(mget(ls(), .GlobalEnv), is.data.frame)]


mm = dlgList(c(dfs), multiple = FALSE, title = "Select Your Dataset")$res
if(mm == "data"){
  aa = colnames(data)[sapply(data, is.factor)]
  if(length(aa) > 0){
    data = category(data)
  } else{
    msgBox <- tkmessageBox(title = "Categorical Or Factor Columns",
                           message = "Categorical Or Factor Columns Not Available in Your Dataset", icon = "info", type = "ok")
  }
} else if(mm == "test"){
  aa = colnames(test)[sapply(test, is.factor)]
  if(length(aa) > 0){
    test = category(test)
  } else{
    msgBox <- tkmessageBox(title = "Categorical Or Factor Columns",
                           message = "Categorical Or Factor Columns Not Available in Your Dataset", icon = "info", type = "ok")
  }
} else if(mm == "train"){
  aa = colnames(train)[sapply(train, is.factor)]
  if(length(aa) > 0){
    train = category(train)
  } else{
    msgBox <- tkmessageBox(title = "Categorical Or Factor Columns",
                           message = "Categorical Or Factor Columns Not Available in Your Dataset", icon = "info", type = "ok")
  }
}


# #mm = menu(dfs,graphics = T,"Select Which Dataset to fill Missig Data")
# if(length(dfs) == 1){
#   aa = colnames(data)[sapply(data, is.factor)]
#   if(length(aa) > 0){
#     data = category(data)
#   } else{
#     msgBox <- tkmessageBox(title = "Categorical Or Factor Columns",
#                            message = "Categorical Or Factor Columns Not Available in Your Dataset", icon = "info", type = "ok")
#   }
#   
# } else if(length(dfs) == 2) {
#   mm = menu(dfs,graphics = T,"Select Which Dataset to fill Missig Data")
#   if(mm == 1){
#     aa = colnames(test)[sapply(test, is.factor)]
#     if(length(aa) > 0){
#       test = category(test)
#     } else{
#       msgBox <- tkmessageBox(title = "Categorical Or Factor Columns",
#                              message = "Categorical Or Factor Columns Not Available in Your Dataset", icon = "info", type = "ok")
#     }
#     
#   } else if(mm == 2){
#     aa = colnames(train)[sapply(train, is.factor)]
#     if(length(aa) > 0){
#       train = category(train)
#     } else{
#       msgBox <- tkmessageBox(title = "Categorical Or Factor Columns",
#                              message = "Categorical Or Factor Columns Not Available in Your Dataset", icon = "info", type = "ok")
#     }
#     
#   }
# } else if(length(dfs) == 3) {
#   mm = menu(dfs,graphics = T,"Select Which Dataset to fill Missig Data")
#   if(mm == 1){
#     aa = colnames(data)[sapply(data, is.factor)]
#     if(length(aa) > 0){
#       data = category(data)
#     } else{
#       msgBox <- tkmessageBox(title = "Categorical Or Factor Columns",
#                              message = "Categorical Or Factor Columns Not Available in Your Dataset", icon = "info", type = "ok")
#     }
#   } else if(mm == 2){
#     aa = colnames(test)[sapply(test, is.factor)]
#     if(length(aa) > 0){
#       test = category(test)
#     } else{
#       msgBox <- tkmessageBox(title = "Categorical Or Factor Columns",
#                              message = "Categorical Or Factor Columns Not Available in Your Dataset", icon = "info", type = "ok")
#     }
#   } else if(mm == 3){
#     aa = colnames(train)[sapply(train, is.factor)]
#     if(length(aa) > 0){
#       train = category(train)
#     } else{
#       msgBox <- tkmessageBox(title = "Categorical Or Factor Columns",
#                              message = "Categorical Or Factor Columns Not Available in Your Dataset", icon = "info", type = "ok")
#     }
#   }
# }
# ########################################################
source("Home_Page.R")
