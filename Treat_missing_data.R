### Treating Missing Valuesinstall.packages()
#data = read.csv("/home/vinayak/test.csv")
library(tcltk)
#data = read.csv("/home/vinayak/Downloads/test.csv")
missing_values = function(a){
  l = colnames(a)[colSums(is.na(a)) > 0]
  m = menu(colnames(a)[colSums(is.na(a)) > 0],graphics = T,title = "Following Columns having missing values.Select the Column number to Fill Missing Values")
  n = menu(c("Mean","Median","Mode","Using Predictive Model"),graphics = T,title = "Which Method to use to fill missing values")
  if(n == 1){
    #l[1] = prints the column name in below
    a[,l[m]] <- as.numeric(a[,l[m]]) #first convert each column into numeric if it is from factor
    a[,l[m]][is.na(a[,l[m]])] = mean(a[,l[m]], na.rm=TRUE)
  }
  else if(n == 2){
    a[,l[m]] <- as.numeric(a[,l[m]]) #first convert each column into numeric if it is from factor
    a[,l[m]][is.na(a[,l[m]])] =median(a[,l[m]], na.rm=TRUE)
  }
  else if(n == 3){
    
        Mode <- function(x) {
            ux <- unique(x)
            ux[which.max(tabulate(match(x, ux)))]
            }
    o = Mode(na.omit(a[,l[m]]))
    a[,l[m]][is.na(a[,l[m]])] = o
  }
  else if(n == 4){
    msgBox <- tkmessageBox(title = "Predictive Model",
                           message = "It's Under Construction... Please Wait", icon = "info", type = "ok")
    #data <- data[!is.na(data$Embarked),]
    #rownames(data) <- NULL
    }
  return(a)
  #print(m)
}


#if(sum(is.na(data)) > 0){
#  data = missing_values(data)
#} 


dfs <- ls()[sapply(mget(ls(), .GlobalEnv), is.data.frame)]
#mm = menu(dfs,graphics = T,"Select Which Dataset to fill Missig Data")
ww = dlgList(c(dfs), multiple = FALSE, title = "Select Your Dataset")$res
if(ww == "data"){
  if(sum(is.na(data)) > 0){
    data = missing_values(data)
  } else{
    msgBox <- tkmessageBox(title = "Missing Data",
                           message = "No Missing Value in Your Dataset ", icon = "info", type = "ok")
  }
} else if(ww == "test"){
  if(sum(is.na(test)) > 0){
    test = missing_values(test)
  } else{
    msgBox <- tkmessageBox(title = "Missing Data",
                           message = "No Missing Value in Your Dataset ", icon = "info", type = "ok")
  }
} else if(ww == "train"){
  if(sum(is.na(train)) > 0){
    train = missing_values(train)
  } else{
    msgBox <- tkmessageBox(title = "Missing Data",
                           message = "No Missing Value in Your Dataset ", icon = "info", type = "ok")
  }
}
# 
# if(length(dfs) == 1){
#   if(sum(is.na(data)) > 0){
#     data = missing_values(data)
#   } else{
#     msgBox <- tkmessageBox(title = "Missing Data",
#                            message = "No Missing Value in Your Dataset ", icon = "info", type = "ok")
#   }
# } else if(length(dfs) == 2) {
#   mm = menu(dfs,graphics = T,"Select Which Dataset to fill Missig Data")
#   if(mm == 1){
#     if(sum(is.na(test)) > 0){
#       test = missing_values(test)
#     } else {
#       msgBox <- tkmessageBox(title = "Missing Data",
#                              message = "No Missing Value in Your Test Dataset ", icon = "info", type = "ok")
#     }
#   }else if(mm == 2){
#     if(sum(is.na(train)) > 0){
#       train = missing_values(train)
#     } else{
#       msgBox <- tkmessageBox(title = "Missing Data",
#                              message = "No Missing Value in Your Train Dataset ", icon = "info", type = "ok")
#     }
#   }
#   }else if(length(dfs) == 3) {
#     mm = menu(dfs,graphics = T,"Select Which Dataset to fill Missig Data")
#     if(mm == 1){
#       if(sum(is.na(data)) > 0){
#         data = missing_values(data)
#       } else{
#         msgBox <- tkmessageBox(title = "Missing Data",
#                                message = "No Missing Value in Your Dataset ", icon = "info", type = "ok")
#       }
#     }else if(mm == 2){
#       if(sum(is.na(test)) > 0){
#         test = missing_values(test)
#       } else{
#         msgBox <- tkmessageBox(title = "Missing Data",
#                                message = "No Missing Value in Your Test Dataset ", icon = "info", type = "ok")
#       }
#     }else if(mm == 3){
#       if(sum(is.na(train)) > 0){
#         train = missing_values(train)
#       } else{
#         msgBox <- tkmessageBox(title = "Missing Data",
#                                message = "No Missing Value in Your Train Dataset ", icon = "info", type = "ok")
#       }
#     }
#   }
# 

source("Home_Page.R")

#sum(is.na(train))

#source("Home_Page.R")
#a = colnames(data)[colSums(is.na(data)) > 0]
#a[2]
#data[,a[2]]

#data[,l[2]] <- as.numeric(data[,l[2]]) #first convert each column into numeric if it is from factor
#data[,l[2]][is.na(data[,l[2]])] = mean(data[,l[2]], na.rm=TRUE)
# table(is.na(data[,l[2]]))


