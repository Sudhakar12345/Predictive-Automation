### Modify Columns
#setwd("/home/vinayak/sudhakar/automation_model")
library(svDialogs)

library(xlsx)
#source("Home_Page.R")
#data = read.csv("/home/vinayak/Downloads/test.csv")
Modify = function(a){
  #View(a)
  b = menu(c("Rename Columns","Remove Columns","Merge Columns"),graphics = T,title = "Select The Option")
  if (b == 1 ){
      c = menu(names(a),graphics = T,title = "Select the Column to Rename")
      text = dlgInput("Enter the new name of the column")$res
      names(a)[c] <-  paste(text)
      return(a)
      library(tcltk)
      msgBox <- tkmessageBox(title = "Automation Project",message = "Successfully Renamed the Column", icon = "info", type = "ok")
      
  } 
  else if(b == 2){
    ind = dlgList(c(colnames(a)), multiple = TRUE, title = "Select the Columns to Remove")$res
    if(length(ind) > 0){
      a = a[ , -which(names(a) %in% c(ind))]
      msgBox <- tkmessageBox(title = "Automation Project",message = "Successfully Delete the Column", icon = "info", type = "ok")
    } else{
      msgBox <- tkmessageBox(title = "Automation Project",message = "Please Select Any one column, Now You Redirect to Home Page", icon = "info", type = "ok")
    }
  }
  else if(b == 3){
    d = menu(names(a),graphics = T,title = "Select the First Column to Merge")
    e = menu(names(a),graphics = T,title = "Select the Second Column to Merge")
    text = dlgInput("Enter the new name of the column")$res
    a$text = paste(a[,d], a[,e], sep = " ")
    colnames(a)[colnames(a)=="text"] <- text
    library(tcltk)
    msgBox <- tkmessageBox(title = "Automation Project",message = "Successfully Merged Two Column", icon = "info", type = "ok")
    #View(data)
    }
  return(a)
}

dfs <- ls()[sapply(mget(ls(), .GlobalEnv), is.data.frame)]


dd = dlgList(c(dfs), multiple = FALSE, title = "Select Your Dataset")$res
if(dd == "data"){
  data = Modify(data)
} else if(dd == "test"){
  test = Modify(test)
} else if(dd == "train"){
  train = Modify(train)
}

#dd == "test"
source("Home_Page.R")
