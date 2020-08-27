## View datasets interactively
library(googleVis)

loading = function(a){
  msgBox <- tkmessageBox(title = "Select Dependent Variable",message = "Use UP and DOWN Arrow keys for going down or beside or up", icon = "info", type = "ok")
  utils::View(a)
}

 library(tcltk)
library(svDialogs)
de =  dlgList(c(ls()[sapply(mget(ls(), .GlobalEnv), is.data.frame)]), multiple = FALSE, title = "Select the Dataset To View")$res

#p = menu(c(ls()[sapply(mget(ls(), .GlobalEnv), is.data.frame)]), graphics = T,title = "Select Below One")
 
 #data = read.csv(file.choose())
 if(de == "data"){
   data = loading(data)
   #source("Home_Page.R")
 }else if(de == "train"){
   train = loading(train)
   #source("Home_Page.R")
 }else if(de == "test"){
   test = loading(test)
   #source("Home_Page.R")
 }
source("Home_Page.R")
 

 