### Reading dataset from different data sources
#setwd("/home/vinayak/sudhakar/automation_model")

read_data <- function()
{
  
  a = menu(c("CSV", "Excel","URL","Text","JSON","Home_Page"),graphics = T, title="What Type of dataset you Import")
  if(a == 1) { data = read.csv(file.choose(),header = T)
  #source("Home_Page.R")
  if(is.data.frame(data) == "TRUE"){
    library(tcltk)
    msgBox <- tkmessageBox(title = "Data Uploading",message = "Data Uploaded Successfully", icon = "info", type = "ok")
      }
  else{
    msgBox <- tkmessageBox(title = "Data Uploading",
                           message = "Data Not Uploaded, Please TryAgain", icon = "info", type = "ok")
  }
  }
  #return(data)
  #data1 = as.data.frame(return(data))}
  else if(a == 2){
    library(xlsx)
    data = read.xlsx(file.choose(),sheetIndex = 1)
    if(is.data.frame(data) == "TRUE"){
      
    library(tcltk)
    msgBox <- tkmessageBox(title = "Data Uploading",
                           message = "Data Uploaded Successfully", icon = "info", type = "ok")
    return(data)
    }
    
  else{
    msgBox <- tkmessageBox(title = "Data Uploading",
                           message = "Data Not Uploaded, Please TryAgain", icon = "info", type = "ok")
  }
  
  }
  else if(a == 3){ data = read.table(readline("Enter or Paste a link"))
  if(is.data.frame(data) == "TRUE"){
    library(tcltk)
    msgBox <- tkmessageBox(title = "Data Uploading",
                           message = "Data Uploaded Successfully", icon = "info", type = "ok")
    source("Home_Page.R")
  }
  else{
    msgBox <- tkmessageBox(title = "Data Uploading",
                           message = "Data Not Uploaded, Please TryAgain", icon = "info", type = "ok")
  }
  return(data)
  }
  else if(a == 4){ data = read.table(file.choose())
  source("Home_Page.R")
  }
  else if(a == 5){ data = fromJSON(file.choose())
  }
  else if(a==6){
    msgBox <- tkmessageBox(title = "Home Page",message = "You are redirecting Home Page", icon = "info", type = "ok")
  }
  else if(a != 1 || 2 || 3 || 4 || 5 || 6){ print("You Enter Wrong Option , You Redirect to Home Page",source("Home_Page.R"))}
  return(data)
}
read_data()

