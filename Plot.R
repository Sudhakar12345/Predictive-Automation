### Plotting the dataset
plot_data = function(a){
  c = menu(c("Plot Using One Variable","Plot Using Two Variable","Plot Using All Variable"),graphics = T, title = "Plot Your Datasets, But it must be Numeric")
  library(ggplot2)
  library(tcltk)
  library(svDialogs)
  x11()
  #par(mfrow = c(3,3))
  if (c == 1 ){
    f = menu(names(a),graphics = T,title = "Select the Column to Plot")
    if(sum(is.na(data[,f]))> 0)
    {
      msgBox <- tkmessageBox(title = "Automation Project",message = "Missing Values are in this column, So can't Plot", icon = "info", type = "ok")
    }
    else{
      t = dlgInput("Type b for line-chart, h for histogram")$res
      plot(density(data[,f]), type = t)
    }
  }
  else if(c == 2){
    de = menu(names(a),graphics = T,title = "Select the First Column to Plot")
    ed = menu(names(a),graphics = T,title = "Select the Second Column to Plot")
    if(sum(is.na(data[,de]))> 0 || sum(is.na(data[,ed]))> 0)
    {
      msgBox <- tkmessageBox(title = "Automation Project",message = "Missing Values are in these columns, , So can't Plot", icon = "info", type = "ok")
    }
    else{
      m = menu(c("Line Graph","Bar Graph"),graphics = T, title = "Select Type of Plot")
      if(m == 1){
        ggplot(data, aes(x=data[,de], y=data[,ed]))+ geom_line(stat="identity", fill="lightblue", colour="orange") + geom_point()
      }
      else if(m == 2){
        ggplot(data, aes(x=data[,de], y=data[,ed]))+ geom_bar(stat="identity", fill="lightblue", colour="orange") + geom_point()
      }
    }
  }
  else if(c == 3){
    c = menu(names(a),graphics = T,title = "Select the Column number to Remove")
    data = data[,-c]
    
    msgBox <- tkmessageBox(title = "Automation Project",message = "Successfully Delete the Column", icon = "info", type = "ok")
  }
}

plot_data(data)
#source("Home_Page.R")
#msgBox <- tkmessageBox(title = "Ggplot Graphs",
#                       message = "It's Under Construction... Please Wait", icon = "info", type = "ok")
#source("Home_Page.R")

### Selecting Colors Manually
library(colourpicker)
if (interactive()) {
  cols <- colourPicker(1)
}
