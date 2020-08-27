### Building Automation Model

setwd("/home/vinayak/sudhakar/R/automation_model")
#setwd(dlgDir(default = getwd())$res)
######### packages are used #####################################
source("load_packages.R")
##################################################################
Home <- function()
  {
    z = menu(c("Load Dataset","View Dataset", "Rename or Remove Columns","Treating Missing Values",
               "Treating Categorical Variables","Build Models",
               "Plot the Data","Quit"),graphics = T, title="Select What do you want to do")
    if(z == 1) {source("Load_dataset.R")} 
    #return(data)
    #data1 = as.data.frame(return(data))}
    else if(z == 2){source("View_Dataset.R")}
    else if(z == 3){source("Modify_Columns.R")}
    else if(z == 4){source("Treat_missing_data.R")}
    else if(z == 5){source("Treat_categorical_data.R")}
    else if(z == 6){source("Build_models.R")}
    else if(z == 7){source("Plot.R")}
    else if(z == 8){quit()}
    else if(z != 1 || 2 || 3 || 4 || 5 || 6 || 7){ print("You Enter Wrong Option , You Redirect to Home Page",source("Home_Page.R"))}
    return(data)
}
Home()








