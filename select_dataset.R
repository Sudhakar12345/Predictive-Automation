#data = read.csv(file.choose())
#str(data)
#summary(data$Age)
dfs <- ls()[sapply(mget(ls(), .GlobalEnv), is.data.frame)]

ch = dlgList(dfs, multiple = F,title= "Choose the dataset to view")$res
menu(dfs,graphics = T)

View(menu(dfs,graphics = T))
############ Rough work
#ls(e)
