library("flexclust")
#### Reference : http://dni-institute.in/blogs/k-means-clustering-algorithm-explained/
################ clustering ###########
library(NbClust)
library(ggplot2)
library(tcltk)
library(svDialogs)
clustering = function(a){
  #a = print(de, quote=FALSE)
  no_of_cluster =  dlgInput("Enter the number of clusters to classify")$res
  ind = dlgList(c(colnames(a)), multiple = TRUE, title = "Select the Variables to cluster")$res
  b = select_if(a[,c(ind)], is.numeric)
  if(ncol(a[,c(ind)]) != ncol(b)){
    msgBox <- tkmessageBox(title = "Error While Running the Model",message = "Catagorical Variables are selected", icon = "info", type = "ok")
  } else {
  datasets = a[,c(ind)]
  i_kmeans = kmeans(datasets,no_of_cluster)
  a$predict_cluster = i_kmeans$cluster
  write.csv(a,"cluster_data.csv")
  msgBox <- tkmessageBox(title = "K-Means Model",message = "Successesfully Run K-means Model", icon = "info", type = "ok")
  }
}

# data1 = read.csv("/home/vinayak/Downloads/Fwd%3a_Data_Analysis_assignment/cluster_data.csv")
# str(data1)
# library(ade4)
# data1$predict_cluster = as.factor(data1$predict_cluster)
# s.class(myData,fac=kmeansRes, add.plot=TRUE, col=rainbow(nlevels(kmeansRes)))
# 
# s.class(data1,fac=data1$predict_cluster, add.plot=TRUE, col=rainbow(nlevels(data1$predict_cluster)))
# 
# library(clusplus)
# #sk2   <- silhouette(data1$default, data1$predict_cluster)
# library(cluster)
# clusplot(data1$default, data1$predict_cluster, main='2D representation of the Cluster solution',color=TRUE, shade=TRUE,
#          labels=2, lines=0)
# data1$predict_cluster = as.numeric(data1$predict_cluster)
# library(ggfortify)
# autoplot(kmeans(data[,c(2,3,4,5,6)],3), data = data)
# #autoplot(kmeans(USArrests, 3), data = USArrests, label = TRUE, label.size = 3)
# autoplot(kmeans(data[,c(2,3,4,5,6)],2), data = data, label = TRUE, label.size = 3)
# 
# library(cluster)
# names(data)
# autoplot(clara(data[,-25], 4))
# autoplot(fanny(data[,-25], 2), frame = TRUE)
# autoplot(pam(data[,c(2)], 3), frame = TRUE, frame.type = 'norm')

#getwd()
# str(iris)
# is.numeric(iris$Sepal.Length)
# sum(apply(data,2,function(x){is.numeric(x)}) == "TRUE")
# # iris$clster = i_kmeans$cluster 
# table(iris$Species,iris$clster)
# 
# iris$Sepal.Length[10] = "4.0"
#################### Calling function


library("dplyr")

#data = read.csv(file.choose())
#dfs <- ls()[sapply(mget(ls(), .GlobalEnv), is.data.frame)]

ll = menu(c("Data","Train","Test"), graphics = TRUE, title = "Please Select")
if(ll == 1){
  clustering(data)
} else if(ll == 2){
  clustering(train)
} else if(ll == 3){
  clustering(test)
}
source("Home_Page.R")
#data = read.csv(file.choose())
# a = nam
# colnames(de , do.NULL = F)
# nam = print(de, quote=FALSE)
# nrow(de, quote=FALSE)
# nrow(cat(de[1], "\n"))
# a = cat(de[1], "\n")
# colnames(noquote(de))
# colnames(data)
# is.data.frame(noquote(de))
# 
# cat(de[1], "\n")
# clustering(print(de, quote=FALSE))
# ll = menu(c("Data","Train Dataset","Test Dataset"), graphics = TRUE, title = "Please Select the Dataset")
# if(ll == 1){
#   svm_one(data)
# } else{
# svm_two(train, test)

######################################
# library(sparklyr)
# sc <- spark_connect(master = "local")
# library(dplyr)
# iris_tbl <- copy_to(sc, iris)
# flights_tbl <- copy_to(sc, nycflights13::flights, "flights")
# batting_tbl <- copy_to(sc, Lahman::Batting, "batting")
# flights_tbl
# spark_read_csv(sc = sc,path = "/home/vinayak/Desktop/toKaran/Churn_prediction.csv",name = "Churn", memory = TRUE)
# 
