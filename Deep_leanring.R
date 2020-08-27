path = "/home/vinayak/sudhakar/R/automation_model"
setwd(path)

#load libraries
library(data.table)
library(mlr)
#set variable names
setcol <- c("age",
            "workclass",
            "fnlwgt",
            "education",
            "education-num",
            "marital-status",
            "occupation",
            "relationship",
            "race",
            "sex",
            "capital-gain",
            "capital-loss",
            "hours-per-week",
            "native-country",
            "target")

#load data
#train <- read.table("adultdata.txt",header = F,sep = ",",col.names = setcol,na.strings = c(" ?"),stringsAsFactors = F)
#test <- read.table("adulttest.txt",header = F,sep = ",",col.names = setcol,skip = 1, na.strings = c(" ?"),stringsAsFactors = F)
train = read.table("https://raw.githubusercontent.com/guillaume-chevalier/predict-if-salary-is-over-50k-with-Keras/master/data/adult.data.txt",header = F,sep = ",",col.names = setcol,na.strings = c(" ?"))
test <-  read.table("https://raw.githubusercontent.com/guillaume-chevalier/predict-if-salary-is-over-50k-with-Keras/master/data/adult.test.txt",header = F,sep = ",",col.names = setcol,na.strings = c(" ?"))

## covert list or data.frame into data.table
setDT(train)
setDT(test)

#Data Sanity
dim(train) #32561 X 15
dim(test) #16281 X 15
str(train)
str(test)

#check missing values
table(is.na(train))
sapply(train, function(x) sum(is.na(x))/length(x))*100
table(is.na(test))
sapply(test, function(x) sum(is.na(x))/length(x))*100

#check target variable
#binary in nature check if data is imbalanced
train[,.N/nrow(train),target]
test[,.N/nrow(test),target]

#remove extra characters
test[,target := substr(target,start = 1,stop = nchar(target)-1)]

#remove leading whitespace
library(stringr)
char_col <- colnames(train)[sapply(test,is.character)]

for(i in char_col)
  set(train,j=i,value = str_trim(train[[i]],side = "left"))
?set
#set all character variables as factor
fact_col <- colnames(train)[sapply(train,is.character)]

for(i in fact_col)
  set(train,j=i,value = factor(train[[i]]))

for(i in fact_col)
  set(test,j=i,value = factor(test[[i]]))
#impute missing values
imp1 <- impute(data = train,target = "target",classes = list(integer = imputeMedian(), factor = imputeMode()))
imp2 <- impute(data = test,target = "target",classes = list(integer = imputeMedian(), factor = imputeMode()))

train <- setDT(imp1$data)
test <- setDT(imp2$data)

#load the package
require(h2o)

#start h2o
localH2o <- h2o.init(nthreads = -1, max_mem_size = "2G")
??h20.init
#load data on H2o
trainh2o <- as.h2o(train)
testh2o <- as.h2o(test)

#set variables
y <- "target"
x <- setdiff(colnames(trainh2o),y)

#train the model - without hidden layer
deepmodel <- h2o.deeplearning(x = x
                              ,y = y
                              ,training_frame = trainh2o
                              ,standardize = T
                              ,model_id = "deep_model"
                              ,activation = "Rectifier"
                              ,epochs = 100
                              ,seed = 1
                              ,nfolds = 5
                              ,variable_importances = T)

#compute variable importance and performance
h2o.varimp_plot(deepmodel,num_of_features = 20)
h2o.performance(deepmodel,xval = T) #84.5 % CV accuracy

deepmodel <- h2o.deeplearning(x = x	 	 
                              ,y = y	 	 
                              ,training_frame = trainh2o	 	 
                              ,validation_frame = testh2o	 	 
                              ,standardize = T	 	 
                              ,model_id = "deep_model"	 	 
                              ,activation = "Rectifier"	 	 
                              ,epochs = 100	 	 
                              ,seed = 1	 	 
                              ,hidden = 5	 	 
                              ,variable_importances = T)	 	 
h2o.performance(deepmodel,valid = T) #85.6%

activation_opt <- c("Rectifier","RectifierWithDropout", "Maxout","MaxoutWithDropout")
hidden_opt <- list(c(10,10),c(20,15),c(50,50,50))
l1_opt <- c(0,1e-3,1e-5)
l2_opt <- c(0,1e-3,1e-5)

hyper_params <- list( activation=activation_opt,
                      hidden=hidden_opt,
                      l1=l1_opt,
                      l2=l2_opt )

#set search criteria
search_criteria <- list(strategy = "RandomDiscrete", max_models=10)

#train model
dl_grid <- h2o.grid("deeplearning"
                    ,grid_id = "deep_learn"
                    ,hyper_params = hyper_params
                    ,search_criteria = search_criteria
                    ,training_frame = trainh2o
                    ,x=x
                    ,y=y
                    ,nfolds = 5
                    ,epochs = 100)

#get best model
d_grid <- h2o.getGrid("deep_learn",sort_by = "accuracy")
best_dl_model <- h2o.getModel(d_grid@model_ids[[1]])
h2o.performance (best_dl_model,xval = T) #CV Accuracy - 84.7%

#Installation - Linux
#Press Ctrl + Alt + T and run the following command
sudo apt-get update
sudo apt-get -y install git
git clone https://github.com/dmlc/mxnet.git ~/mxnet --recursive
cd ~/mxnet/setup-utils
bash install-mxnet-ubuntu-r.sh

#load package
require(mxnet)

#convert target variables into numeric
train[,target := as.numeric(target)-1]
test[,target := as.numeric(target)-1]

#convert train data to matrix
train.x <- data.matrix(train[,-c("target"),with=F])
train.y <- train$target

#convert test data to matrix
test.x <- data.matrix(test[,-c("target"),with=F])
test.y <- test$target

#set seed to reproduce results
set.seed(1)

mlpmodel <- mx.mlp(data = train.x
                   ,label = train.y
                   ,hidden_node = 3 #one layer with 10 nodes
                   ,out_node = 2
                   ,out_activation = "softmax" #softmax return probability
                   ,num.round = 100 #number of iterations over training data
                   ,array.batch.size = 20 #after every batch weights will get updated
                   ,learning.rate = 0.03 #same as step size
                   ,eval.metric = mx.metric.accuracy
                   ,eval.data = list(data = test.x, label = test.y))


#create NN structure
data <- mx.symbol.Variable("data")
fc1 <- mx.symbol.FullyConnected(data, num_hidden=3) #3 neuron in one layer
lrm <- mx.symbol.SoftmaxOutput(fc1)


nnmodel <- mx.model.FeedForward.create(symbol = lrm
                                       ,X = train.x
                                       ,y = train.y
                                       ,ctx = mx.cpu()
                                       ,num.round = 100
                                       ,eval.metric = mx.metric.accuracy
                                       ,array.batch.size = 50
                                       ,learning.rate = 0.01)


#configure another network
data <- mx.symbol.Variable("data")
fc1 <- mx.symbol.FullyConnected(data, name = "fc1", num_hidden=10) #1st hidden layer
act1 <- mx.symbol.Activation(fc1, name = "sig", act_type="relu") 
fc2 <- mx.symbol.FullyConnected(act1, name = "fc2", num_hidden=2) #2nd hidden layer
out <- mx.symbol.SoftmaxOutput(fc2, name = "soft")

#train the network
dp_model <- mx.model.FeedForward.create(symbol = out
                                        ,X = train.x
                                        ,y = train.y
                                        ,ctx = mx.cpu()
                                        ,num.round = 100
                                        ,eval.metric = mx.metric.accuracy
                                        ,array.batch.size = 50
                                        ,learning.rate = 0.005)

#predict on test
pred_dp <- predict(dp_model,test.x)
str(pred_dp) #contains 2 rows and 16281 columns

#transpose the pred matrix
pred.val <- max.col(t(pred_dp))-1

