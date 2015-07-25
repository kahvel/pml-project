library(doParallel)
registerDoParallel(cores=2)

library(caret)

pml_training = read.csv("C:\\Users\\HP\\Desktop\\pml-training.csv", na.strings=c("NA","#DIV/0!",""))
pml_testing = read.csv("C:\\Users\\HP\\Desktop\\pml-testing.csv", na.strings=c("NA","#DIV/0!",""))

drops = c("X", "raw_timestamp_part_1", "raw_timestamp_part_2", "cvtd_timestamp", "num_window")
pml_training = pml_training[,!(names(pml_training) %in% drops)]
pml_testing = pml_testing[,!(names(pml_training) %in% drops)]

zeroVar = nearZeroVar(pml_training)
pml_training = pml_training[-zeroVar]
pml_testing = pml_testing[-zeroVar]

na_sums = colSums(is.na(pml_training))
pml_training = pml_training[, na_sums == 0]
pml_testing = pml_testing[, na_sums == 0]

inTrain = createDataPartition(pml_training$classe, p = 0.6)[[1]]
training = pml_training[inTrain,]
validation = pml_training[-inTrain,]

model <- train(training$classe~., method="rf", preProcess="pca", data=training)



pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}





#drops <- c("user_name","classe","new_window","cvtd_timestamp")
#train_numeric = training[,!(names(training) %in% drops)]
#
#zeroVar <- nearZeroVar(pml_training)

nums <- sapply(training, is.numeric)
train_numeric = training[,nums]
train_other = training[,!nums]
valid_numeric = validation[,nums]
valid_other = validation[,!nums]

train_numeric$classe = training$classe
valid_numeric$classe = validation$classe

#preProc = preProcess(train_numeric, method="pca")
model <- train(train_numeric$classe~., method="rf", preProcess="pca", data=train_numeric)




