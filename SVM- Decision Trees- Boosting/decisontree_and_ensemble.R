require(rpart)
require(rpart.plot)
require(caret)
require(adabag)

build_tree<-function(Train_1,Test_1,cp){
  #decision trees
  dt_1<-rpart(Y~.,data=Train_1,method="class",control = rpart.control(cp = cp, maxsurrogate=0))
  #stats
  #printcp(dt_1)
  #summary(dt_1)
  print(paste("number of leaves in training data:",sum(dt_1$frame$var=="<leaf>")))
  cat("\n")
  
  #confusion marix
  y_pred_train<-predict(dt_1,Train_1[, -which(names(Train_1) == "Y")],type="class")
  y_actual_train<-as.factor(unlist(Train_1['Y']))
  conf <- table(y_actual_train,y_pred_train)
  rownames(conf) <- paste("Actual", rownames(conf), sep = ":")
  colnames(conf) <- paste("Pred", colnames(conf), sep = ":")
  print("training data")
  print("confusion matrix")
  print(conf)
  print(paste("accuracy train:",accuracy<-round(sum(diag(conf))/length(y_actual_train),2)))
  
  cat("\n")
  y_pred_test<-predict(dt_1,Test_1[, -which(names(Test_1) == "Y")],type="class")
  y_actual_test<-as.factor(unlist(Test_1['Y']))
  conf <- table(y_actual_test,y_pred_test)
  rownames(conf) <- paste("Actual", rownames(conf), sep = ":")
  colnames(conf) <- paste("Pred", colnames(conf), sep = ":")
  print("test data")
  print("confusion matrix")
  print(conf)
  print(paste("accuracy test:",accuracy<-round(sum(diag(conf))/length(y_actual_test),2)))
  return(dt_1)
}


prune_cross_validation<-function(Train_1,cp_range,cv_num){
  folds <- lds <- cut(seq(1,nrow(Train_1)),breaks=cv_num,labels=FALSE)
  acc_cp<-NULL
  nodes_cp<-NULL
  for(j in cp_range){
    acc<-NULL
    nodes<-NULL
    for(i in 1:3){
      #print(i)
      Indexes <- which(folds==i,arr.ind=TRUE)
      trainData <- Train_1[-Indexes, ]
      valData <- Train_1[Indexes, ]
      #build the tree
      dt_1<-rpart(Y~.,data=trainData,method="class",control = rpart.control(cp = 0.0001, maxsurrogate=0))
      #prune it
      dt_1_pruned <- prune(dt_1, cp = j)
      #c(nodes,print(paste("number of leaves:",sum(dt_1_pruned$frame$var=="<leaf>"))))
      nodes<-c(nodes,sum(dt_1_pruned$frame$var=="<leaf>"))
      #hold out data
      y_pred_val<-predict(dt_1_pruned,valData[, -which(names(valData) == "Y")],type="class")
      y_actual_val<-as.factor(unlist(valData['Y']))
      conf <- table(y_actual_val,y_pred_val)
      accuracy<-round(sum(diag(conf))/length(y_actual_val),2)
      acc<-c(acc,accuracy)
    }
    acc<-mean(acc)*100
    nodes<-mean(nodes)
    acc_cp<-c(acc_cp,acc)
    nodes_cp<-c(nodes_cp,nodes)
  }
  nodes_cp<-as.integer(nodes_cp)
  print("cross validation results")
  #print("number of nodes after pruning using training data")
  #print(nodes_cp)
  print("cross valdation accuracy")
  print(acc_cp)
  cross<-data.frame(nodes=nodes_cp,acc=acc_cp)
  return(cross)
}


prune_test<-function(dt_1,Test_1,cp_range){
  nodes_test<-NULL
  acc_test<-NULL
  for(i in cp_range){
    dt_1_pruned <- prune(dt_1, cp = i)
    nodes_test<-c(nodes_test,sum(dt_1_pruned$frame$var=="<leaf>"))
    #test data
    y_pred_val<-predict(dt_1_pruned,Test_1[, -which(names(Test_1) == "Y")],type="class")
    y_actual_val<-as.factor(unlist(Test_1['Y']))
    conf <- table(y_actual_val,y_pred_val)
    accuracy<-round(sum(diag(conf))/length(y_actual_val),2)*100
    acc_test<-c(acc_test,accuracy) 
  }
  print("Test data results")
  print("number of nodes after pruning using training data")
  print(nodes_test)
  print("Test data accuracy")
  print(acc_test)
  cross<-data.frame(nodes=nodes_test,acc=acc_test)
  return(cross)
  
}

#plot
plot_dt<-function(dt_1,cp_pruned,title){
  only_count <- function(x, labs, digits, varlen)
  {
    paste(x$frame$n)
  }
  dt_1_pruned <- prune(dt_1, cp = cp_pruned) #0.001
  boxcols <- c("pink", "palegreen3")[dt_1_pruned$frame$yval]
  #print(boxcols)
  par(xpd=TRUE)
  prp(dt_1_pruned, faclen = 0, cex = 0.5, node.fun=only_count, box.col = boxcols,main=title)
  return(dt_1_pruned)
}

cv_boosting <-function(Train_d,number_of_learners,cv_num)
{
  folds <- cut(seq(1,nrow(Train_d)),breaks=cv_num,labels=FALSE)
acc_cv<-NULL
for(j in number_of_learners){
  print(paste("number of learners:",j))
  acc<-NULL
  for(i in 1:cv_num){
    Indexes <- which(folds==i,arr.ind=TRUE)
    trainData <- Train_d[-Indexes, ]
    valData <- Train_d[Indexes, ]
    valData2<-valData[, -which(names(valData) == "Y")]
    adaboost<-boosting(Y~., data=trainData,mfinal = j,control = rpart.control(cp = 0.0001, maxsurrogate=0))
    #predict
    Val_pred<-predict.boosting(adaboost,newdata=valData2)
    Val_table<-table(as.factor(as.integer(Val_pred$class)),as.factor(valData$Y))
    print(paste("Validation:",round(sum(diag(Val_table))/length(valData$Y),2)*100))
    acc<-c(acc,round(sum(diag(Val_table))/length(valData$Y),2)*100)
  }
  acc<-mean(acc)
  acc_cv<-c(acc_cv,acc)
  print(acc)
}
print(acc_cv)
}

boosting_testandtrain <-function(cp_range,Train_d,Test_d,mfinal_val) {

  Train_pred_data<-Train_d[, -which(names(Train_d) == "Y")]
  Test_pred_data<-Test_d[, -which(names(Test_d) == "Y")]
  test_Acc<-NULL
  train_ACC<-NULL
  
  for(i in cp_range){
    
    print(paste("cp:",i))
    adaboost<-boosting(Y~., data=Train_d,mfinal=mfinal_val,control = rpart.control(cp =0.001, maxsurrogate=0))
    print("all good")
    Train_pred<-predict.boosting(adaboost,newdata=Train_pred_data)
    Train_table<-table(as.factor(as.integer(Train_pred$class)),as.factor(Train_d$Y))
    print(paste("Training:",round(sum(diag(Train_table))/length(Train_d$Y),2)*100))
    train_ACC<-c(train_ACC,round(sum(diag(Train_table))/length(Train_d$Y),2)*100)
    
    Test_pred<-predict.boosting(adaboost,newdata=Test_pred_data)
    Test_table<-table(as.factor(as.integer(Test_pred$class)),as.factor(Test_d$Y))
    print(paste("Test:",round(sum(diag(Test_table))/length(Test_d$Y),2)*100))
    test_Acc<-c(test_Acc,round(sum(diag(Test_table))/length(Test_d$Y),2)*100)
    
  }
  
  print(train_ACC)
  print(test_Acc)
}
#################################
#decision tree
################################


###########################################
#dataset1
#########################################

#readind data
Train_1<-read.csv("OnlinePopulatity_train.csv")
Test_1<-read.csv("OnlinePopulatity_test.csv")

#removing the index varaible
Train_1['X']<-NULL
Test_1['X']<-NULL

decision_tree_1<-build_tree(Train_1,Test_1,0.0001)
cp_range<-c(0.05,0.01,0.005,0.001,0.0005,0.0001)
cv_1<-prune_cross_validation(Train_1,cp_range,3)
test_acc<-prune_test(decision_tree_1,Test_1,cp_range)

plot(test_acc$nodes,cv_1$acc, type="b", pch=19, col="red",main="Number of nodes after pruning using training data vs accuracy",xlab="Number of nodes in training data",ylab="Accuracy")
lines(test_acc$nodes,test_acc$acc, pch=18, col="blue", type="b", lty=2)
legend(530,64,legend=c("cv accuacy","test accuacy"),col=c("red", "blue"), lty=1:2, cex=0.8)

plot_dt(decision_tree_1,0.001,"pink for low shares and green for high shares")



###########################################
#dataset2
#########################################

#readind data
Train_2<-read.csv("Bank_train.csv")
Test_2<-read.csv("Bank_test.csv")

#removing the index varaible
Train_2['X']<-NULL
Test_2['X']<-NULL


decision_tree_2<-build_tree(Train_2,Test_2,0.0001)
cp_range<-c(0.05,0.01,0.005,0.001,0.0005,0.0001)
cv_2<-prune_cross_validation(Train_2,cp_range,3)
test_acc2<-prune_test(decision_tree_2,Test_2,cp_range)

plot(test_acc2$nodes,cv_2$acc, type="b", pch=19, col="red",main="Number of nodes after pruning using training data vs accuracy",xlab="Number of nodes in training data",ylab="Accuracy")
lines(test_acc2$nodes,test_acc2$acc, pch=18, col="blue", type="b", lty=2)
legend(149,91.35,legend=c("cv accuacy","test accuacy"),col=c("red", "blue"), lty=1:2, cex=0.8)

plot_dt(decision_tree_2,0.01,"pink for no subscribed and green for subscribed")


##############################
#ensemble
##############################


###########################################
#dataset1
#########################################

Train_d<-Train_1
Train_d$Y<-factor(Train_d$Y)
Test_d<-Test_1
Test_d$Y<-factor(Test_d$Y)

number_of_learners<-c(5,10,25,50,75,100,150,200,250)
cv_num<-3
cv_boosting(Train_d,number_of_learners,cv_num)

cp_range<-c(0.01,0.005,0.001,0.0005,0.0001)
mfinal_val<-200
boosting_testandtrain(cp_range,Train_d,Test_d,mfinal_val)

###########################################
#dataset2
#########################################

Train_d<-Train_2
Train_d$Y<-factor(Train_d$Y)
Test_d<-Test_2
Test_d$Y<-factor(Test_d$Y)

number_of_learners<-c(5,10,25,50,75,100,150,200,250)
cv_num<-3
cv_boosting(Train_d,number_of_learners,cv_num)

cp_range<-c(0.01,0.005,0.001,0.0005,0.0001)
mfinal_val<-25
boosting_testandtrain(cp_range,Train_d,Test_d,mfinal_val)



