

EnsurePackage<-function(x)
{ # EnsurePackage(x) - Installs and loads a package
  # if necessary
  x <- as.character(x)
  if (!require(x, character.only=TRUE))
  {
    install.packages(pkgs=x,
                     repos="http://cran.r-project.org")
  }
  require(x, character.only=TRUE)
  
}

#Installs and loads all packages necessary

PrepareExam<-function(){
  
  EnsurePackage("glmnet")
  EnsurePackage("boot")
  EnsurePackage("MASS")
  EnsurePackage("ISLR")
  EnsurePackage("class")
  EnsurePackage("ElemStatLearn")

}

PrepareExam()



cv.knn<- function (dataY, dataX, kn=3, K=10, seed=123) {
  n <- nrow(dataX)
  set.seed(seed)
  library(class)
  
  f <- ceiling(n/K)
  s <- sample(rep(1:K, f), n)  
  dataX=scale(dataX)
  CV=NULL;PvsO=NULL
  
  for (i in 1:K) { 
    test.index <- seq_len(n)[(s == i)] #test data
    train.index <- seq_len(n)[(s != i)] #training data
    
    train.X <- dataX[train.index,]
    test.X <- dataX[test.index,]
    train.y <- dataY[train.index]
    test.y <- dataY[test.index]
    #predicted test set y
    knn.pred=knn(train.X, test.X, train.y, k=kn) 
    #observed - predicted on test data 
    error= mean(knn.pred!=test.y) 
    #error rates 
    CV=c(CV,mean(error))
    predvsobs=data.frame(knn.pred,test.y)
    PvsO=rbind(PvsO,predvsobs)
  } 
  
  #Output
  list(k = K,
       knn_error_rate = mean(CV), confusion=table(PvsO[,1],PvsO[,2]), seed=seed)
}


cv.lda<-
  function (data, model=Direction~Lag1+Lag2, yname="Direction", K=10, seed=123) {
    #model is lda model
    #yname: name of response variable
    #K: number of partition for cv #K=10
    #seed: random seed number
    
    n <- nrow(data)
    set.seed(seed)
    datay=data[,yname] #response variable
    library(MASS)
    #partition the data into K subsets
    f <- ceiling(n/K)
    s <- sample(rep(1:K, f), n)  
    #generate indices 1:10 and sample n of them  
    # K fold cross-validated error
    
    CV=NULL
    
    for (i in 1:K) { #i=1
      test.index <- seq_len(n)[(s == i)] #test data
      train.index <- seq_len(n)[(s != i)] #training data
      
      #model with training data
      lda.fit=lda(model, data=data[train.index,])
      #observed test set y
      lda.y <- data[test.index, yname]
      #predicted test set y
      lda.predy=predict(lda.fit, data[test.index,])$class
      
      #observed - predicted on test data
      error= mean(lda.y!=lda.predy)
      #error rates 
      CV=c(CV,error)
    }
    
    #Output
    list(call = model, K = K, 
         lda_error_rate = mean(CV), seed = seed)  
    
  }


cv.qda<-
  function (data, model=Direction~Lag1+Lag2, yname="Direction", K=10, seed=123) {
    #model is qda model
    #yname: name of response variable
    #K: number of partition for cv #K=10
    #seed: random seed number
    
    n <- nrow(data)
    set.seed(seed)
    datay=data[,yname] #response variable
    library(MASS)
    #partition the data into K subsets
    f <- ceiling(n/K)
    s <- sample(rep(1:K, f), n)  
    #generate indices 1:10 and sample n of them  
    # K fold cross-validated error
    
    CV=NULL
    
    for (i in 1:K) { #i=1
      test.index <- seq_len(n)[(s == i)] #test data
      train.index <- seq_len(n)[(s != i)] #training data
      
      #model with training data
      qda.fit=qda(model, data=data[train.index,])
      #observed test set y
      qda.y <- data[test.index, yname]
      #predicted test set y
      qda.predy=predict(qda.fit, data[test.index,])$class
      
      #observed - predicted on test data
      error= mean(qda.y!=qda.predy)
      #error rates 
      CV=c(CV,error)
    }
    
    #Output
    list(call = model, K = K, 
         qda_error_rate = mean(CV), seed = seed)  
    
  }




mycv.glm<-
  function (data, glmfit, K=10, seed=123) {
    #glmfit is glm fit with whole data
    #this function is to get the Cross-validated mean square error for regression
    #output R2 and MSE
    n <- nrow(data)
    set.seed(seed) #K=10
    
    #partition the data into K subsets
    f <- ceiling(n/K)
    s <- sample(rep(1:K, f), n)  
    #generate indices 1:10 and sample n of them  
    # K fold cross-validated mean squared error
    
    CV=NULL; Rcv=NULL
    
    for (i in 1:K) { #i=1
      test.index <- seq_len(n)[(s == i)] #test data
      train.index <- seq_len(n)[(s != i)] #training data
      
      #model with training data
      glm.fit=glm(glmfit$call, data=data[train.index,])
      
      #observed test set y
      glm.y <- glmfit$y[test.index]
      #observed - predicted on test data
      error= glm.y - predict(glm.fit, newdata=data[test.index,])
      #mean squred error
      MSE <- mean(error^2)
      CV=c(CV,MSE)
      R=1-sum(error^2)/sum((glm.y-mean(glm.y))^2)
      Rcv=c(Rcv,R)
    }
    
    #Output
    list(call = glmfit$call, K = K, 
         MSE = mean(CV),R2=mean(Rcv), 
         seed = seed)  
    
  }



mycv.stepAIC<-
  function (data, glmfit, K=10, seed=123) {
    #lmfit is lm fit with whole data
    #this function is to get the Cross-validated mean square error for regression
    #output R2 and MSE
    library(MASS)
    n <- nrow(data)
    set.seed(seed) #K=10
    
    #partition the data into K subsets
    f <- ceiling(n/K)
    s <- sample(rep(1:K, f), n)  
    
    #generate indices 1:10 and sample n of them  
    # K fold cross-validated mean squared error
    
    CV=NULL; Rcv=NULL
    
    for (i in 1:K) { #i=1
      test.index <- seq_len(n)[(s == i)] #test data
      train.index <- seq_len(n)[(s != i)] #training data
      
      #model with training data
      glm.fit <- glm(glmfit$call, data = data[train.index,])
      glm.fit1 <- stepAIC(glm.fit, trace = F)
      #observed test set y
      glm.y <- glmfit$y[test.index]
      #predicted y for test data
      pred.y <- predict(glm.fit1, newdata=data[test.index,])
      #observed - predicted on test data
      error= glm.y - pred.y
      #mean squred error
      MSE <- mean(error^2)
      CV=c(CV,MSE)
      R=1-sum(error^2)/sum((glm.y-mean(glm.y))^2)
      Rcv=c(Rcv,R)
    }
    
    #Output
    list(call = glmfit$call, K = K, 
         MSE = mean(CV),R2=mean(Rcv), 
         seed = seed)  
    
  }

mycv.stepAIC.logistic<-
  function (data, glmfit, K=10, seed=123) {
    #logistic regression
    #this function is to get the Cross-validated mean square error for regression
    #output R2 and MSE
    library(MASS)
    n <- nrow(data)
    set.seed(seed) #K=10
    
    #partition the data into K subsets
    f <- ceiling(n/K)
    s <- sample(rep(1:K, f), n)  
    
    #generate indices 1:10 and sample n of them  
    # K fold cross-validated mean squared error
    
    CV=NULL; O.P=NULL
    
    for (i in 1:K) { #i=1
      test.index <- seq_len(n)[(s == i)] #test data
      train.index <- seq_len(n)[(s != i)] #training data
      
      #model with training data
      glm.fit <- glm(glmfit$call, data = data[train.index,], family=binomial)
      glm.fit1 <- stepAIC(glm.fit, trace = F)
      #observed test set y
      test.y <- glmfit$y[test.index]
      
      #predicted probability for test data
      pred.y <- predict(glm.fit1, newdata=data[test.index,],type="response")
      
      #change prediction probability to class prediction
      tname=names(table(glmfit$y))
      ypred=ifelse(pred.y>.5,tname[2],tname[1])
      
      #
      error=mean(ypred!=test.y) #classification error 
      ovsp <- cbind(pred=ypred,obs=test.y) #pred vs obs vector
      
      
      CV <- c(CV,error) 
      O.P <- rbind(O.P,ovsp)
    }
    
    #Output
    list(call = glmfit$call, K = K, 
         Error = mean(CV), ConfusianMatrix=table(O.P[,1],O.P[,2]), 
         seed = seed)  
    
  }


mycv.lasso<-
  function (data, model=perf~., yname="perf", K=10, seed=123) {
    #Lasso model for regression 
    #this function is to get the Cross-validated mean square error for regression
    #output R2 and MSE
    require(class)
    library(glmnet)
    n <- nrow(data)
    set.seed(seed) #K=10
    datay=data[,yname] #response variable
    x=model.matrix(model,data)[,-1] #model matrix
    
    #partition the data into K subsets
    f <- ceiling(n/K)
    s <- sample(rep(1:K, f), n)  
    
    #generate indices 1:10 and sample n of them  
    # K fold cross-validated mean squared error
    
    CV=NULL; Rcv=NULL
    grid=10^seq(10,-2,length=100)
    
    for (i in 1:K) { #i=1
      test.index <- seq_len(n)[(s == i)] #test data
      train.index <- seq_len(n)[(s != i)] #training data
      
      #model with training data
      #Find best lambda from cv
      cv.out=cv.glmnet(x[train.index,],datay[train.index],alpha=1,
                       lambda=grid)
      bestlam=cv.out$lambda.min
      #using best lambda run lasso model
      #and get the prediction
      lasso.mod=glmnet(x[train.index,],datay[train.index],alpha=1,
                       lambda=grid)
      lasso.pred=predict(lasso.mod, s=bestlam, newx=x[test.index,])
      
      
      error=(lasso.pred-datay[test.index])
      MSE=mean(error^2)
      CV=c(CV,MSE)
      R=1-sum(error^2)/sum((datay[test.index]-mean(datay[test.index]))^2)
      Rcv=c(Rcv,R)
    }
    
    #Output
    list(call = model, K = K, 
         MSE = mean(CV),R2=mean(Rcv), 
         seed = seed)  
    
  }


mycv.lasso.logistic<-
  function (data, model=perf~., yname="perf", K=10, seed=123) {
    #Lasso model for logistic regression 
    #this function is to get the Cross-validated error for logistic regression
    #output error and confusion matrix
    require(class)
    library(glmnet)
    n <- nrow(data)
    set.seed(seed) #K=10
    datay=(data[,yname]) #response variable
    x=model.matrix(model,data)[,-1] #model matrix
    tn=names(table(datay))
    #partition the data into K subsets
    f <- ceiling(n/K)
    s <- sample(rep(1:K, f), n)  
    
    #generate indices 1:10 and sample n of them  
    # K fold cross-validated mean squared error
    
    CV=NULL
    grid=10^seq(10,-2,length=100)
    
    for (i in 1:K) { #i=1
      test.index <- seq_len(n)[(s == i)] #test data
      train.index <- seq_len(n)[(s != i)] #training data
      
      #model with training data
      #Find best lambda from cv
      cv.out=cv.glmnet(x[train.index,],datay[train.index],alpha=1,
                       lambda=grid,family="binomial")
      bestlam=cv.out$lambda.min
      #using best lambda run lasso model
      #and get the prediction
      
      lasso.pred=predict(cv.out, s=bestlam, newx=x[test.index,],type="class")
      lasso.pred=ifelse(lasso.pred=="1",tn[1],tn[2])
      error=mean(lasso.pred!=datay[test.index])
      CV=c(CV,error)
      
    }
    
    #Output
    list(call = model, K = K, 
         error = mean(CV), 
         seed = seed)  
    
  }


