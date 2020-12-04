#'linearRegression
#'
#'Performs a linear regression analysis on the given formula
#'
#'@param form an object of class 'formula', or one that can be coerced to that class. ie. a symbolic description of the model to be fitted
#'
#'@param dat an optional data frame containing the data on the model to be fit
#'
#'@param subs an optional vector specifying a subset of data to be used
#'
#'@return a list of length 7 containing the coefficients of the model, residuals, fitted values, rank of the fitted model, the residual degrees of freedom, the model formula used, and the model frame used
#'
#'@examples
#'linearRegression(dist~speed, cars)
#'y<-rnorm(100); x<-rnorm(100); linearRegression(y~x, subs=1:50)
#'linearRegression(mpg~cyl+disp+hp+wt+cyl:disp+disp:wt+cyl:disp:hp, dat=mtcars)
#'
#'@import stats
#'
#'@export
#'
linearRegression<-function(form, dat=NULL, subs=NULL){
  #Get the independent and dependent variables
  formula_character<-as.character(formula(form))
  y_symbolic<-formula_character[2]
  dep<-formula_character[3]
  dep_vec<-unlist(strsplit(dep, split="+",fixed=TRUE))

  if(!is.null(dat)){
    y<-get(y_symbolic, envir=as.environment(dat))
  }else{
    y<-get(y_symbolic)
  }

  X_names<-unique(gsub(" ", "", dep_vec))

  #Add independent variables to design matrix
  X<-matrix(nrow=length(y), ncol=length(X_names)+1)
  X[,1]<-1
  num_nonint<-0
  if(!is.null(dat)){
    for(i in 1:length(X_names)){
      int<-unlist(strsplit(X_names[i], split=":", fixed=TRUE))
      if (length(int)>1){
        X[,(i+1)]<-get(int[1], envir=as.environment(dat))
        for(j in 2:length(int)){
          X[,(i+1)]<-X[,(i+1)] * get(int[j], envir=as.environment(dat))
        }
      }else{
        X[,(i+1)]<-get(X_names[i], envir=as.environment(dat))
      }
    }
  }else{
    for(i in 1:length(X_names)){
      int<-unlist(strsplit(X_names[i], split=":", fixed=TRUE))
      if(length(int)>1){
        X[,(i+1)]<-get(int[1])
        for(j in 2:length(int)){
          X[,(i+1)]<-X[,(i+1)] * get(int[j])
        }
      }else{
        X[,(i+1)]<-get(X_names[i])
      }
    }
  }

  #Check subset option
  if(!is.null(subs)){
    X<-X[subs,]
    y<-y[subs]
  }

  #Find regression coefficients
  beta<-solve((t(X)%*%X))%*%t(X)%*%as.matrix(y)
  names(beta)<-c("(Intercept)", X_names)

  #Get residuals
  H<-X%*%solve((t(X)%*%X))%*%t(X)
  resids<-(diag(nrow=length(y), ncol=length(y))-H)%*%as.matrix(y)

  #Rank
  rank<-dim(X)[2]

  #Fitted values
  fitted_values<-H%*%as.matrix(y)

  #Model frame
  model<-data.frame(y)
  X_names_model<-unique(unlist(sapply(X_names, function(x)strsplit(x, split=":")))) #vector with names of all independent variables used in model
  if(!is.null(dat)){
    for(i in 1:length(X_names_model)){
      model<-cbind(model, get(X_names_model[i], envir=as.environment(dat)))
    }
  }else{
    for(i in 1:length(X_names_model)){
      model<-cbind(model, get(X_names_model[i]))
    }
  }

  names(model)<-c(y_symbolic, X_names_model)

  if(!is.null(subs)){
    model<-model[subs,]
  }



  #Residual degrees of freedom
  residual_df<-length(y)-dim(X)[2]

  #Formula called
  form_to_return<-paste(y_symbolic, "~", paste(X_names, collapse="+"), sep="")

  return(list(coefficients=beta, residuals=resids, rank=rank, fitted.values=fitted_values, df.residual=residual_df, model=model, formula=formula(form_to_return)))

}
