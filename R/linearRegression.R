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

  #Find independent variables to be used in model
  X_names<-c()
  for (i in 1:length(dep_vec)){
    starred_terms<-unlist(strsplit(dep_vec[i], split="*", fixed=TRUE))
    if (length(starred_terms)>1){
      X_names<-c(X_names, starred_terms)
      for (j in 1:(length(starred_terms)-1)){
        for (k in (j+1):length(starred_terms)){
          X_names<-c(X_names, paste(starred_terms[j], starred_terms[k], sep=":"))
        }
      }
    }else{
      X_names<-c(X_names, dep_vec[i])
    }
  }

  X_names<-unique(gsub(" ", "", X_names))

  #Add independent variables to design matrix
  X<-matrix(nrow=length(y), ncol=length(X_names)+1)
  X[,1]<-1
  num_nonint<-0
  if(!is.null(dat)){
    for(i in 1:length(X_names)){
      int<-unlist(strsplit(X_names[i], split=":", fixed=TRUE))
      if (length(int)>1){
        X[,(i+1)]<-get(int[1], envir=as.environment(dat))*get(int[2], envir=as.environment(dat))
      }else{
        X[,(i+1)]<-get(X_names[i], envir=as.environment(dat))
        num_nonint<-num_nonint+1
      }
    }
  }else{
    for(i in 1:length(X_names)){
      int<-unlist(strsplit(X_names[i], split=":", fixed=TRUE))
      if(length(int)>1){
        X[,(i+1)]<-get(int[1])*get(int[2])
      }else{
        X[,(i+1)]<-get(X_names[i])
        num_nonint<-num_nonint+1
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
  names(beta)<-c("(intercept)", X_names)

  #Get residuals
  H<-X%*%solve((t(X)%*%X))%*%t(X)
  resids<-(diag(nrow=length(y), ncol=length(y))-H)%*%as.matrix(y)

  #Rank
  rank<-dim(X)[2]

  #Fitted values
  fitted_values<-H%*%as.matrix(y)

  #Model frame
  model<-as.data.frame(X)
  model[[1]]<-y
  model<-model[,1:(num_nonint+1)]
  names(model)<-c(y_symbolic, X_names[1:num_nonint])

  #Residual degrees of freedom
  residual_df<-length(y)-dim(X)[2]

  return(list(coefficients=beta, residuals=resids, rank=rank, fitted.values=fitted_values, df.residual=residual_df, model=model, formula=formula(form)))

}
