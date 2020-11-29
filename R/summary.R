#'summary
#'
#'Determines various characteristics of a linearRegression model
#'
#'@param model a model fit through linearRegression
#'
#'@return a list containing the model formula, residuals, a data frame (entitled 'coefficients') with estimated regression coefficients, standard errors, t-statistics, and p-values for each coefficient, as well as
#'R-squared, adjusted R-squared, degrees of freedom, model F-statistic, p-value, and residual standard error
#'
#'@examples
#'fit<-linearRegression(dist~speed, cars); summary(fit)
#'
#'@import stats
#'
#'@export
summary<-function(model){
  #Model formula
  form<-model$formula

  #Residuals
  resids<-model$residuals

  #Coefficients
  coefic<-model$coefficients

  #Standard errors
  num_coef<-length(coefic)
  sigma_hat<-sum(resids^2)/model$df.residual


  #Create design matrix
  formula_character<-as.character(form)
  X_names<-gsub(" ", "", unlist(strsplit(formula_character[3], "+", fixed=TRUE)))

  X<-matrix(nrow=length(model$model[,1]), ncol=length(X_names)+1)
  X[,1]<-1

  for(i in 1:length(X_names)){
    int<-unlist(strsplit(X_names[i], split=":", fixed=TRUE))
    if (length(int)>1){
      X[,(i+1)]<-get(int[1], envir=as.environment(model$model))
      for(j in 2:length(int)){
        X[,(i+1)]<-X[,(i+1)] * get(int[j], envir=as.environment(model$model))
      }
    }else{
      X[,(i+1)]<-get(X_names[i], envir=as.environment(model$model))
    }
  }

  var_beta<-sigma_hat*solve(t(X)%*%X)

  standard_errors<-sqrt(diag(var_beta))

  #t-statistics
  t_stat<-coefic/standard_errors

  #p-values
  p_values<-2*(1-pt(abs(t_stat), model$df.residual))

  #Data frame of coefficients, standard errors, t-statistics, p-values
  df<-data.frame("Estimate"=coefic, "Std.Error"=standard_errors, "t.value"=t_stat, "p.value"=p_values)
  row.names(df)<-c("(Intercept)", X_names)

  #R-squared
  SSE<-sum(resids^2)
  SST<-sum((model$model[,1]-mean(model$model[,1]))^2)
  R_squared<-1-(SSE/SST)

  #Adjusted R-squared
  R_squared_adj<-1-((SSE/model$df.residual)/(SST/(length(model$model[,1])-1)))

  #Degrees of freedom
  df_num<-num_coef-1
  df_den<-model$df.residual

  #Model F-statistic
  SSR<-SST-SSE
  F_stat<-(SSR/df_num)/(SSE/df_den)

  #Model p-value
  model_p_value<-1-pf(F_stat, df_num, df_den)

  #Residual standard error
  resid_se<-sqrt(SSE/df_den)

  return(list(formula=form, residuals=resids, coefficients=df, df=c(df_num, df_den), R.squared=R_squared, adj.R.squared=R_squared_adj, F.stat=F_stat, model.p.value=model_p_value, residual.se=resid_se))
}
