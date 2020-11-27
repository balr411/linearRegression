#'anova
#'
#'Performs analysis of variance on a model already fitted through linearRegression
#'
#'@param model a model fit through linearRegression
#'
#'@return an analysis of variance table
#'
#'@examples
#'fit<-linearRegression(dist~speed, cars); anova(fit)
#'
#'@import stats
#'
#'@export
anova<-function(model){
  #Initialize ANOVA table
  row_num<-length(model$coefficients)
  df<-data.frame("Df"=rep(NA, row_num), "Sum.Sq"=rep(NA, row_num), "Mean.Sq"=rep(NA, row_num), "F.value"=rep(NA, row_num), "P.value"=rep(NA, row_num))
  num_observations<-length(model$residuals)

  #Get error sum of squares and mean square error - don't forget to name the rows
  resids<-model$residuals
  SSE<-sum(resids^2)
  df_error<-model$df.residual
  MSE<-SSE/df_error
  df[[1]][row_num]<-df_error
  df[[2]][row_num]<-SSE
  df[[3]][row_num]<-MSE

  #Get regression sum of squares and MSR for each predictor
  if(row_num==2){ #simple linear regression case
    df[[1]][1]<-1
    df[[2]][1]<-sum((model$model[,1]-mean(model$model[,1]))^2)-SSE
    df[[3]][1]<-df[[2]][1]
    df[[4]][1]<-df[[3]][1]/MSE
    df[[5]][1]<-1-pf(df[[4]][1], 1, df_error)
  }else{
    formula_character<-as.character(formula(model$formula))
    dep<-formula_character[3]
    dep_vec<-unlist(strsplit(dep, split="+",fixed=TRUE))
    dep_vec<-gsub(" ", "", dep_vec)
    #Run regression model on only first predictor to get SSR of first predictor
    formula_current<-paste(formula_character[2], "~", dep_vec[1], sep="")
    model_current<-linearRegression(formula_current, dat=model$model)

    df[[1]][1]<-1
    df[[2]][1]<-sum((model_current$model[,1]-mean(model_current$model[,1]))^2)-sum(model_current$residuals^2)
    df[[3]][1]<-df[[2]][1]
    df[[4]][1]<-df[[3]][1]/MSE
    df[[5]][1]<-1-pf(df[[4]][1], 1, df_error)

    #Keep track of each model's SSE value so we can use partial sum of squares to find each variables SSR
    SSE_models<-vector(length=length(dep_vec))
    SSE_models[1]<-sum(model_current$residuals^2)

    #Loop over remaining independent variables, adding them to the model one by one to find their respective SSR values
    for(i in 2:(length(dep_vec))){
      formula_current<-paste(formula_character[2], "~", paste(dep_vec[1:i], collapse="+"), sep="")
      model_current<-linearRegression(formula_current, dat=model$model)
      SSE_models[i]<-sum(model_current$residuals^2)
      df[[1]][i]<-1
      df[[2]][i]<- SSE_models[i-1] - SSE_models[i]
      df[[3]][i]<-df[[2]][i]
      df[[4]][i]<-df[[3]][i]/MSE
      df[[5]][i]<-1-pf(df[[4]][i], 1, df_error)
    }
  }

  if(row_num==2){
    row.names(df)<-gsub(" ", "", c(as.character(formula(model$formula))[3], "Residuals"))
  }else{
    row.names(df)<-c(dep_vec, "Residuals")
  }

  return(df)


}
