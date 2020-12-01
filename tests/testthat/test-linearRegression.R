test_that("test if linearRegression returns correct results", {
  fit_linReg<-linearRegression(mpg~cyl+disp+hp+wt, dat=mtcars)
  fit_lm<-lm(mpg~cyl+disp+hp+wt, data=mtcars)
  expect_equal(as.numeric(fit_linReg$coefficients), as.numeric(fit_lm$coefficients))
  expect_equal(as.numeric(fit_linReg$residuals), as.numeric(fit_lm$residuals))
  expect_equal(fit_linReg$rank, fit_lm$rank)
  expect_equal(as.numeric(fit_linReg$fitted.values), as.numeric(fit_lm$fitted.values))
  expect_equal(fit_linReg$df.residual,fit_lm$df.residual)
  expect_equal(as.numeric(as.matrix(fit_linReg$model)), as.numeric(as.matrix(fit_lm$model)))
  expect_equal(fit_linReg$formula, formula(mpg~cyl+disp+hp+wt))
})

test_that("test if anova function returns correct results", {
  fit_linReg<-linearRegression(mpg~cyl+disp+hp+wt, dat=mtcars)
  fit_lm<-lm(mpg~cyl+disp+hp+wt, data=mtcars)
  anova_fit_linReg<-linearRegression::anova(fit_linReg)
  anova_fit_lm<-stats::anova(fit_lm)

  expect_equal(as.numeric(anova_fit_linReg[[1]]), as.numeric(anova_fit_lm[[1]]))
  expect_equal(as.numeric(anova_fit_linReg[[2]]), as.numeric(anova_fit_lm[[2]]))
  expect_equal(as.numeric(anova_fit_linReg[[3]][1:(length(anova_fit_linReg[[3]])-2)]), as.numeric(anova_fit_lm[[3]][1:(length(anova_fit_lm[[3]])-2)]))
  expect_equal(as.numeric(anova_fit_linReg[[4]][1:(length(anova_fit_linReg[[4]])-2)]), as.numeric(anova_fit_lm[[4]][1:(length(anova_fit_lm[[4]])-2)]))

})

test_that("test if summary function returns correct results",{
  fit_linReg<-linearRegression(mpg~cyl+disp+hp+wt, dat=mtcars)
  fit_lm<-lm(mpg~cyl+disp+hp+wt, data=mtcars)
  summary_fit_linReg<-linearRegression::summary(fit_linReg)
  summary_fit_lm<-base::summary(fit_lm)

  expect_equal(summary_fit_linReg$R.squared,summary_fit_lm$r.squared)
  expect_equal(summary_fit_linReg$adj.R.squared,summary_fit_lm$adj.r.squared)
  expect_equal(as.vector(summary_fit_linReg$coefficients[,1]), as.vector(summary_fit_lm$coefficients[,1]))
  expect_equal(as.vector(summary_fit_linReg$coefficients[,2]), as.vector(summary_fit_lm$coefficients[,2]))
  expect_equal(as.vector(summary_fit_linReg$coefficients[,3]), as.vector(summary_fit_lm$coefficients[,3]))
  expect_equal(as.vector(summary_fit_linReg$coefficients[,4]), as.vector(summary_fit_lm$coefficients[,4]))
  expect_equal(summary_fit_linReg$F.stat, as.numeric(summary_fit_lm$fstatistic[1]))
  expect_equal(summary_fit_linReg$df, as.vector(summary_fit_lm$fstatistic[2:3]))
  expect_equal(summary_fit_linReg$formula, formula(mpg~cyl+disp+hp+wt))
  expect_equal(as.numeric(summary_fit_linReg$residuals), as.numeric(summary_fit_lm$residuals))
  expect_equal(summary_fit_linReg$model.p.value, 1-pf(as.numeric(summary_fit_lm$fstatistic[1]), as.numeric(summary_fit_lm$fstatistic[2]), as.numeric(summary_fit_lm$fstatistic[3])))

})


test_that("test if functions work when no dat given and interaction included",{
  y<<-rnorm(100)
  x1<<-rnorm(100)
  x2<<-rnorm(100)

  fit_linReg<-linearRegression(y~x1+x2+x1:x2)
  fit_lm<-lm(y~x1+x2+x1:x2)

  anova_fit_linReg<-linearRegression::anova(fit_linReg)
  anova_fit_lm<-stats::anova(fit_lm)

  summary_fit_linReg<-linearRegression::summary(fit_linReg)
  summary_fit_lm<-base::summary(fit_lm)

  expect_equal(as.numeric(fit_linReg$coefficients), as.numeric(fit_lm$coefficients))
  expect_equal(as.numeric(fit_linReg$residuals), as.numeric(fit_lm$residuals))
  expect_equal(fit_linReg$rank, fit_lm$rank)
  expect_equal(as.numeric(fit_linReg$fitted.values), as.numeric(fit_lm$fitted.values))
  expect_equal(fit_linReg$df.residual,fit_lm$df.residual)
  expect_equal(as.numeric(as.matrix(fit_linReg$model)), as.numeric(as.matrix(fit_lm$model)))
  expect_equal(fit_linReg$formula, formula(y~x1+x2+x1:x2))

  expect_equal(as.numeric(anova_fit_linReg[[1]]), as.numeric(anova_fit_lm[[1]]))
  expect_equal(as.numeric(anova_fit_linReg[[2]]), as.numeric(anova_fit_lm[[2]]))
  expect_equal(as.numeric(anova_fit_linReg[[3]][1:(length(anova_fit_linReg[[3]])-2)]), as.numeric(anova_fit_lm[[3]][1:(length(anova_fit_lm[[3]])-2)]))
  expect_equal(as.numeric(anova_fit_linReg[[4]][1:(length(anova_fit_linReg[[4]])-2)]), as.numeric(anova_fit_lm[[4]][1:(length(anova_fit_lm[[4]])-2)]))

  expect_equal(summary_fit_linReg$R.squared,summary_fit_lm$r.squared)
  expect_equal(summary_fit_linReg$adj.R.squared,summary_fit_lm$adj.r.squared)
  expect_equal(as.vector(summary_fit_linReg$coefficients[,1]), as.vector(summary_fit_lm$coefficients[,1]))
  expect_equal(as.vector(summary_fit_linReg$coefficients[,2]), as.vector(summary_fit_lm$coefficients[,2]))
  expect_equal(as.vector(summary_fit_linReg$coefficients[,3]), as.vector(summary_fit_lm$coefficients[,3]))
  expect_equal(as.vector(summary_fit_linReg$coefficients[,4]), as.vector(summary_fit_lm$coefficients[,4]))
  expect_equal(summary_fit_linReg$F.stat, as.numeric(summary_fit_lm$fstatistic[1]))
  expect_equal(summary_fit_linReg$df, as.vector(summary_fit_lm$fstatistic[2:3]))
  expect_equal(summary_fit_linReg$formula, formula(y~x1+x2+x1:x2))
  expect_equal(as.numeric(summary_fit_linReg$residuals), as.numeric(summary_fit_lm$residuals))
  expect_equal(summary_fit_linReg$model.p.value, 1-pf(as.numeric(summary_fit_lm$fstatistic[1]), as.numeric(summary_fit_lm$fstatistic[2]), as.numeric(summary_fit_lm$fstatistic[3])))
})

test_that("use subset function",{
  y<<-rnorm(100)
  x1<<-rnorm(100)
  x2<<-rnorm(100)

  fit_linReg<-linearRegression(y~x1+x2+x1:x2, subs=1:50)
  fit_lm<-lm(y~x1+x2+x1:x2, subset=1:50)

  anova_fit_linReg<-linearRegression::anova(fit_linReg)
  anova_fit_lm<-stats::anova(fit_lm)

  summary_fit_linReg<-linearRegression::summary(fit_linReg)
  summary_fit_lm<-base::summary(fit_lm)

  expect_equal(as.numeric(fit_linReg$coefficients), as.numeric(fit_lm$coefficients))
  expect_equal(as.numeric(fit_linReg$residuals), as.numeric(fit_lm$residuals))
  expect_equal(fit_linReg$rank, fit_lm$rank)
  expect_equal(as.numeric(fit_linReg$fitted.values), as.numeric(fit_lm$fitted.values))
  expect_equal(fit_linReg$df.residual,fit_lm$df.residual)
  expect_equal(as.numeric(as.matrix(fit_linReg$model)), as.numeric(as.matrix(fit_lm$model)))
  expect_equal(fit_linReg$formula, formula(y~x1+x2+x1:x2))

  expect_equal(as.numeric(anova_fit_linReg[[1]]), as.numeric(anova_fit_lm[[1]]))
  expect_equal(as.numeric(anova_fit_linReg[[2]]), as.numeric(anova_fit_lm[[2]]))
  expect_equal(as.numeric(anova_fit_linReg[[3]][1:(length(anova_fit_linReg[[3]])-2)]), as.numeric(anova_fit_lm[[3]][1:(length(anova_fit_lm[[3]])-2)]))
  expect_equal(as.numeric(anova_fit_linReg[[4]][1:(length(anova_fit_linReg[[4]])-2)]), as.numeric(anova_fit_lm[[4]][1:(length(anova_fit_lm[[4]])-2)]))

  expect_equal(summary_fit_linReg$R.squared,summary_fit_lm$r.squared)
  expect_equal(summary_fit_linReg$adj.R.squared,summary_fit_lm$adj.r.squared)
  expect_equal(as.vector(summary_fit_linReg$coefficients[,1]), as.vector(summary_fit_lm$coefficients[,1]))
  expect_equal(as.vector(summary_fit_linReg$coefficients[,2]), as.vector(summary_fit_lm$coefficients[,2]))
  expect_equal(as.vector(summary_fit_linReg$coefficients[,3]), as.vector(summary_fit_lm$coefficients[,3]))
  expect_equal(as.vector(summary_fit_linReg$coefficients[,4]), as.vector(summary_fit_lm$coefficients[,4]))
  expect_equal(summary_fit_linReg$F.stat, as.numeric(summary_fit_lm$fstatistic[1]))
  expect_equal(summary_fit_linReg$df, as.vector(summary_fit_lm$fstatistic[2:3]))
  expect_equal(summary_fit_linReg$formula, formula(y~x1+x2+x1:x2))
  expect_equal(as.numeric(summary_fit_linReg$residuals), as.numeric(summary_fit_lm$residuals))
  expect_equal(summary_fit_linReg$model.p.value, 1-pf(as.numeric(summary_fit_lm$fstatistic[1]), as.numeric(summary_fit_lm$fstatistic[2]), as.numeric(summary_fit_lm$fstatistic[3])))

})

test_that("test when dat given and interaction included",{
  fit_linReg<-linearRegression(mpg~cyl+disp+hp+wt+cyl:disp, dat=mtcars)
  fit_lm<-lm(mpg~cyl+disp+hp+wt+cyl:disp, data=mtcars)

  anova_fit_linReg<-linearRegression::anova(fit_linReg)
  anova_fit_lm<-stats::anova(fit_lm)

  summary_fit_linReg<-linearRegression::summary(fit_linReg)
  summary_fit_lm<-base::summary(fit_lm)

  expect_equal(as.numeric(fit_linReg$coefficients), as.numeric(fit_lm$coefficients))
  expect_equal(as.numeric(fit_linReg$residuals), as.numeric(fit_lm$residuals))
  expect_equal(fit_linReg$rank, fit_lm$rank)
  expect_equal(as.numeric(fit_linReg$fitted.values), as.numeric(fit_lm$fitted.values))
  expect_equal(fit_linReg$df.residual,fit_lm$df.residual)
  expect_equal(as.numeric(as.matrix(fit_linReg$model)), as.numeric(as.matrix(fit_lm$model)))
  expect_equal(fit_linReg$formula, formula(mpg~cyl+disp+hp+wt+cyl:disp))

  expect_equal(as.numeric(anova_fit_linReg[[1]]), as.numeric(anova_fit_lm[[1]]))
  expect_equal(as.numeric(anova_fit_linReg[[2]]), as.numeric(anova_fit_lm[[2]]))
  expect_equal(as.numeric(anova_fit_linReg[[3]][1:(length(anova_fit_linReg[[3]])-2)]), as.numeric(anova_fit_lm[[3]][1:(length(anova_fit_lm[[3]])-2)]))
  expect_equal(as.numeric(anova_fit_linReg[[4]][1:(length(anova_fit_linReg[[4]])-2)]), as.numeric(anova_fit_lm[[4]][1:(length(anova_fit_lm[[4]])-2)]))

  expect_equal(summary_fit_linReg$R.squared,summary_fit_lm$r.squared)
  expect_equal(summary_fit_linReg$adj.R.squared,summary_fit_lm$adj.r.squared)
  expect_equal(as.vector(summary_fit_linReg$coefficients[,1]), as.vector(summary_fit_lm$coefficients[,1]))
  expect_equal(as.vector(summary_fit_linReg$coefficients[,2]), as.vector(summary_fit_lm$coefficients[,2]))
  expect_equal(as.vector(summary_fit_linReg$coefficients[,3]), as.vector(summary_fit_lm$coefficients[,3]))
  expect_equal(as.vector(summary_fit_linReg$coefficients[,4]), as.vector(summary_fit_lm$coefficients[,4]))
  expect_equal(summary_fit_linReg$F.stat, as.numeric(summary_fit_lm$fstatistic[1]))
  expect_equal(summary_fit_linReg$df, as.vector(summary_fit_lm$fstatistic[2:3]))
  expect_equal(summary_fit_linReg$formula, formula(mpg~cyl+disp+hp+wt+cyl:disp))
  expect_equal(as.numeric(summary_fit_linReg$residuals), as.numeric(summary_fit_lm$residuals))
  expect_equal(summary_fit_linReg$model.p.value, 1-pf(as.numeric(summary_fit_lm$fstatistic[1]), as.numeric(summary_fit_lm$fstatistic[2]), as.numeric(summary_fit_lm$fstatistic[3])))
})

test_that("test if functions work when no dat given and no interaction included",{
  y<<-rnorm(100)
  x1<<-rnorm(100)
  x2<<-rnorm(100)

  fit_linReg<-linearRegression(y~x1+x2)
  fit_lm<-lm(y~x1+x2)

  anova_fit_linReg<-linearRegression::anova(fit_linReg)
  anova_fit_lm<-stats::anova(fit_lm)

  summary_fit_linReg<-linearRegression::summary(fit_linReg)
  summary_fit_lm<-base::summary(fit_lm)

  expect_equal(as.numeric(fit_linReg$coefficients), as.numeric(fit_lm$coefficients))
  expect_equal(as.numeric(fit_linReg$residuals), as.numeric(fit_lm$residuals))
  expect_equal(fit_linReg$rank, fit_lm$rank)
  expect_equal(as.numeric(fit_linReg$fitted.values), as.numeric(fit_lm$fitted.values))
  expect_equal(fit_linReg$df.residual,fit_lm$df.residual)
  expect_equal(as.numeric(as.matrix(fit_linReg$model)), as.numeric(as.matrix(fit_lm$model)))
  expect_equal(fit_linReg$formula, formula(y~x1+x2))

  expect_equal(as.numeric(anova_fit_linReg[[1]]), as.numeric(anova_fit_lm[[1]]))
  expect_equal(as.numeric(anova_fit_linReg[[2]]), as.numeric(anova_fit_lm[[2]]))
  expect_equal(as.numeric(anova_fit_linReg[[3]][1:(length(anova_fit_linReg[[3]])-2)]), as.numeric(anova_fit_lm[[3]][1:(length(anova_fit_lm[[3]])-2)]))
  expect_equal(as.numeric(anova_fit_linReg[[4]][1:(length(anova_fit_linReg[[4]])-2)]), as.numeric(anova_fit_lm[[4]][1:(length(anova_fit_lm[[4]])-2)]))

  expect_equal(summary_fit_linReg$R.squared,summary_fit_lm$r.squared)
  expect_equal(summary_fit_linReg$adj.R.squared,summary_fit_lm$adj.r.squared)
  expect_equal(as.vector(summary_fit_linReg$coefficients[,1]), as.vector(summary_fit_lm$coefficients[,1]))
  expect_equal(as.vector(summary_fit_linReg$coefficients[,2]), as.vector(summary_fit_lm$coefficients[,2]))
  expect_equal(as.vector(summary_fit_linReg$coefficients[,3]), as.vector(summary_fit_lm$coefficients[,3]))
  expect_equal(as.vector(summary_fit_linReg$coefficients[,4]), as.vector(summary_fit_lm$coefficients[,4]))
  expect_equal(summary_fit_linReg$F.stat, as.numeric(summary_fit_lm$fstatistic[1]))
  expect_equal(summary_fit_linReg$df, as.vector(summary_fit_lm$fstatistic[2:3]))
  expect_equal(summary_fit_linReg$formula, formula(y~x1+x2))
  expect_equal(as.numeric(summary_fit_linReg$residuals), as.numeric(summary_fit_lm$residuals))
  expect_equal(summary_fit_linReg$model.p.value, 1-pf(as.numeric(summary_fit_lm$fstatistic[1]), as.numeric(summary_fit_lm$fstatistic[2]), as.numeric(summary_fit_lm$fstatistic[3])))
})

test_that("simple linear regression",{
  y<<-rnorm(100)
  x1<<-rnorm(100)

  fit_linReg<-linearRegression(y~x1)
  fit_lm<-lm(y~x1)

  anova_fit_linReg<-linearRegression::anova(fit_linReg)
  anova_fit_lm<-stats::anova(fit_lm)

  summary_fit_linReg<-linearRegression::summary(fit_linReg)
  summary_fit_lm<-base::summary(fit_lm)

  expect_equal(as.numeric(fit_linReg$coefficients), as.numeric(fit_lm$coefficients))
  expect_equal(as.numeric(fit_linReg$residuals), as.numeric(fit_lm$residuals))
  expect_equal(fit_linReg$rank, fit_lm$rank)
  expect_equal(as.numeric(fit_linReg$fitted.values), as.numeric(fit_lm$fitted.values))
  expect_equal(fit_linReg$df.residual,fit_lm$df.residual)
  expect_equal(as.numeric(as.matrix(fit_linReg$model)), as.numeric(as.matrix(fit_lm$model)))
  expect_equal(fit_linReg$formula, formula(y~x1))

  expect_equal(as.numeric(anova_fit_linReg[[1]]), as.numeric(anova_fit_lm[[1]]))
  expect_equal(as.numeric(anova_fit_linReg[[2]]), as.numeric(anova_fit_lm[[2]]))
  expect_equal(as.numeric(anova_fit_linReg[[3]][1:(length(anova_fit_linReg[[3]])-2)]), as.numeric(anova_fit_lm[[3]][1:(length(anova_fit_lm[[3]])-2)]))
  expect_equal(as.numeric(anova_fit_linReg[[4]][1:(length(anova_fit_linReg[[4]])-2)]), as.numeric(anova_fit_lm[[4]][1:(length(anova_fit_lm[[4]])-2)]))

  expect_equal(summary_fit_linReg$R.squared,summary_fit_lm$r.squared)
  expect_equal(summary_fit_linReg$adj.R.squared,summary_fit_lm$adj.r.squared)
  expect_equal(as.vector(summary_fit_linReg$coefficients[,1]), as.vector(summary_fit_lm$coefficients[,1]))
  expect_equal(as.vector(summary_fit_linReg$coefficients[,2]), as.vector(summary_fit_lm$coefficients[,2]))
  expect_equal(as.vector(summary_fit_linReg$coefficients[,3]), as.vector(summary_fit_lm$coefficients[,3]))
  expect_equal(as.vector(summary_fit_linReg$coefficients[,4]), as.vector(summary_fit_lm$coefficients[,4]))
  expect_equal(summary_fit_linReg$F.stat, as.numeric(summary_fit_lm$fstatistic[1]))
  expect_equal(summary_fit_linReg$df, as.vector(summary_fit_lm$fstatistic[2:3]))
  expect_equal(summary_fit_linReg$formula, formula(y~x1))
  expect_equal(as.numeric(summary_fit_linReg$residuals), as.numeric(summary_fit_lm$residuals))
  expect_equal(summary_fit_linReg$model.p.value, 1-pf(as.numeric(summary_fit_lm$fstatistic[1]), as.numeric(summary_fit_lm$fstatistic[2]), as.numeric(summary_fit_lm$fstatistic[3])))
})
