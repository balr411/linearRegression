test_that("test if linearRegression returns correct results", {
  fit_linReg<-linearRegression(mpg~cyl+disp+hp+wt, dat=mtcars)
  fit_lm<-lm(mpg~cyl+disp+hp+wt, data=mtcars)
  expect_equal(as.numeric(fit_linReg$coefficients), as.numeric(fit_lm$coefficients))
  expect_equal(as.numeric(fit_linReg$residuals), as.numeric(fit_lm$residuals))
  expect_equal(fit_linReg$rank, fit_lm$rank)
  expect_equal(as.numeric(fit_linReg$fitted.values), as.numeric(fit_lm$fitted.values))
  expect_equal(fit_linReg$df.residual,fit_lm$df.residual)
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
  expect_equal(as.vector(summary_fit_linReg$coefficients[,2]), as.vector(summary_fit_lm$coefficients[,2]))
  expect_equal(as.vector(summary_fit_linReg$coefficients[,3]), as.vector(summary_fit_lm$coefficients[,3]))
  expect_equal(as.vector(summary_fit_linReg$coefficients[,4]), as.vector(summary_fit_lm$coefficients[,4]))
  expect_equal(summary_fit_linReg$F.stat, as.numeric(summary_fit_lm$fstatistic[1]))
  expect_equal(summary_fit_linReg$df, as.vector(summary_fit_lm$fstatistic[2:3]))

})
