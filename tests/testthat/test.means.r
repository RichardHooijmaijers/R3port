#library(testthat)
library(R3port)
context("descriptive statistics calculations")

test_that("Descriptives in pack 1 are correctly calculated and formatted", {
  data(Indometh)
  res  <- means(Indometh,"conc","time",dig=2)
  comp <- function(val,by,stats)  as.character(formatC(round(tapply(val,by,stats),15),digits = 2,format="f"))
  # Round was applied to overcome floating point differences
  expect_equal(res$value[res$statistic=="N"], as.character(tapply(Indometh$conc,Indometh$time,length)))
  expect_equivalent(res$value[res$statistic=="Mean"],comp(Indometh$conc,Indometh$time,mean))
  expect_equivalent(res$value[res$statistic=="Median"],comp(Indometh$conc,Indometh$time,median))
  expect_equivalent(res$value[res$statistic=="SD"],comp(Indometh$conc,Indometh$time,sd))
  expect_equivalent(res$value[res$statistic=="Min"],comp(Indometh$conc,Indometh$time,min))
  expect_equivalent(res$value[res$statistic=="Max"],comp(Indometh$conc,Indometh$time,max))
})

test_that("alpha is used correctly", {
  data(Indometh)
  res  <- means(Indometh,"conc","time",pack=2,alpha=.1,dig=2)
  calc <- res$value[res$statistic=="90% CLM" & res$time==0.25]
  comp <- Indometh$conc[Indometh$time==0.25]
  comp1 <- mean(comp) - qt(0.95,df=length(comp)-1) * sd(comp) / sqrt(length(comp))
  comp1 <- as.character(formatC(round(comp1,15),digits=2,format="f"))
  comp2 <- mean(comp) + qt(0.95,df=length(comp)-1) * sd(comp) / sqrt(length(comp))
  comp2 <- as.character(formatC(round(comp2,15),digits=2,format="f"))

  expect_equivalent(as.character(unique(res$statistic[grep("CLM",res$statistic)])),"90% CLM")
  expect_equivalent(calc,paste(comp1,comp2,sep=" - "))
})

test_that("check if totals are calculated correctly", {
  data(Indometh)
  res  <- means(Indometh,"conc","time",total="time",pack=2,alpha=.1,dig=2)

})
