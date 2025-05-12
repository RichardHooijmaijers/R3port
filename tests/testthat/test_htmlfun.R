#library(testthat)
library(R3port)
library(ggplot2)
context("Test html related functions")

test_that("html_doc function works as expected", {
  options(show=FALSE, compile=FALSE, orientation="portrait", template=system.file("simple.html",package = "R3port"))
  tmplt <- readLines(system.file("simple.html",package = "R3port"))
  res1  <- capture.output(ltx_doc("test",template=paste0(system.file(package = "R3port"),"/simple.html")))

  # There should be a difference at only three places (title, orientation, rrres)
  expect_equal(sum(!res1%in%tmplt),3)
  expect_true(any(grepl("DOCTYPE html",res1)))
  expect_true(any(grepl("<body>",res1)))
  expect_true(any(grepl("</body>",res1)))
  
  html_doc("test2", out=paste0(tempdir(),"/subdirc/test.html"))
  expect_true(file.exists(paste0(tempdir(),"/subdirc/test.html")))
})

test_that("html_plot function works as expected", {
  data(Theoph)
  pl1  <- ggplot(Theoph,aes(Time,conc, group=Subject)) + geom_line()
  pl2  <- pl1 + facet_wrap(~Subject)
  tmfn <- tempfile(fileext=".html")
  html_plot(list(pl1,pl2), out = tmfn, title = "Test Title")

  expect_true(file.exists(tmfn))
  expect_true(file.exists(paste0(tools::file_path_sans_ext(tmfn),".html.rawhtml")))
  tf <- readLines(tmfn)
  expect_true(any(grepl("<h1>[[:alnum:]]*| *</h1>",tf)))
  expect_true(file.exists(paste0(tempdir(),"/figures")))
  expect_true(any(grepl("\\.png$",list.files(paste0(tempdir(),"/figures")))))

  pl3 <- function() {
    plot(conc~Time,data=Theoph)
    title(main="a plot")
  }
  tmfn2 <- tempfile(fileext=".html")
  html_plot(pl3(), out = tmfn2, pwidth = 2000, pheight = 1200)
  expect_true(file.exists(tmfn2))
  expect_true(any(grepl("\\.png$",list.files(paste0(tempdir(),"/figures")))))
  
  expect_error(html_plot(pl3()))
  options(pwidth=1200,pheight=500)
  html_plot(pl1, out = tmfn, title = "Test Title", cleancur = TRUE, titlepr = "01")
})


test_that("html_list function works as expected", {
  data(Theoph)
  grp  <- c(rep("",3),rep("grouped variables",2))
  tmfn <- tempfile(fileext=".html")
  html_list(Theoph,out=tmfn,vargroup=grp, template=system.file("bootstrap.html",package="R3port"))
  
  tf <- readLines(tmfn)
  expect_true(any(grepl("<table class='sample'>",tf)))
  
  html_list(head(Theoph),out=tmfn, xrepeat = TRUE, group = 1,, tclass = 'test')
  
  tf <- readLines(tmfn)
  expect_true(any(grepl("<table class='test'>",tf)))
})

test_that("html_table function works as expected", {
  data(Indometh)
  Indometh$id  <- as.numeric(as.character(Indometh$Subject))
  Indometh$trt <- ifelse(Indometh$id<4,"trt 1","trt 2")
  tmfn         <- tempfile(fileext=".html")
  
  html_table(Indometh,x=c("trt","time"),y="id",var="conc", out=tmfn, xabove=TRUE)
  
  tf <- readLines(tmfn)
  expect_true(any(grepl("xabove",tf)))
  expect_true(sum(grepl("colspan",tf))>1)
  
  html_table(Indometh,x=c("trt","time"),y="id",var="conc", out=paste0(tempdir(),"/subdirde/test.html"), yhead=TRUE)
  tf <- readLines(paste0(tempdir(),"/subdirde/test.html"))
  expect_true(any(grepl("colspan.*id",tf)))
  expect_true(file.exists(paste0(tempdir(),"/subdirde/test.html")))
})


test_that("html_combine function works as expected", {
  data(Theoph)
  html_list(Theoph[1:11,],out=tempfile(fileext=".html"))
  html_plot(plot(conc~Time,data=Theoph),out=tempfile(fileext=".html"))
  html_combine(combine=tempdir(),out="rep1.html")
  
  tf <- readLines(paste0(tempdir(),"/rep1.html"))
  expect_true(any(grepl("img",tf)))
  expect_true(any(grepl("table",tf)))
  
  html_combine(combine=tempdir(),out="rep2.html",toctheme=FALSE, clean=1)
  tf <- readLines(paste0(tempdir(),"/rep1.html"))
  expect_true(!any(grepl("<li>",tf)))
})
