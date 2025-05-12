#library(testthat)
library(R3port)
library(ggplot2)
context("Test ltx related functions")

test_that("ltx_doc function works as expected", {
  options(show=FALSE, compile=FALSE, orientation="portrait", template=system.file("default.tex",package = "R3port"))
  tmplt <- readLines(system.file("default.tex",package = "R3port"))
  res1  <- capture.output(ltx_doc("test",template=paste0(system.file(package = "R3port"),"/default.tex")))

  # There should be a difference at only three places (title, orientation, rrres)
  expect_equal(sum(!res1%in%tmplt),3)
  expect_true(grepl("portrait",res1[grepl("\\{geometry\\}",res1)]))
  expect_true(any(grepl("documentclass\\{article\\}",res1)))
  expect_true(any(grepl("begin\\{document\\}",res1)))
  expect_true(any(grepl("end\\{document\\}",res1)))
  
  ltx_doc("test2", out=paste0(tempdir(),"/subdira/test.tex"),compile=FALSE)
  expect_true(file.exists(paste0(tempdir(),"/subdira/test.tex")))
})

test_that("ltx_plot function works as expected", {
  data(Theoph)
  pl1  <- ggplot(Theoph,aes(Time,conc, group=Subject)) + geom_line()
  pl2  <- pl1 + facet_wrap(~Subject)
  tmfn <- tempfile(fileext=".tex")
  #ltx_plot(list(pl1,pl2),out=tmfn, title="Test Title",titlesub="\\begin{center}dditional info\\end{center}", shorttitle = "short", captpl="bottom")
  ltx_plot(list(pl1,pl2), out = tmfn, title = "Test Title", titlesub = "Additional info",
           shorttitle = "short", compile=FALSE)

  expect_true(file.exists(tmfn))
  expect_true(file.exists(paste0(tools::file_path_sans_ext(tmfn),".tex.rawtex")))
  tf <- readLines(tmfn)
  expect_true(any(grepl("caption\\[[[:alnum:]]*\\]\\{[[:alnum:]| ]*\\}.*footnote",tf)))
  expect_true(file.exists(paste0(tempdir(),"/figures")))
  expect_true(any(grepl("\\.pdf$",list.files(paste0(tempdir(),"/figures")))))

  pl3 <- function() {
    plot(conc~Time,data=Theoph)
    title(main="a plot")
  }
  tmfn2 <- tempfile(fileext=".tex")
  ltx_plot(pl3(), out = tmfn2, compile = FALSE, outfmt = "png", pwidth = 2000, pheight = 1200)
  expect_true(file.exists(tmfn2))
  expect_true(any(grepl("\\.png$",list.files(paste0(tempdir(),"/figures")))))
  
  expect_error(ltx_plot(pl3()))
  options(pwidth=9,pheight=5)
  ltx_plot(pl1, out = tmfn, title = "Test Title", titlesub = "Additional info",
           shorttitle = "short", compile=FALSE, cleancur = TRUE, titlepr = "01")

})


test_that("ltx_list function works as expected", {
  data(Theoph)
  grp  <- c(rep("",3),rep("grouped variables",2))
  tmfn <- tempfile(fileext=".tex")
  ltx_list(Theoph,out=tmfn,vargroup=grp, tabenv = "longtable",
           template=system.file("listing.tex",package="R3port"), compile=FALSE)
  
  tf <- readLines(tmfn)
  expect_true(any(grepl("longtable",tf)))
  
  ltx_list(head(Theoph),out=tmfn,vargroup=grp, tabenv = "tabular",
           template=system.file("listing.tex",package="R3port"), compile=FALSE)
  
  tf <- readLines(tmfn)
  expect_true(any(grepl("tabular",tf)))
})

test_that("ltx_table function works as expected", {
  data(Indometh)
  Indometh$id  <- as.numeric(as.character(Indometh$Subject))
  Indometh$trt <- ifelse(Indometh$id<4,"trt 1","trt 2")
  tmfn         <- tempfile(fileext=".tex")
  
  ltx_table(Indometh,x=c("trt","time"),y="id",var="conc", 
            compile=FALSE, out=tmfn, xabove=TRUE)
  
  tf <- readLines(tmfn)
  expect_true(any(grepl("longtable",tf)))
  expect_true(sum(grepl("multicolumn",tf))>1)
  
  ltx_table(Indometh,x=c("trt","time"),y="id",var="conc", tabenv = "tabular",
            compile=FALSE, out=paste0(tempdir(),"/subdirb/test.tex"), xabove=TRUE, yhead=TRUE)
  
  
  tf <- readLines(paste0(tempdir(),"/subdirb/test.tex"))
  expect_true(any(grepl("tabular",tf)))
  expect_true(any(grepl("multicolumn.*id",tf)))
})


test_that("ltx_combine function works as expected", {
  data(Theoph)
  ltx_list(Theoph[1:11,],out=tempfile(fileext=".tex"), compile=FALSE)
  ltx_plot(plot(conc~Time,data=Theoph),out=tempfile(fileext=".tex"), compile=FALSE)
  ltx_combine(combine=tempdir(),out="rep1.tex", compile=FALSE)
  
  tf <- readLines(paste0(tempdir(),"/rep1.tex"))
  expect_true(any(grepl("includegraphics",tf)))
  expect_true(any(grepl("longtabl",tf)))
  
  ltx_combine(combine=tempdir(),out="rep2.tex", compile=FALSE,
              template=paste0(system.file(package="R3port"),"/beamer.tex"),
              presentation=TRUE)
  
  tf <- readLines(paste0(tempdir(),"/rep2.tex"))
  expect_true(any(grepl("frame",tf)))
})
