#library(testthat)
library(R3port)
context("doc functions")

test_that("Check doc functions", {
  tmplt <- readLines(paste0(system.file(package = "R3port"),"/default.tex"))
  res1  <- capture.output(ltx_doc("test",template=paste0(system.file(package = "R3port"),"/default.tex")))
  
  # There should be a difference at only three places (title, orientation, rrres)
  expect_equal(sum(!res1%in%tmplt),3)
  expect_true(grepl("landscape",res1[grepl("\\{geometry\\}",res1)]))
  expect_true(any(grepl("documentclass\\{article\\}",res1)))
  expect_true(any(grepl("begin\\{document\\}",res1)))
  expect_true(any(grepl("end\\{document\\}",res1)))
  
  tmplh <- readLines(paste0(system.file(package = "R3port"),"/bootstrap.html"))
  res2  <- capture.output(html_doc("test",template=paste0(system.file(package = "R3port"),"/bootstrap.html")))
  
  expect_equal(length(tmplh),length(res2))
  expect_true(all(grepl("report",res2[grepl("\\{\\{\\{rtitle",tmplh)])))
  expect_true(grepl("style\\.css",res2[grepl("\\{\\{css",tmplh)]))
})

