#------------------------------------------ ltx_doc ------------------------------------------
#' Prints latex code for a table, listing, plot, or text to a a file or console
#'
#' This function makes a latex document using output generated with functions in the R3port package
#' or any other latex code available as vector. Basically it adds a preamble to a tex file and let's
#' the user select various options to customize the output.
#'
#' @param text character vector to be placed within latex document
#' @param out filename for the output latex file (if NULL it will print to console)
#' @param template file name of the template file to use (see examples how templates can be used/adapted)
#' @param rendlist a render list to be used for the template file (see (see \code{\link{whisker-package}}))
#' @param orientation string indicating the page orientation (can be either "landscape" or "portrait")
#' @param rtitle string indicating the title of the output document
#' @param compile logical indicating if the tex file should be compiled (using tools::texi2dvi)
#' @param show logical indicating if the resulting pdf file from the compiled tex file should be opened when created
#'
#' @return The function returns a latex file (or writes output to console)
#' @seealso \code{\link{html_doc}}
#'
#' @export
#' @examples
#'
#' \dontrun{
#'   txt <- "\\section{example}"
#'   tbl <- "\\begin{tabular}{|l|c|r|} 1 & 2 & 3 \\\\ 4 & 5 & 6 \\\\ \\end{tabular}"
#'   add <- "\\\\ Including some additional text"
#'   ltx_doc(c(txt,tbl,add),out=paste0(tempfile(),".tex"),show=FALSE)
#'
#'   # You can use xtable (and any other packages that output tex)
#'   library(xtable)
#'   data(Theoph)
#'   xtbl <- print(xtable(Theoph),tabular.environment="longtable",floating=FALSE,print.results=FALSE)
#'   ltx_doc(xtbl,out=tempfile(fileext = ".tex"))
#'
#' }

ltx_doc <- function(text,out=NULL,template=paste0(system.file(package="R3port"),"/simple.tex"),rendlist,orientation="landscape",rtitle="report",compile=TRUE,show=TRUE){
  if(!is.null(out) && !dir.exists(dirname(out))){
    succ <- try(dir.create(dirname(out),showWarnings = FALSE))
    if(!succ) stop("Output folder cannot be created")
  }

  # Set logics for option system
  if(!is.null(getOption('show')))        show         <- getOption('show')
  if(!is.null(getOption('template')))    template     <- getOption('template')
  if(!is.null(getOption('orientation'))) orientation  <- getOption('orientation')
  if(!is.null(getOption('compile')))     compile      <- getOption('compile')

  # Create render list (if not available) and read in template file for processing with whisker
  # Take into account that a default template should have the items in rendlist or else a user
  # defined rendlist should be provided
  lout <- readLines(template)

  # Process results
  # Take into account that rrres can be quite the string
  if(missing(rendlist)){
    rendlist <- list(orientation=orientation,rtitle=rtitle,rrres=paste(text,collapse="\n"))
  }else{
    rendlist <- c(rendlist,orientation=orientation,rtitle=rtitle,rrres=paste(text,collapse="\n"))
  }
  if(is.null(out)) out <- stdout()
  writeLines(whisker::whisker.render(lout, rendlist), out)

  # compile and show document
  curwd <- getwd()
  if(out!=stdout()){
    try(setwd(dirname(out)))
    out <- basename(out)
  }
  if(compile & out!=stdout()){
    # compilation on linux with space in path is not handled well by texi2dvi
    if(Sys.info()['sysname']=="Linux" && grepl(" ",getwd())){
      try(system(paste("pdflatex",out),ignore.stdout=TRUE))
      try(system(paste("pdflatex",out),ignore.stdout=TRUE))
    }else{
      try(tools::texi2dvi(out,pdf=TRUE,clean=TRUE))
    }
  }
  if(show & out!=stdout()){
    if(Sys.info()['sysname']=="Darwin"){
      try(system(paste0("open \"",sub("\\.tex$","\\.pdf",out),"\""),wait=FALSE))
    }else if(Sys.info()['sysname']=="Linux"){
      try(system(paste0("xdg-open '",sub("\\.tex$","\\.pdf",out),"'")))
    }else if(Sys.info()['sysname']=="Windows"){
      try(shell(paste0("\"",sub("\\.tex$","\\.pdf",out),"\""),wait=FALSE))
    }
  }
  setwd(curwd)
}
