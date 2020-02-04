#------------------------------------------ ltx_combine ------------------------------------------
#' Combines multiple latex files to a single tex and compiles document
#'
#' This function combines multiple latex files. This is done based on the name of the files and should
#' end with raw.tex to make the function pick-up these files.
#'
#' @param combine character string with the location of the raw tex files or list with file names within same directory
#' @param out filename for the output latex file (if an empty string is provided it will print to console)
#' @param presentation logical indicating if the output is a latex presentation (in this case the results will be placed
#'    within frames and without captions and clearpage)
#' @param clean integer between 0 and 2 indicating if all individual files should be kept (0), all individual tex and raw tex files should
#'    be deleted (1) or all individual files should be deleted (2)
#' @param ... additional arguments passed through to \code{\link{ltx_doc}}. Most important are template, rendlist, compile and show
#'
#' @details Currently the generated output is saved in the same place where the separate tables and plots are located
#'   defined in the 'combine' argument. This is done even when a different file path is specified in 'out' (using the basename function).
#'   The reason is to not copy files linked within the document and preventing broken links. This behaviour might change in future releases
#'
#' @return The function returns a latex file (or writes output to console)
#'
#' @export
#' @examples
#'
#' # Take into account the usage of tempfile() with multiple function calls
#' data(Theoph)
#' ltx_list(Theoph[1:11,],out=tempfile(fileext=".tex"),show=FALSE)
#' ltx_plot(plot(conc~Time,data=Theoph),out=tempfile(fileext=".tex"),show=FALSE)
#' \dontrun{
#'   ltx_combine(combine=tempdir(),out="rep1.tex")
#'
#'   # possibility for presentation layout (beamer template provided in package)
#'   ltx_combine(combine=tempdir(),out="rep1.tex",
#'               template=paste0(system.file(package="R3port"),"/beamer.tex"),
#'               presentation=TRUE)
#'
#'   # Or other template with different orientation
#'   ltx_combine(combine=tempdir(),out="rep1.tex",
#'               template=paste0(system.file(package="R3port"),"/listing.tex"),
#'               orientation="portrait")
#' }
ltx_combine <- function(combine=".",out=NULL,presentation=FALSE,clean=0,...){

  # list files, read and combine tex files and compile
  if(class(combine)!="list"){rt <- list.files(combine,"\\.rawtex$",full.names=TRUE)}else{rt <- unlist(combine)}
  location <- dirname(rt)[1]

  # remove log and aux files in directory
  file.remove(list.files(location,".aux$",full.names=TRUE))
  file.remove(list.files(location,".log$",full.names=TRUE))

  # For now assume that output is always saved in (one) 'location' (to prevent broken links for figures)
  if(!is.null(out)) out <- paste(location,basename(out),sep="/")

  if(presentation){
    rtl <- unlist(plyr::llply(rt,function(x){r <- readLines(x); r <- c("\\begin{frame}[fragile,allowframebreaks]",r,"\\end{frame}")}))
    rtl <- gsub("\\\\caption\\{.*\\}|\\\\caption\\[\\]\\{.*\\}","%\\\\caption\\{\\}",rtl)
    rtl <- gsub("\\\\end\\{figure\\}\\\\clearpage","\\\\end\\{figure\\}",rtl)
  }else{
    rtl <- plyr::llply(rt,function(x){r <- readLines(x); r <- c(r,"\\newpage")})
    rtl <- unlist(rtl)
  }
  if(clean==1) file.remove(list.files(location,"\\.tex$|\\.rawtex$",full.names=TRUE))
  if(clean==2) file.remove(list.files(location,"\\.tex$|\\.rawtex$|\\.pdf$",full.names=TRUE))
  ltx_doc(rtl,out=out,...)
}
