#------------------------------------------ html_combine ------------------------------------------
#' Combines multiple HTML files to a single tex and compiles document
#'
#' This function combines multiple HTML files. This is done based on the name of the files and should
#' end with raw.tex to make the function pick-up these files.
#'
#' @param combine character string with the location of the raw html files or list with file names within same directory
#' @param out filename for the output HTML file (if NULL it will print to console)
#' @param toctheme logical indicating if the created file should also have a toc (take into account for template)
#' @param css character with name of the css style sheet to use, default use package style sheet
#' @param clean integer between 0 and 2 indicating if all individual files should be kept (0), all individual raw html files should
#'    be deleted (1) or all individual files should be deleted (2)
#' @param ... additional arguments passed through to \code{\link{html_doc}}. Most important are template, rendlist, css and show
#'
#' @details Currently the generated output is saved in the same place where the separate tables and plots are located
#'   defined in the 'combine' argument. This is done even when a different file path is specified in 'out' (using the basename function).
#'   The reason is to not copy files linked within the document and preventing broken links. This behaviour might change in future releases
#'
#' @return The function returns a HTML file (or writes output to console)
#'
#' @export
#' @examples
#'
#' # Take into account the usage of tempfile() with multiple function calls
#' data(Theoph)
#' html_list(Theoph[1:11,],out=tempfile(fileext=".html"),show=FALSE)
#' html_plot(plot(conc~Time,data=Theoph),out=tempfile(fileext=".html"),show=FALSE)
#' \dontrun{
#'   html_combine(combine=tempdir(),out="rep1.html")
#'
#'   # toctheme can be used to have a clickable toc,
#'   # a bootstrap template for this is provided in the package
#'   html_combine(combine=tempdir(),out="rep1.html",
#'               template=paste0(system.file(package="R3port"),"/bootstrap.html"),
#'               toctheme=TRUE)
#'  }

html_combine <- function(combine=".",out=NULL,toctheme=TRUE,css=paste0(system.file(package="R3port"),"/style.css"),clean=0,...){

  if(class(combine)!="list"){rt <- list.files(combine,"\\.rawhtml$",full.names=TRUE)}else{rt <- unlist(combine)}

  # For now assume that output is always saved in (one) 'location' (to prevent broken links for figures)
  location <- dirname(rt)[1]
  if(is.na(location)) stop("no raw html files to combine")
  if(!is.null(out)) out <- paste(location,basename(out),sep="/")

  # For the toctheme place results in div and create unordered list for toc
  if(toctheme){
    rtl <- plyr::llply(1:length(rt),function(x){
      r <- readLines(rt[x])
      r <- c(paste0("<div id='out",x,"d' class='showhide'>"),r,'</div>')
    })
    titl  <- plyr::laply(rtl,function(x) x[grep("<h1>.*</h1>",x)])
    titl  <- gsub("<h1>|</h1>","",titl)
    ids1  <- paste0("out",1:length(rt))
    toc   <- paste(paste0("<li ><a id='",ids1,"' href='#'>",titl,"</a></li>"),collapse='')
  }else{
    rtl <- plyr::llply(rt,function(x){r <- readLines(x); r <- c(r,"<br/>")})
  }
  # The logics for the rendlist
  if(!is.null(out) & length(grep("https:|http:",css))==0){
    file.copy(from=css,to=dirname(out),overwrite=TRUE)
  }
  css <- ifelse(grepl("https:|http:",css),css,basename(css))
  if(!methods::hasArg(rendlist)){
    rendlist <- list(css=css,rrres=paste(unlist(rtl),collapse="\n"))
  }else{
    rendlist <- c(rendlist,css=css,rrres=paste(unlist(rtl),collapse="\n"))
  }
  if(toctheme) rendlist <- c(rendlist,rrtoc=toc)
  if(clean==1) file.remove(list.files(location,"\\.rawhtml$",full.names=TRUE))
  if(clean==2) file.remove(list.files(location,"\\.rawhtml$|\\.html$",full.names=TRUE))

  html_doc(rtl,out=out,rendlist=rendlist,...)
}
