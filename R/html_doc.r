#------------------------------------------ html_doc ------------------------------------------
#' Prints a HTML table, listing or plot to a a file or console
#'
#' This function makes a HTML document using output generated with functions in the R3port package
#' or any other HTML code available as vector. Basically it adds tags to a html template file and let's
#' the user select various options to customize the output.
#'
#' @param text character vector to be placed within HTML document
#' @param out character with filename for the output HTML file (if NULL it will print to console)
#' @param show logical indicating if the resulting pdf file from the compiled tex file should be opened when created
#' @param rtitle string indicating the title of the output document
#' @param template character with file name of the template file to use
#' @param rendlist list with render items to be used for the template file (see [whisker::whisker.render])
#' @param css character with name of the css style sheet to use, default use package style sheet
#'
#' @details This function is used as wrapper within multiple functions in the R3port package but is also convenient in case
#'   custom information should be placed within an output document
#'
#' @return The function returns a HTML file (or writes output to console)
#' @seealso [ltx_doc()]
#'
#' @export
#' @examples
#'
#' txt <- "<h1>Example</h1>"
#' tbl <- "<table><tr><td id='fcol'>table data</td><td>for custom table</td></tr></table>"
#' add <- "<p>Including some additional text</p>"
#' \dontrun{
#'   html_doc(c(txt,tbl,add),out=tempfile(fileext=".html"))
#' }
html_doc <- function(text,out=NULL,show=TRUE,rtitle="report",template=paste0(system.file(package="R3port"),"/simple.html"),rendlist,css=paste0(system.file(package="R3port"),"/style.css")){
  if(!is.null(out) && !dir.exists(dirname(out))){
    succ <- try(dir.create(dirname(out),showWarnings = FALSE))
    if(!succ) stop("Output folder cannot be created")
  }

  # Set logics for option system
  if(!is.null(getOption('show')))     show     <- getOption('show')
  if(!is.null(getOption('template'))) template <- getOption('template')
  if(!is.null(getOption('css')))      css      <- getOption('css')

  if(!is.null(out) & length(grep("https:|http:",css))==0){
    file.copy(from=css,to=dirname(out),overwrite=TRUE)
  }
  css <- ifelse(grepl("https:|http:",css),css,basename(css))
  # Create render list (if not available) and read in template file for processing with whisker
  # Take into account that a default template should have the items in rendlist or else a user
  # defined rendlist should be provided
  # Take into account that rrres can be quite the string
  if(missing(rendlist)){
    rendlist <- list(css=css,rtitle=rtitle,rrres=paste(text,collapse="\n"))
  }else{
    rendlist <- c(rendlist,css=css,rtitle=rtitle,rrres=paste(text,collapse="\n"))
  }
  hout <- readLines(template)

  # Process results
  if(is.null(out)) out <- stdout()
  writeLines(whisker::whisker.render(hout, rendlist), out)

  if(out!=stdout() && show){
    if(Sys.info()['sysname']=="Darwin"){
      try(system(paste0("open '",normalizePath(out),"'"),wait=FALSE))
    }else{
      utils::browseURL(paste0("file://",normalizePath(out)))
    }
  }
}
