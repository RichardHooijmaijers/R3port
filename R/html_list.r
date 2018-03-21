#------------------------------------------ html_list ------------------------------------------
#' Creates a HTML listing
#'
#' This function creates a HTML listing which can be written to a file or console
#'
#' @param dfrm data frame to be listed
#' @param vars character vector that defines the variables within the data frame to be placed in the table
#' @param fill character indicating the character to use in case of missing values
#' @param vargroup character vector of the same length as vars. Creates a first line in the table to group variables (see details)
#' @param porder logical indicating if the data frame should be ordered on the variables given in vars
#' @param uselabel logical indicating if labels should be used for the variable(s).
#'    If set to TRUE, the function will try to use the label attribute for the display of variable(s).
#' @param footnote character string with the footnote to be placed in the footer of the page (HTML coding can be used for example to create line breaks)
#' @param title character string to define the title of the output which will be added to the caption
#' @param titlepr character string to define the prefix of the output title. Can be used to create custom table numbering
#' @param group numeric indicating the last index of the used variables that should be grouped (displayed in table with a certain white space), interpreted as x[1:group]
#' @param xrepeat logical indicating if duplicate x values should be repeated in the table or not
#' @param tclass character string with the table class. Can be used in combination with custom css
#' @param out filename for the output HTML file (if NULL it will print to console)
#' @param rawout character string with the name of the raw HTML file to generate (e.g. only table)
#'    In case NULL no raw output will be generated. In order to combine results the filename should end in .rawhtml
#' @param ... additional arguments passed through to \code{\link{html_doc}}. Most important are template, rendlist, css and show
#'
#' @details The vargroup argument should be provided in the following form: \cr
#'   \code{c(rep("",4),rep("group1",3),rep("group2",4))}.
#'   The function will place the text within the vector with the given length as first line in the table with a midrule below it.
#'   an exception is made for empty strings. The format of the current listing/css was designed to look good in the browser and in a WORD
#'   document (when html is opened in WORD). Additional colspans are added in the table header which might lead to additional space in case
#'   a user defined css file is used.
#'
#' @return The function returns a HTML file (or writes output to console)
#'
#' @export
#' @examples
#'
#' # an example how vargroup can be used
#' grp <- c(rep("",3),rep("grouped variables",2))
#' \dontrun{
#'   data(Theoph)
#'   html_list(Theoph,out=tempfile(fileext=".html"))
#'   html_list(Theoph,out=tempfile(fileext=".html"),vargroup=grp)
#' }
html_list <- function(dfrm,vars=names(dfrm),fill="",vargroup=NULL,porder=TRUE,uselabel=TRUE,footnote=NULL,title="listing",
                      titlepr=NULL,group=NULL,xrepeat=FALSE,tclass="sample",out=NULL,rawout=paste0(out,".rawhtml"),...){

  # Create pre-table attributes
  tbld  <- dfrm[,c(vars)]
  if(porder) tbld  <- tbld[do.call("order", do.call("list",tbld[,vars,drop=FALSE])),]
  for(i in 1:ncol(tbld)) tbld[,i] <- as.character(tbld[,i])

  tbl <- NULL
  tbl <- c(tbl,paste0("<h1>",titlepr,title,"</h1>"))
  tbl <- c(tbl,paste0("<table class='",tclass,"'>"))

  # Create header
  hdr <- NULL
  # this should be made easier
  if(!is.null(vargroup)){
    # Could not use as.numeric(as.factor()) here because every increment should be unique
    un         <- 1
    for(i in seq_along(vargroup)[-1]) if(vargroup[i]==vargroup[i-1]) {un[i] <- un[i-1]}else{un[i] <- un[i-1] + 1}
    vargr      <- plyr::ddply(data.frame(vargroup,un),c("un","vargroup"),nrow)
    vargr$sstr <- (cumsum(vargr$V1)-vargr$V1) + 1
    vargr$sstp <- (cumsum(vargr$V1)-vargr$V1) + 1 + vargr$V1 - 1
    vargr$hdr  <- ifelse(vargr$vargroup=="",paste0("<td colspan='",vargr$V1,"'>",vargr$vargr,"</td>"),paste0("<td id='fh' colspan='",vargr$V1,"'>",vargr$vargr,"</td>"))
    hdr        <- c(hdr,paste("<tr><td colspan='",sum(vargr$V1),"'></td></tr>"))
    hdr        <- c(hdr,paste(vargr$hdr,collapse=""))
    hdr        <- c(hdr,paste("<tr><td colspan='",sum(vargr$V1),"'></td></tr>"))
  }
  lb <- vars
  if(uselabel) lb <- sapply(lb,function(lbls) ifelse(is.null(attr(dfrm[,lbls],'label')),lbls,attr(dfrm[,lbls],'label')))
  hdr <- c(hdr,paste0("</tr>",paste("<td id='lh'>",lb,"</td>",collapse=""),"</tr>"))
  tbl <- c(tbl,hdr)

  # Add data and close off
  if(!is.null(group)) dup <- !duplicated(tbld[,1:group,drop=FALSE],fromLast=TRUE)
  if(!xrepeat){
    duplst <- plyr::llply(1:length(vars),function(coln){duplicated(do.call("paste",tbld[,1:coln,drop=FALSE]))})
    plyr::l_ply(1:length(duplst),function(coln){tbld[unlist(duplst[coln]),coln] <<- ""})
  }
  dtal <- plyr::llply(1:nrow(tbld),function(num){
    dta <- paste0(paste("<td>",tbld[num,],"</td>",collapse=""),"</tr>")
    if(!is.null(group) && dup[num]==TRUE) dta <- c(dta,paste0("<tr><td id='grps' colspan='",ncol(tbld),"'></td></tr>"))
    return(dta)
  })
  tbl <- c(tbl,unlist(dtal),"</table>")
  if(!is.null(footnote)) tbl <- c(tbl,footnote)

  html_doc(tbl,out=out,...)
  if(!is.null(rawout) & !dir.exists(dirname(rawout))){
    succ <- try(dir.create(dirname(rawout),showWarnings = FALSE))
    if(!succ) stop("Output folder for raw files cannot be created")
  }
  if(!is.null(rawout)) cat(tbl,sep="\n",file=rawout)
}
