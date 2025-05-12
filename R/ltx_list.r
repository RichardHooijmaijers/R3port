#------------------------------------------ ltx_list ------------------------------------------
#' Creates a latex listing
#'
#' This function creates a latex listing which can be written to a file or console
#'
#' @param dfrm the data frame to be prepared
#' @param vars character vector that defines the variables within the data frame to be placed in the listing
#' @param fill `r lifecycle::badge("deprecated")` character vector of one indicating the character to use in case of missing values this is only applicable for [ltx_table]
#' @param vargroup a vector of the same length as vars. Creates a first line in the table to group variables (see details)
#' @param porder logical indicating if the data frame should be ordered on the variables given in vars
#' @param uselabel logical indicating if labels should be used for the x variable(s).
#'    If set to TRUE, the function will try to use the label attribute for the display of x variable(s).
#' @param footnote character string with the footnote to be placed in the footer of the page (LaTeX coding can be used for example to create line breaks)
#' @param tablenote character string with the table note to be placed directly below the table (LaTeX coding can be used for example to create line breaks)
#' @param mancol character string to define manual column alignment. in case argument is NULL, a sensible default will be set.
#' @param size character string to define the font size of the table
#' @param title character string to define the title of the table which will be added to the caption
#' @param titlepr character string to define the prefix of the table title. Can be used to create custom table numbering
#' @param group number indicating which x variables should be grouped (displayed in table with a certain white space) and interpreted as `x[1:group]`
#' @param xrepeat logical indicating if duplicate x values should be repeated in the table or not
#' @param hyper logical indicating if a hypertarget should be set used for bookmarks
#' @param out filename for the output latex file (if NULL it will print to console)
#' @param rawout character string with the name of the raw latex file to generate (e.g. only listing with no preamble and document ending)
#'    In case NULL no raw output will be generated. In order to combine results the filename should end in .rawtex
#' @param convchar logical indicating if special characters should be masked
#' @param tabenv character with the table environment to use. Currently "longtable" and "tabular" are supported
#' @param label character with the label to add after the caption for referencing the table in text
#' @param flt character with the type of floating environment to use (only applicable for tabular environment)
#' @param ... additional arguments passed through to [ltx_doc()]. Most important are template, rendlist, compile and show
#'
#' @details The vargroup argument should be provided in the following form: \cr
#'   `c(rep("",4),rep("group1",3),rep("group2",4))`.
#'   The function will place the text within the vector with the given length as first line in the table with a midrule below it.
#'   an exception is made for empty strings.
#'
#' @return The function returns a latex file (or writes output to console)
#'
#' @export
#' @examples
#'
#' \dontrun{
#'   data(Theoph)
#'   grp <- c(rep("",3),rep("grouped variables",2))
#'   ltx_list(Theoph,out=tempfile(fileext=".tex"),vargroup=grp,
#'            template=paste0(system.file(package="R3port"),"/listing.tex"))
#' }
ltx_list <- function(dfrm,vars=names(dfrm),fill="",vargroup=NULL,porder=TRUE,uselabel=TRUE,footnote="",tablenote="",mancol=NULL,size="\\footnotesize",
                   title="listing",titlepr=NULL,group=NULL,xrepeat=FALSE,hyper=TRUE,out=NULL,rawout=paste0(out,".rawtex"),convchar=TRUE,tabenv="longtable",
                   label=NULL,flt="h",...){

  # Create pre-table attributes
  tbld  <- dfrm[,c(vars)]
  if(porder) tbld  <- tbld[do.call("order", do.call("list",tbld[,vars,drop=FALSE])),]
  if(convchar){
    tbld[]  <- apply(tbld,2,function(x) gsub('([<>])', '$\\1$',gsub('([#$%&_\\^\\\\{}])', '\\\\\\1', as.character(x), perl = TRUE), perl = TRUE))
  }else{
    tbld[]  <- apply(tbld,2,as.character)
  }

  tbl <- NULL
  if(hyper & !is.null(titlepr)) tbl <- c(tbl,paste0("\\hypertarget{",title,"}{} \\bookmark[dest=",title,",level=0]{",titlepr,": ",title,"}"))
  if(hyper & is.null(titlepr))  tbl <- c(tbl,paste0("\\hypertarget{",title,"}{} \\bookmark[dest=",title,",level=0]{",title,"}"))
  if(!is.null(titlepr))  tbl <- c(tbl,paste0("\\renewcommand{\\tablename}{} \\renewcommand\\thetable{{",titlepr,"}}"))
  if(footnote!="") tbl <- c(tbl,paste0("\\lfoot{\\footnotesize ",footnote,"}"))

  coldef  <- ifelse(is.null(mancol),paste(rep("l",ncol(tbld)),collapse=""),mancol)
  labdef  <- ifelse(is.null(label),"",paste0("\\label{",label,"}"))
  if(tabenv=="longtable") tbl <- c(tbl,paste0("\\begingroup",size,"\\begin{longtable}{",coldef,"}\n\\caption{",title,"}",labdef,"\\\\"))
  if(tabenv=="tabular")   tbl <- c(tbl,paste0("\\begin{table}[",flt,"]\n\\caption{",title,"}",labdef," ",size,"\\begin{tabular}{",coldef,"}\n"))

  # Create header
  hdr <- NULL
  if(!is.null(vargroup)){
    # Could not use as.numeric(as.factor()) here because every increment should be unique
    un         <- 1
    for(i in seq_along(vargroup)[-1]) if(vargroup[i]==vargroup[i-1]) {un[i] <- un[i-1]}else{un[i] <- un[i-1] + 1}
    vargr      <- plyr::ddply(data.frame(vargroup,un),c("un","vargroup"),nrow)
    vargr$sstr <- (cumsum(vargr$V1)-vargr$V1) + 1
    vargr$sstp <- (cumsum(vargr$V1)-vargr$V1) + 1 + vargr$V1 - 1
    hdr <- c(hdr,paste(paste("\\multicolumn{",vargr$V1,"}{c}{",vargr$vargr,"}",collapse="&",sep=""),"\\\\"))
    hdr <- c(hdr,paste("\\cmidrule(lr){",vargr$sstr[which(vargr$vargroup!="")],"-",vargr$sstp[which(vargr$vargroup!="")],"}",collapse=" ",sep=""))
  }

  lb <- vars
  if(uselabel) lb <- sapply(lb,function(lbls) ifelse(is.null(attr(dfrm[,lbls],'label')),lbls,attr(dfrm[,lbls],'label')))
  if(convchar) lb <- gsub('([#$%&_\\^\\\\{}])', '\\\\\\1',lb, perl = TRUE)
  hdr <- c(hdr,paste0(paste(lb,collapse= " & "),"\\\\"))
  hdr <- c(hdr,"\\hline")
  if(tabenv=="longtable"){
    tbl <- c(tbl,"\\toprule",hdr,"\\endfirsthead")
    tbl <- c(tbl,paste0("\\multicolumn{",ncol(tbld),"}{c}{\\tablename~\\thetable{}: (continued)}\\\\\\\\"))
    tbl <- c(tbl,"\\toprule",hdr,"\\endhead \\hline \\endfoot \\hline","\\endlastfoot")
  }else{
    tbl <- c(tbl,"\\hline",hdr)
  }

  # Add data and close off
  if(!is.null(group)) dup <- !duplicated(tbld[,1:group,drop=FALSE],fromLast=TRUE)
  if(!xrepeat){
    duplst <- plyr::llply(1:length(vars),function(coln){duplicated(do.call("paste",tbld[,1:coln,drop=FALSE]))})
    plyr::l_ply(1:length(duplst),function(coln){tbld[unlist(duplst[coln]),coln] <<- ""})
  }
  dtal <- plyr::llply(1:nrow(tbld),function(num){
    dta <- paste(paste(tbld[num,], collapse= " & "),"\\\\")
    if(!is.null(group)){if(dup[num]==TRUE) dta <- c(dta,"[2ex]")}
    return(dta)
  })
  tbl <- c(tbl,unlist(dtal))
  if(tabenv=="longtable") {
    tbl <- c(tbl,"\\end{longtable}",tablenote,"\\endgroup")
  }else{
    tbl <- c(tbl,"\\hline\\end{tabular}\\\\",tablenote,"\\end{table}")
  }

  if(!is.null(rawout) & !dir.exists(dirname(rawout))){
    succ <- try(dir.create(dirname(rawout),showWarnings = FALSE))
    if(!succ) stop("Output folder for raw files cannot be created")
  }
  if(!is.null(rawout)) cat(tbl,sep="\n",file=rawout)
  ltx_doc(text=tbl,out=out,...)
}
