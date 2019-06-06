#------------------------------------------ ltx_table ------------------------------------------
#' Creates a latex table
#'
#' This function creates a latex table. The function calls a combination of functions within the
#' R3port package to create an overall table and writes it to a file or console
#'
#' @param dfrm the data frame for which the table should be generated
#' @param x vector of x variable(s) in the data frame
#' @param y vector of y variable(s) in the data frame (will be cast to generate long format)
#' @param var variable within the data frame with the values to be placed in the table
#' @param fill character vector of one indicating the character to use in case of missing values
#' @param uselabel logical indicating if labels should be used for the x variable(s).
#'    If set to TRUE, the function will try to use the label attribute for the display of x variable(s).
#' @param yhead logical indicating if the y variable should also be set as header in the table.
#' @param footnote character string with the footnote to be placed in the footer of the page (LaTeX coding can be used for example to create line breaks)
#' @param tablenote character string with the table note to be placed directly below the table (LaTeX coding can be used for example to create line breaks)
#' @param mancol character string to define manual column alignment. in case argument is NULL, a sensible default will be set.
#' @param size character string to define the font size of the table
#' @param title character string to define the title of the table which will be added to the caption
#' @param titlepr character string to define the prefix of the table title. Can be used to create custom table numbering
#' @param xabove logical indicating if the first unique x variable should be placed in the table row above. Mostly used to save space on a page
#' @param group number indicating which x variables should be grouped (displayed in table with a certain white space) and interpreted as x[1:group]
#' @param xrepeat logical indicating if duplicate x values should be repeated in the table or not
#' @param hyper logical indicating if a hypertarget should be set used for bookmarks
#' @param out filename for the output latex file (if NULL it will print to console)
#' @param rawout character string with the name of the raw latex file to generate (e.g. only table with no preamble and document ending)
#'    In case NULL no raw output will be generated. In order to combine results the filename should end in .rawtex
#' @param convchar logical indicating if special characters should be masked
#' @param tabenv character with the table environment to use. Currently "longtable" and "tabular" are supported
#' @param label character with the label to add after the caption for referencing the table in text
#' @param ... additional arguments passed through to \code{\link{ltx_doc}}. Most important are template, rendlist, compile and show
#'
#' @return The function returns a latex file (or writes output to console)
#'
#' @export
#' @examples
#'
#' \dontrun{
#'   data(Indometh)
#'   Indometh$id  <- as.numeric(as.character(Indometh$Subject))
#'   Indometh$trt <- ifelse(Indometh$id<4,"trt 1","trt 2")
#'
#'   ltx_table(Indometh,x=c("trt","time"),y="id",var="conc",
#'             out=tempfile(fileext=".tex"),xabove=TRUE)
#'
#'   # Usage of multiple y values
#'   ltx_table(Indometh,x="time",y=c("trt","id"),var="conc",
#'             out=tempfile(fileext=".tex"))
#'
#'   # Some examples for different options
#'   ltx_table(Indometh,x=c("time","trt"),y="id",var="conc",
#'             out=tempfile(fileext=".tex"),yhead=TRUE,
#'             group=1,titlepr="TBL01",title="Dummy table",
#'             footnote="this table is not very informative")
#' }
ltx_table <- function(dfrm,x,y,var,fill="",uselabel=TRUE,yhead=FALSE,footnote="",tablenote="",mancol=NULL,size="\\footnotesize",title="table",titlepr=NULL,
                      xabove=FALSE,group=NULL,xrepeat=FALSE,hyper=TRUE,out=NULL,rawout=paste0(out,".rawtex"),convchar=TRUE,tabenv="longtable",label=NULL,...){

  tableprep    <- table_prep(dfrm=dfrm,x=x,y=y,var=var,fill=fill,type="latex",convchar=convchar)
  tabledesign  <- ltx_table_design(tableprep,uselabel=uselabel,yhead=yhead,footnote=footnote,mancol=mancol,size=size,title=title,titlepr=titlepr,
                                   xabove=xabove,group=group,xrepeat=xrepeat,hyper=hyper,tabenv=tabenv,tablenote=tablenote,label=label)
  ltx_doc(tabledesign,out=out,...)
  if(!is.null(rawout) & !dir.exists(dirname(rawout))){
    succ <- try(dir.create(dirname(rawout),showWarnings = FALSE))
    if(!succ) stop("Output folder for raw files cannot be created")
  }
  if(!is.null(rawout)) cat(tabledesign,sep="\n",file=rawout)
}
