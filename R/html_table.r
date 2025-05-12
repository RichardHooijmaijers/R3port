#------------------------------------------ html_ltable ------------------------------------------
#' Creates a HTML table
#'
#' This function creates a HTML table. The function calls a combination of functions within the
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
#' @param footnote character string with the footnote to be placed in the footer of the page (HTML coding can be used for example to create line breaks)
#' @param title character string to define the title of the table which will be added to the caption
#' @param titlepr character string to define the prefix of the table title. Can be used to create custom table numbering
#' @param xabove logical indicating if the first unique x variable should be placed in the table row above. Mostly used to save space on a page
#' @param group number indicating which x variables should be grouped (displayed in table with a certain white space) and interpreted as `x[1:group]`
#' @param xrepeat logical indicating if duplicate x values should be repeated in the table or not
#' @param tclass character string with the table class. Can be used in combination with custom css
#' @param out filename for the output HTML file (if NULL it will print to console)
#' @param rawout character string with the name of the raw HTML file to generate (e.g. only table with)
#'    In case NULL no raw output will be generated. In order to combine results the filename should end in .raw.html
#' @param ... additional arguments passed through to [html_doc()]. Most important are template, rendlist, css and show
#'
#' @details The format of the current table/css was designed to look good in the browser and in a WORD
#'   document (when html is opened in WORD). Additional colspans are added in the table header which might lead to additional space in case
#'   a user defined css file is used.
#'
#' @return The function returns a HTML file (or writes output to console)
#'
#' @export
#' @examples
#'
#' \dontrun{
#'   data(Indometh)
#'   Indometh$id  <- as.numeric(as.character(Indometh$Subject))
#'   Indometh$trt <- ifelse(Indometh$id<4,"trt 1","trt 2")
#'
#'   html_table(Indometh,x=c("trt","time"),y="id",var="conc",
#'             out=tempfile(fileext=".html"),xabove=TRUE)
#'
#'   # Usage of multiple y values
#'   html_table(Indometh,x="time",y=c("trt","id"),var="conc",
#'             out=tempfile(fileext=".html"))
#'
#'   # Some examples for different options
#'   html_table(Indometh,x=c("time","trt"),y="id",var="conc",
#'             out=tempfile(fileext=".html"),yhead=TRUE,
#'             group=1,titlepr="TBL01",title="Dummy table",
#'             footnote="this table is not very informative")
#' }
html_table <- function(dfrm,x,y,var,fill="",uselabel=TRUE,yhead=FALSE,footnote=NULL,title="table",titlepr=NULL,xabove=FALSE,group=NULL,
                      xrepeat=FALSE,tclass="sample",out=NULL,rawout=paste0(out,".rawhtml"),...){

  tableprep    <- table_prep(dfrm=dfrm,x=x,y=y,var=var,fill=fill,type="HTML")
  tabledesign  <- html_table_design(tableprep,uselabel=uselabel,yhead=yhead,footnote=footnote,title=title,titlepr=titlepr,xabove=xabove,group=group,xrepeat=xrepeat,tclass=tclass)
  html_doc(tabledesign,out=out,...)
  if(!is.null(rawout) & !dir.exists(dirname(rawout))){
    succ <- try(dir.create(dirname(rawout),showWarnings = FALSE))
    if(!succ) stop("Output folder for raw files cannot be created")
  }
  if(!is.null(rawout)) cat(tabledesign,sep="\n",file=rawout)
}
