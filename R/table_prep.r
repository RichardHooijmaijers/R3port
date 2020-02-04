#------------------------------------------ table_prep ------------------------------------------
#' Prepares the data for pivotal tabulation
#'
#' This function prepares the data pivotal for tabulation. It is intended to use as a first step
#' in the creation of a report pivotal table. It is not likely that this function will be used as stand-alone
#'
#' @param dfrm the data frame for which the table should be generated
#' @param x vector of x variable(s) in the data frame
#' @param y vector of y variable(s) in the data frame (will be cast to generate long format)
#' @param var variable within the data frame with the values to be placed in the table
#' @param fill character vector of one indicating the character to use in case of missing values
#' @param type character vector of one indicating the type of table to generate, see details
#' @param convchar logical indicating if special characters should be masked
#'
#' @details This function only prepares the data for pivotal tabulation. The base of the function is the dcast
#'   function from plyr to reshape the data and the generation of a table header object to indicate how headers
#'   should be placed within the table. The function also specifies a type argument which includes "latex" or "html",
#'   this is necessary to identify specific character handling (convchar) that differs between HTML and LaTeX.
#' @return The function returns a list with the original dataframe, the table header, table data and other table specifications
#'
#' @export
#' @examples
#'
#' data(Indometh)
#' table_prep(Indometh,"time","Subject","conc")

table_prep <- function(dfrm,x,y,var,fill="",type="latex",convchar=TRUE){

  # generate analysis data at last step make characters for entire table (paste() will show factor levels)
  tbld  <- dfrm[,c(x,y,var)]
  tbld  <- tbld[do.call("order", do.call("list",tbld[,y,drop=FALSE])),]
  form  <- paste(paste0(x,collapse="+"),"~",paste0(y,collapse="+"))
  tbldt <- reshape2::dcast(tbld,stats::as.formula(form),value.var=var,fill=fill)
  if(type=="latex" & convchar==TRUE) {for(i in 1:ncol(tbldt)) tbldt[,i] <- gsub('([#$%&_\\^\\\\{}])', '\\\\\\1', as.character(tbldt[,i]), perl = TRUE)}
  if(type=="html" | convchar!=TRUE)  {for(i in 1:ncol(tbldt)) tbldt[,i] <- as.character(tbldt[,i])}

  # generate table header
  tblh <- tbld[!duplicated(do.call("paste",do.call("list",tbld[,y,drop=FALSE]))),y,drop=FALSE]
  names(tblh) <- paste0("y",1:length(y))
  plyr::l_ply(1:ncol(tblh),function(num){
    hdr        <- plyr::ddply(tblh,paste0("y",1:num),nrow)
    hdr$sstr   <- (cumsum(hdr$V1)-hdr$V1) + 1 + length(x)
    hdr$sstp   <- (cumsum(hdr$V1)-hdr$V1) + 1 + length(x) + hdr$V1 - 1
    names(hdr) <- c(paste0("y",1:num),paste0(c("yn","ystr","ystp"),num))
    tblh       <- merge(tblh,hdr,by=paste0("y",1:num))
    assign("tblh",tblh,envir=parent.env(environment()))
  })

  # Generate other information
  tblo <- list('x'=x,'y'=y,'var'=var)

  # return list with data to print and information regarding headers
  tblh       <- tblh[do.call("order", do.call("list",tblh[,paste0("y",1:length(y)),drop=FALSE])),]
  return(list(odata=dfrm,tbld=tbldt,tblh=tblh,tblo=tblo))
}
