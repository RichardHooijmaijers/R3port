#------------------------------------------ means ------------------------------------------
#' Calculate summary statistics on a data frame
#'
#' This function calculates summary statistics on a data frame. It is intended to use to
#' calculate a standard number of descriptive statistics which are defined in the packs argument
#'
#' @param dfrm the data frame to calculate the statistics on
#' @param variable character with the variable within the data frame for which the statistics should be calculated
#' @param by character vector of variable(s) within the data frame to stratify the statistics on
#' @param total character vector of variable(s) within the data frame to calculate totals on
#' @param pack numeric indicating number of the pack or name of the function with different descriptive statistics (see details)
#' @param dig the number of digits to use for output statistics (except for N which is always displayed as integer)
#' @param alpha the alpha in case the statistics pack calculates confidence limits
#'
#' @details The function calculates multiple statistics of a data frame that can be stratified. Furthermore, the statistics for a total
#'   can also be generated. Currently the function has the possibility to calculate 3 different sets of statistics which are commonly
#'   used within the field of clinical data analysis:
#'   \enumerate{
#'     \item N, Mean, Median, SD, Min, Max
#'     \item Ntot, N, Nmiss, Mean, Median, SD, Min, Max, CLM (different N values are given to identify the number of missing values)
#'     \item N, Mean (SD), Median, Range (Both Mean (SD) and Range are concatenated to generate a dense overview of statistics)
#'   }
#' All statistics are calculated on using na.rm set to TRUE, meaning that NA values are removed before calculating the statistics.
#' A predefined set of statistics is chosen and not the possibility to implement user defined descriptive statistics.
#' The reason for this is that there are many other (more simple) options to do this within R.
#'
#' @return The function returns a dataframe with calculated descriptive statistics
#'
#' @export
#' @examples
#'
#' data(Indometh)
#' means(Indometh,"conc","time",total="time",pack=3)

means <- function(dfrm,variable,by,total=NULL,pack=1,dig=2,alpha=.1){
  # keep applicable variables repeat rows for calculation of totals and rename variables for ddply
  dfrm        <- dfrm[,c(by,variable)]
  if(!is.null(total)){
    tot <- dfrm
    for(i in 1:length(by)) {if((by%in%total)[i]==TRUE) tot[,by[i]] <- "Total"}
    dfrm  <- plyr::rbind.fill(dfrm,tot)
  }
  names(dfrm) <- c(paste0("by",1:length(by)),"variable")

  # define different functions for the different stat packs
  if(pack==1){
    stats <-  function(x){
      data.frame(N        = length(x$variable),
                 Mean     = mean(x$variable,na.rm=TRUE),
                 Median   = stats::median(x$variable,na.rm=TRUE),
                 SD       = stats::sd(x$variable,na.rm=TRUE),
                 Min      = min(x$variable,na.rm=TRUE),
                 Max      = max(x$variable,na.rm=TRUE),
                 stringsAsFactors=FALSE)
    }
  }else if(pack==2){
    stats <-  function(x){
      data.frame(Ntot     = length(x$variable),
                 N        = length(stats::na.omit(x$variable)),
                 Nmiss    = length(x$variable) - length(stats::na.omit(x$variable)),
                 Mean     = mean(x$variable,na.rm=TRUE),
                 Median   = stats::median(x$variable,na.rm=TRUE),
                 SD       = stats::sd(x$variable,na.rm=TRUE),
                 Min      = min(x$variable,na.rm=TRUE),
                 Max      = max(x$variable,na.rm=TRUE),
                 CLM      = paste0(formatC(mean(x$variable,na.rm=TRUE) - stats::qt(1-(alpha/2),df=length(stats::na.omit(x$variable))-1)*stats::sd(x$variable,na.rm=TRUE)/sqrt(length(stats::na.omit(x$variable))),digits=dig,format="f")," - ",
                                   formatC(mean(x$variable,na.rm=TRUE) + stats::qt(1-(alpha/2),df=length(stats::na.omit(x$variable))-1)*stats::sd(x$variable,na.rm=TRUE)/sqrt(length(stats::na.omit(x$variable))),digits=dig,format="f")),
                 stringsAsFactors=FALSE)
    }
  }else if(pack==3){
    stats <-  function(x){
      data.frame(N        = length(x$variable),
                 MeanSD   = paste0(formatC(mean(x$variable,na.rm=TRUE),digits=dig,format="f")," (",
                                   formatC(stats::sd(x$variable,na.rm=TRUE),digits=dig,format="f"),")"),
                 Median   = stats::median(x$variable,na.rm=TRUE),
                 Range    = paste0(formatC(min(x$variable,na.rm=TRUE),digits=dig,format="f")," - ",
                                   formatC(max(x$variable,na.rm=TRUE),digits=dig,format="f")),
                 stringsAsFactors=FALSE)
    }
  }else{
    stop("Provide a valid pack number")
  }

  # Calculate statistics and finalize output
  res                         <- plyr::ddply(dfrm,paste0("by",1:length(by)),stats)
  res                         <- reshape2::melt(res,id=paste0("by",1:length(by)))
  res$value[res$variable%in%c("N","Ntot","Nmiss")] <-
    formatC(as.numeric(res$value[res$variable%in%c("N","Ntot","Nmiss")]),digits=0,format="f")
  res$value[!res$variable%in%c("N","Ntot","Nmiss","MeanSD","Range","CLM")] <-
      formatC(as.numeric(res$value[!res$variable%in%c("N","Ntot","Nmiss","MeanSD","Range","CLM")]),digits=dig,format="f")
  nwlv                        <- levels(res$variable)
  nwlv[which(nwlv=="MeanSD")] <- "Mean (SD)"
  nwlv[which(nwlv=="CLM")]    <- paste0((1-alpha)*100,"% CLM")
  attr(res$variable,"levels") <- nwlv
  names(res) <- c(by,"statistic","value")
  return(res)
}
