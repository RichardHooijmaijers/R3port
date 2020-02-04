#------------------------------------------ freq ------------------------------------------
#' Calculate frequency statistics on data frame
#'
#' This function calculates frequency statistics on a data frame. It is intended to use to
#' calculate the frequency and percentages based on a predefined denominator.
#'
#' @param dfrm data frame to calculate the statistics on
#' @param vars character vector of variable(s) within the data frame to perform the statistics on
#' @param id character vector of variable(s). In case id is set the function will take into account non duplicated values
#'   for id in the calculations.
#' @param denom the denominator for the calculation of percentage. See details for valid options
#' @param dig number of digits to use for output percentage (frequency is displayed as integer)
#' @param total vector of variable(s) within the data frame to calculate totals on
#' @param totaldenom the denominator for the calculation of percentage of the totals. See details for valid arguments
#' @param spacechar the character to use for space for combined frequency and percentage in output (e.g. "~" for tex documents)
#'
#' @details The function calculates frequency statistics of variable(s) within a data frame. Furthermore, the frequencies for a total
#'   can also be generated. To calculate the percentages, the denominator should be supplied (and in case totals has a value also totaldenom).
#'   The denom and totaldenom can be a constant numerical value, a variable within the data frame or a separate data frame. In case the argument
#'   is a data frame, the function will attempt to merge this data frame based on equal variables within dfrm/denom and dfrm/totaldenom.
#'   The possibility for a separate denominator is implemented as in many cases the denominator is not the total number of observations
#'   but is defined elsewhere (e.g. for number of adverse events, the total number of subjects is used as denominator).
#'
#' @return The function returns a dataframe with frequencies and percentages
#' @export
#' @examples
#'
#' data(Indometh)
#' freq(Indometh,vars="time",id="Subject",
#'         denom=nrow(Indometh),total="",totaldenom=nrow(Indometh))
freq <- function(dfrm,vars,id,denom=nrow(dfrm),dig=2,total=NULL,totaldenom=nrow(dfrm),spacechar=NULL){
  if(nrow(dfrm)==0) stop("It is not allowed to provide an empty data.frame")

  # Perform actions to include denom/totaldenom in a correct way
  if(!"dnm"%in%names(dfrm) & !is.data.frame(denom)){
    dfrm <- cbind(dfrm,dnm=denom)
  }else if(!"dnm"%in%names(dfrm) & is.data.frame(denom))  {
    if(length(names(dfrm)[names(dfrm)%in%names(denom)])==0) stop("no equal variables for merging dfrm with denom dataframe")
    dfrm <-  merge(dfrm,denom,vars=names(dfrm)[names(dfrm)%in%names(denom)],all.x=TRUE)
  }
  if(!is.null(total)){
    if(!"tdnm"%in%names(dfrm) & !is.data.frame(totaldenom)) {
      dfrmt <- cbind(dfrm,tdnm=totaldenom)
    }else if(!"tdnm"%in%names(dfrm) & is.data.frame(totaldenom))  {
      if(length(names(dfrm)[names(dfrm)%in%names(totaldenom)])==0) stop("no equal variables for merging dfrm with totaldenom dataframe")
      dfrmt <- merge(dfrm,totaldenom,vars=names(dfrm)[names(dfrm)%in%names(totaldenom)],all.x=TRUE)
    }
  }
  # Keep uniques if specified and calculate frequencies
  if(!missing(id)){
    dfrm <- dfrm[do.call("order", do.call("list",dfrm[,c(id,vars)])),]
    dfrm <- dfrm[!duplicated(do.call("paste",do.call("list",dfrm[,c(id,vars)]) )),,drop=FALSE]
  }

  dfrm     <- dfrm[,c(vars,"dnm")]
  res      <- plyr::ddply(dfrm,c(vars,"dnm"),nrow)
  if(!is.null(total)){
    if(length(total)==1 && total==""){
      rest <- plyr::ddply(dfrmt,c("tdnm"),nrow)
    }else{
      rest <- plyr::ddply(dfrmt,c(total,"tdnm"),nrow)
    }
    for(i in 1:length(vars)) {if((vars%in%total)[i]==FALSE) rest[,vars[i]] <- "Total"}
    rest <- plyr::rename(rest,c("tdnm"="dnm"))
    res  <- plyr::rbind.fill(res,rest)
  }

  # Calculate percentages and finalize results
  res          <- plyr::rename(res,c("V1"="Freq"))
  res$Perc     <- formatC((as.numeric(res$Freq)/res$dnm)*100,digits=dig,format="f")
  wdt          <- ceiling(log10(max(as.numeric(res$Freq)))) + 1
  res$FreqPerc <- paste0(formatC(res$Freq,width=wdt,flag="-"),"(",res$Perc,")")
  if(!is.null(spacechar)) res$FreqPerc <- gsub(" ",spacechar,res$FreqPerc)

  return(res)
}
