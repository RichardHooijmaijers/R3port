#------------------------------------------ ltx_plot ------------------------------------------
#' Prints a R plot to a latex file or console
#'
#' This function makes a latex document including the plots defined
#'
#' @param plot plot object or function call that creates plot to be printed to file
#' @param out filename for the output latex file
#' @param title character string to define the title of the table which will be added to the caption
#' @param titlepr character string to define the prefix of the table title. Can be used to create custom table numbering
#' @param footnote character string with the footnote to be placed in the footer of the page (LaTeX coding can be used for example to create line breaks)
#' @param lwidth character string indicating the width of the plot within latex (e.g. "\\\\linewidth")
#' @param pwidth numeric indicating the width of the plot to be generated in inches or pixels (for respectively the extensions pdf and png)
#' @param pheight numeric indicating the height of the plot to be generated in inches or pixels (for respectively the extensions pdf and png)
#' @param res numeric indicating the resolution of the plot (in case png is used), if set to NULL it will adapt the value according height of the plot
#' @param fontsize character string with the default font or pointsize passed through to png or pdf function
#' @param units character string with the units to use for plot width and height passed through to png function
#' @param hyper logical indicating if a hypertarget should be set used for bookmarks
#' @param outfmt character string indicating the format of the output file (currently "pdf" and "png" are accepted)
#' @param rawout character string with the name of the raw latex file to generate (e.g. only plot code with no preamble and document ending)
#'    In case NULL no raw output will be generated. In order to combine results the filename should end in .rawtex
#' @param linebreak logical indicating if a linebreak (clearpage) should be given after a plot
#' @param ... additional arguments passed through to \code{\link{ltx_doc}}. Most important are template, rendlist, compile and show
#'
#' @return The function returns a latex file (or writes output to console)
#'
#' @export
#' @examples
#'
#' # It is convenient to have an object for the plot argument
#' \dontrun{
#'   data(Theoph)
#'   library(ggplot2)
#'   pl <- qplot(Time, conc, data=Theoph, facets=~Subject,geom="line")
#'   ltx_plot(pl,out=tempfile(fileext=".tex"))
#'
#'   # Base plots work a bit different and can be placed
#'   # in the function directly or wrapped in a function
#'   pl <- function() {
#'     plot(conc~Time,data=Theoph)
#'     title(main="a plot")
#'   }
#'   ltx_plot(pl(),out=tempfile(fileext=".tex"))
#'   # In case of big data it can be more convenient to have a png included
#'   ltx_plot(plot(rnorm(1e6)),out=tempfile(fileext=".tex"),
#'            outfmt="png",pwidth=2000,pheight=1200)
#' }
ltx_plot <- function(plot,out,title="plot",titlepr=NULL,footnote="",lwidth=NULL,pwidth=10,pheight=5.5,res=NULL,hyper=TRUE,outfmt="pdf",
                     fontsize=12,units="px",rawout=paste0(out,".rawtex"),linebreak=TRUE,...){
  if(is.null(out)|out=="") stop("A valid name for the output should be specified")

  # Set logics for option system
  if(!is.null(getOption('pwidth')))   pwidth   <- getOption('pwidth')
  if(!is.null(getOption('pheight')))  pheight  <- getOption('pheight')

  # Create subfolder to place graphs in
  dir.create(paste(dirname(out),"figures",sep="/"),showWarnings = FALSE)

  # Save plot to location.
  prpl <- function(){
    if(outfmt=="pdf"){
      grDevices::pdf(file=paste0(dirname(out),"/figures/",sub("\\.tex$","",basename(out)),"%03d.pdf"),width=pwidth,height=pheight,onefile=FALSE,pointsize=fontsize)
    }else if(outfmt=="png"){
      reso   <- ifelse(is.null(res) & units=="px",as.numeric(pheight)/6,ifelse(is.null(res) & units!="px",200,res))
      grDevices::png(filename=paste0(dirname(out),"/figures/",sub("\\.tex$","",basename(out)),"%03d.png"),width=pwidth,height=pheight,res=reso,pointsize=fontsize,units=units)
    }else{
      stop("provide valid format for output (outfmt should be 'pdf' or 'png')")
    }
    print(plot)
    grDevices::dev.off()
  }
  tplot <- try(prpl())
  if(class(tplot)=="try-error"){
    if(!is.null(grDevices::dev.list())) grDevices::dev.off()
    stop("Cannot create the plot")
  }

  # Create the latex plot code
  # Multiple plots are implemented where the caption and bookmarks are not repeated.
  # However if a titlepr is provided, the caption is repeated with ",cont'd" and not added to lof
  # This strategy was chosen as the default behaviour is to increment number the caption
  numplots <-  list.files(paste0(dirname(out),"/figures/"),pattern=paste0(sub("\\.tex$","",basename(out)),"...\\.",outfmt))
  plt  <- NULL
  for(i in 1:length(numplots)){
    if(!missing(titlepr))  plt <- c(plt,paste0("\\renewcommand{\\figurename}{} \\renewcommand\\thefigure{{",titlepr,"}}"))
    plt <- c(plt,paste0("\\lfoot{\\footnotesize ",footnote,"}"))
    plt  <- c(plt,"\\begin{figure}[H]")
    if(hyper & !missing(titlepr) & i==1) plt <- c(plt,paste0("\\hypertarget{",title,"}{} \\bookmark[dest=",title,",level=0]{",titlepr,": ",title,"}"))
    if(hyper & missing(titlepr) & i==1)  plt <- c(plt,paste0("\\hypertarget{",title,"}{} \\bookmark[dest=",title,",level=0]{",title,"}"))
    if(i==1) {
      plt  <- c(plt,paste0("\\caption{",title,"}"))
    }else{
      if(!missing(titlepr)) plt  <- c(plt,paste0("\\caption[]{",title,", cont'd}"))
    }
    if(is.null(lwidth)){
      plt  <- c(plt,paste0("\\includegraphics{{\"figures/",paste0(sub("\\.tex$","",basename(out)),formatC(i,width=3,flag="0"),"\"}.",outfmt),"}\\\\"))
    }else{
      plt  <- c(plt,paste0("\\includegraphics[width=",lwidth,"]{{\"figures/",paste0(sub("\\.tex$","",basename(out)),formatC(i,width=3,flag="0"),"\"}.",outfmt),"}\\\\"))
    }

    plt  <- c(plt,paste0("\\end{figure}",ifelse(linebreak,"\\clearpage","")))
  }

  # Print the plot
  ltx_doc(text=plt,out=out,...)
  if(!is.null(rawout) & !dir.exists(dirname(rawout))){
    succ <- try(dir.create(dirname(rawout),showWarnings = FALSE))
    if(!succ) stop("Output folder for raw files cannot be created")
  }
  if(!is.null(rawout)) cat(plt,sep="\n",file=rawout)
}
